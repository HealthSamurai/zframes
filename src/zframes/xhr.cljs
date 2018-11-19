(ns zframes.xhr
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [clojure.string :as str]
            [re-frame.core :as rf]))

(defn sub-query-by-spaces
  [k s] (->> (str/split s #"\s+")
             (mapv (fn [v] (str (name k) "=" v)))
             (str/join "&")))

(defn to-query [params]
  (->> params
       (mapcat (fn [[k v]]
                 (cond
                   (vector? v) (mapv (fn [vv] (str (name k) "=" vv)) v)
                   (set? v) [(str (name k) "=" (str/join "," v))]
                   :else [(str (name k) "=" v) #_(sub-query-by-spaces k v)])))
       (str/join "&")))

(defn base-url [db url]
  (str (get-in db [:config :base-url]) url))

(rf/reg-sub
 :xhr/url
 (fn [db [_ url]] (base-url db url)))

(defn make-form-data [files]
  (let [form-data (js/FormData.)]
    (doall
     (for [[i file] (map-indexed vector files)]
       (.append form-data (str "file" i) file (str "file" i))))
    form-data))

(defn *json-fetch [{:keys [uri token headers is-fetching-path params success error]
                   :as opts}]
  (let [headers (cond-> {"accept" "application/json"
                         "authorization" (str "Bearer " token)}
                  (nil? (:files opts)) (assoc "Content-Type" "application/json")
                  true (merge (or headers {})))
        fetch-opts (-> (merge {:method "get" :mode "cors"} opts)
                       (dissoc :uri :headers :success :error :params :files)
                       (assoc :headers headers))
        fetch-opts (cond-> fetch-opts
                     (:body opts) (assoc :body (if (string? (:body opts)) (:body opts) (.stringify js/JSON (clj->js (:body opts)))))
                     (:files opts) (assoc :body (make-form-data (:files opts))))
        ;; FIXME: do not do this; do not go to db from subsciprion
        url (if (str/starts-with? uri "http")
              uri @(rf/subscribe [:xhr/url uri]))]

    (when is-fetching-path (rf/dispatch [::fetch-start is-fetching-path]))
    (->
     (js/fetch (str url (when params (str "?" (to-query params)))) (clj->js fetch-opts))
     (.then
      (fn [resp]
        (when is-fetching-path (rf/dispatch [::fetch-end is-fetching-path]))
        (.then (.json resp)
               ;; There is a json
               (fn [doc]
                 (if (< (.-status resp) 299)
                   (rf/dispatch [(:event success)
                                 (merge success
                                        {:request opts
                                         :response resp
                                         :original-data (.stringify js/JSON doc)
                                         :data (js->clj doc :keywordize-keys true)})])
                   (rf/dispatch [(:event error)
                                 (merge error
                                        {:request opts
                                         :response resp
                                         :data (js->clj doc :keywordize-keys true)})])))
               ;; No json
               (fn [doc]
                 (println "Error:" doc)
                 (rf/dispatch
                  [(:event success)
                   (merge success
                          {:request opts
                           :response resp
                           :data doc})]))
               )))
     (.catch (fn [err]
               (.error js/console err)
               (rf/dispatch [(:event error)
                             (merge error
                                    {:request opts
                                     :error err})]))))))

(defn json-fetch [opts]
  (if (vector? opts)
    (doseq [o opts] (*json-fetch o))
    (*json-fetch opts)))

(rf/reg-fx :json/fetch json-fetch)
(rf/reg-fx ::json-fetch json-fetch)

(rf/reg-event-db
 ::fetch-start
 (fn [db [_ path]]
   (assoc db path true)))

(rf/reg-event-db
 ::fetch-end
 (fn [db [_ path]]
   (assoc db path false)))

(defn xhr-fetch [{:keys [uri token format headers is-fetching-path params success error] :as opts}]
  (let [fmt (or (get {"json" "application/json" "yaml" "text/yaml"} format) "text/yaml")
        headers (cond-> {"accept" fmt
                         "content-type" fmt
                         "authorization" (str "Bearer " token)}
                  (nil? (:files opts)) (assoc "Content-Type" "application/json")
                  true (merge (or headers {})))
        fetch-opts (-> (merge {:method "get" :mode "cors"} opts)
                       (dissoc :uri :headers :success :error :params :files)
                       (assoc :headers headers))
        fetch-opts (cond-> fetch-opts
                     (:body opts) (assoc :body (if (string? (:body opts))
                                                 (:body opts)
                                                 (.stringify js/JSON (clj->js (:body opts)))))
                     (:files opts) (assoc :body (make-form-data (:files opts))))
        ;; FIXME: do not do this; do not go to db from subsciprion
        url (if (str/starts-with? uri "http")
              uri @(rf/subscribe [:xhr/url uri]))]

    (when is-fetching-path (rf/dispatch [::fetch-start is-fetching-path]))
    (->
     (js/fetch (str url (when params (str "?" (to-query params)))) (clj->js fetch-opts))
     (.then
      (fn [resp]
        (when is-fetching-path (rf/dispatch [::fetch-end is-fetching-path]))
        (.then (.text resp)
               (fn [doc]
                 (if (< (.-status resp) 299)
                   (rf/dispatch [(:event success)
                                 (merge success
                                        {:request opts
                                         :response resp
                                         :data doc})])
                   (rf/dispatch [(:event error)
                                 (merge error
                                        {:request opts
                                         :response resp
                                         :data doc})])))
               ;; No json
               (fn [doc]
                 (println "Error:" doc)
                 (rf/dispatch
                  [(:event success)
                   (merge success
                          {:request opts
                           :response resp
                           :data doc})]))
               )))
     (.catch (fn [err]
               (.error js/console err)
               (rf/dispatch [(:event error)
                             (merge error
                                    {:request opts
                                     :error err})]))))))

(rf/reg-fx :xhr/fetch xhr-fetch)
