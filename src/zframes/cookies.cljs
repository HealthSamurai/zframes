(ns zframes.cookies
  (:refer-clojure :exclude [get set!])
  (:require [goog.net.cookies :as gcookies]
            [re-frame.core :as rf]
            [cljs.reader :as reader]))

(defn get-cookie
  "Returns the cookie after parsing it with cljs.reader/read-string."
  [k]
  (try
    (let [v (.get goog.net.cookies (name k))
          v (if v
              (js/window.atob v)
              "nil")]
      (reader/read-string v))
    (catch :default e
      (js/console.error (str "Get cookie (" k ") error: " e))
      nil)))

;; For some reason on mobile safari cookie is truncated
;; before comma. But not in all situations. I decided
;; to use base64 to dodge many errors with signs.
(defn set-cookie
  "Stores the cookie value using pr-str."
  [k v]
  (.set goog.net.cookies (name k) (js/window.btoa (pr-str v))))

(defn remove! [k] (.remove goog.net.cookies (name k)))

(defn watch-expires [k]
  ;; todo
  )

(rf/reg-cofx
 ::get
 (fn [coeffects key]
   (assoc-in coeffects [:cookie key] (get-cookie key))))

(rf/reg-fx
 ::set
 (fn [{k :key v :value :as opts}]
   (set-cookie k v)))

(rf/reg-fx
 ::remove
 (fn [{k :key}]
   (.remove goog.net.cookies (name k))))
