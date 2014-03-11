(ns tris.core
  (:gen-class)
  (:require [clojure.java.shell :as sh]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.resource :use wrap-resource]
            [ring.middleware.file-info :use wrap-file-info]
            [ring.middleware.params :use wrap-params]
            [ring.middleware.keyword-params :use wrap-keyword-params]
            [hiccup.core :use html]
            [garden.core :use css]
            [clojure.data.xml :as xml]
            [tris.tris :as t]))

(def index
  (html
   [:html
    [:head
     [:script {:src "/js/main.js"}]]
    [:body
     [:h1 "Hello world"]
     (for [n [:x :y :z :o]]
       [:input {:type "text" :size 2 :maxlength 2 :id (name n) :value (if (= :o n) "0" "1")}])
     [:input {:id "server" :type "checkbox" :checked "checked"}]
     "use server"
     [:br]
     [:img#graph {:src "/gv.svg?q=1,1,1&o=0"}]
     ]]))

(defn handler [request]
  (println request)
  (case (:uri request)
    "/gv.svg" {:status 200
               :headers {"Content-Type" "image/svg+xml"}
               :body (xml/emit-str
                      (xml/sexp-as-element
                       (t/svg (apply t/make-graph (map #(Integer. %) (clojure.string/split (:q (:params request)) #",")))
                              (Double. (or (:o (:params request)) "0")))))
               ;(:out (sh/sh "neato" "-Tsvg" :in (gv (apply make-graph (map #(Integer. %) (clojure.string/split (:q (:params request)) #",")) ))))
               }
    "/" {:status 200
         :headers {"Content-Type" "text/html"}
         :body index}))

(def app
  (-> handler
      (wrap-resource "public")
      (wrap-file-info)
      (wrap-keyword-params)
      (wrap-params)))

(defn -main
  [& args]
  (case (count args)
    0 (jetty/run-jetty app {:port 3000})
    1 (jetty/run-jetty app {:port (Integer. (first args))})
    3 (println (t/gv (apply t/make-graph (map #(Integer. %) args))))
    4 (println (t/svg (apply t/make-graph (map #(Integer. %) (take 3 args)))
                    (Double. (last args))))
    (println "Usage: tris x y z")))


;; if X=-Y=Z points will overlap
