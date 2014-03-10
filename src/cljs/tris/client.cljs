(ns tris.client
  (:require-macros [hiccups.core :as hiccups])
  (:require [tris.tris :as tris]
            [hiccups.runtime]))

(defn- elem [id]
  (.getElementById js/document (name id)))

(defn ^:export load-graph [server x y z o]
  (set! (.-src (elem "graph"))
        (if server
          (str "/gv.svg?q=" x "," y "," z "&o=" o)
          (str "data:image/svg+xml;base64,"
               (js/btoa
                (hiccups/html (tris.tris.svg (apply tris.tris.make_graph
                                                    (map js/parseInt [x y z]))
                                             (js/parseInt o))))))))

(defn- update-graph []
  (apply load-graph (.-checked (elem "server")) (map #(.-value (elem %)) [:x :y :z :o])))

(.addEventListener
 js/document "DOMContentLoaded"
 (fn []
   (doseq [n [:x :y :z :o :server]]
     (.addEventListener (elem n) "change" update-graph))))
