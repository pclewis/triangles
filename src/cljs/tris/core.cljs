(ns tris.core)

(defn ^:export load-graph [x y z]
  (set! (.-src (.getElementById js/document "graph"))
        (str "/gv.svg?q=" x "," y "," z)))

(defn update-graph []
  (apply load-graph (map #(.-value (.getElementById js/document (name %))) [:x :y :z])))

(.addEventListener
 js/document "DOMContentLoaded"
 (fn []
   (doseq [n [:x :y :z]]
     (.addEventListener (.getElementById js/document (name n)) "change" update-graph))))
