(ns tris.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]))

(defn spy [x]
  (println x)
  x)

(defn connected? [graph x y]
  (boolean (some #{#{x y}} (lines-to-edges graph))))

(defn in-line? [lines & nodes]
  (some #(every? (apply hash-set %) (apply hash-set nodes)) lines))

(defn triangle? [graph x y z]
  (and (connected? graph x y)
       (connected? graph y z)
       (connected? graph z x)
       (not (in-line? graph x y z))))

(def lines-to-edges
  (memoize (fn [lines]
             (apply hash-set (map (partial apply hash-set) (mapcat #(combo/combinations % 2) lines)))))

(def base-lines #{[:x :y] [:y :z] [:z :x]})

(defn break-line [lines [lx ly] node]
  (map (fn [line]
         (let [line (some #(when (and (some #{lx} %)
                                      (some #{ly} %)) %)
                          lines)]
           (conj (disj lines line)
                 (into [(first line) node] (rest line)))))))


(defn cast-ray [lines origin [hx hy :as hyp]]
  (let [end-node (keyword (gensym (name origin)))
        ng (break-line lines hyp end-node)
        line [origin]
        [ng line] (reduce (fn [[new-lines line] target-line]
                            (let [new-node (keyword (gensym (name origin)))]
                              [(break-line new-lines [(first target-line) (last target-line)] new-node)
                               (conj line new-node)]))
                          [ng line] (spy (filter #(and (some #{hx hy} #{(first %)})
                                                       (not (some #{:x :y :z} #{(last %)}))) lines)))]
    (conj ng (conj line end-node))))


(break-line base-lines [:y :z] :nn)

(def g12
  (-> base-lines
      (cast-ray :x [:y :z])
      (cast-ray :y [:x :z])
      (cast-ray :y [:x :z])))




(triangle? g12 :y :y9313 :y9312)
(println g12)

(count
 (lines-to-edges g12))

(defn T [x] (-> (+ x 1)
                (* x)
                (/ 2)))
(defn T+ [& xs] (T (apply + xs)))

(defn edges [x y]
  (+ (* x (T+ y 1))
     (T+ x 1)
     (* y (T+ x 1))
     (T+ y 1)
     1))

(T+ 1 1)
(edges 2 1)

(defn cast-xs [g] (iterate #(cast-ray % :x [:y :z]) g))
(defn cast-ys [g] (iterate #(cast-ray % :y [:x :z]) g))

(defn make-graph [x y]
  (-> base-lines
      cast-ys
      (nth y)
      cast-xs
      (nth x)))

(defn lines-to-nodes [lines]
  (apply hash-set (mapcat identity lines)))

(defn lines-to-tris [lines]
  (filter #(apply triangle? lines %) (combo/combinations (lines-to-nodes lines) 3)))

(lines-to-tris g12)

(make-graph 1 1)

(defn graph-info [x y]
  (let [g (make-graph x y)
        edges (lines-to-edges g)
        nodes (lines-to-nodes g)
        tris (lines-to-tris g)]
    (println x "x" y "graph")
    (println "Edges:" (count edges))
    (println "Nodes:" (count nodes))
    (println "Tris:" (count tris))
    (println "Edges - Nodes - Tris:" (- (count edges) (count nodes) (count tris)))))

(graph-info 4 4)
(graph-info 5 5)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))
