(ns tris.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]
            [clojure.java.shell :as sh]
            [clojure.set]))

;; generic utility functions
(defn spy [x] (println x) x)
(def find-first (comp first filter))

;;   z
;;  / \
;; x---y
(def base-lines
  "The outside lines of the triangle."
  #{#{:x :y}
    #{:y :z}
    #{:z :x}})

(defn lines-to-edges [lines]
  (set (map (partial apply hash-set)
                 (mapcat #(combo/combinations % 2) lines))))

(defn lines-to-nodes [lines]
  (set (mapcat identity lines)))

(defn connected? [edges x y]
  (boolean (edges #{x y})))

(defn in-line? [line nodes]
  (every? (set line) nodes))

(defn in-any-line? [lines & nodes]
  (some #(in-line? % nodes) lines))

(defn triangle? [lines edges x y z]
  (and (connected? edges x y)
       (connected? edges y z)
       (connected? edges z x)
       (not (in-any-line? lines x y z))))

(defn lines-to-tris [lines]
  (let [edges (lines-to-edges lines)
        nodes (lines-to-nodes lines)]
    (filter #(apply triangle? lines edges %) (combo/combinations nodes 3))))

(defn break-line [lines target node]
  (set (map (fn [line] (if (in-line? line target)
                        (conj line node)
                        line)) lines)))

(defn hypotenuse [x]
  (find-first #(not (% x)) base-lines))

(defn make-node [origin]
  (-> origin name gensym keyword))

(defn lines-from
  "Return lines originating from pts that do not intersect the opposite point(s)"
  [pts lines]
  (let [hs (apply clojure.set/intersection (map hypotenuse pts))]
    (filter #(and (some pts %) (not-any? hs %)) lines)))

(defn add-line [lines start]
  (apply conj
        (reduce (fn [[new-lines new-line] target]
                  (let [new-node (make-node start)]
                    [(break-line new-lines target new-node)
                     (conj new-line new-node)]))
                [lines #{start}]
                (lines-from (hypotenuse start) lines))))

(defn add-lines [lines & starts]
  (reduce add-line lines starts))

(defn T [x] (-> (inc x)
                (* x)
                (/ 2)))

(defn T+ [& xs] (T (apply + xs)))

(defn n-edges [x y z]
  (+ (* x (T+ y z 1))
     (T+ x 1)
     (* y (T+ x z 1))
     (T+ y 1)
     (* z (T+ x y 1))
     (T+ z 1)))

(defn n-nodes [x y z]
  (+ (* x y)
     (* x z)
     (* y z)
     x y z 3))

(defn n-tris [x y z]
  (/
   (+ (* x x) (* y y) (* z z)
      (* x x y) (* x x z)
      (* y y x) (* y y z)
      (* z z x) (* z z y)
      (* 2 x y z)
      (* 4 x y) (* 4 x z) (* 4 y z)
      (* 3 x) (* 3 y) (* 3 z)
      2)
   2)
  )

(defn make-graph [x y z]
  (apply add-lines base-lines (concat (repeat x :x) (repeat y :y) (repeat z :z))))

(defn graph-info [x y z]
  (let [g (make-graph x y z)
        edges (lines-to-edges g)
        nodes (lines-to-nodes g)
        tris (lines-to-tris g)]
    (println x "x" y "x" z "graph")
    (println "Edges:" (count edges) "(formula:" (n-edges x y z) ")")
    (println "Nodes:" (count nodes) "(formula:" (n-nodes x y z) ")")
    (println "Tris:" (count tris) "(formula:" (n-tris x y z) ")")
    (println "Edges - Nodes - Tris:" (- (count edges) (count nodes) (count tris)))
;    (println "Lines:" g)
;    (println "Uniq tris:" (count (distinct tris)))
;    (println "Tris:" tris)
    ))


(defn degraph [lines]
  (map #(-> #{%} (lines-from lines) count) '(:x :y :z)))

(defn line-angles [max n]
  (drop 1 (drop-last 1 (range 0 max (/ max (+ 2 n))))))

(defn line-angles [max n]
  (drop 1  (range 0 max (/ max (inc n)))))

;; {:root :x
;;  :start [0 0]
;;  :angle 45
;;  :offset 120
;;  :nodes #{...}
(defn lines-to-info [lines]
  (mapcat (fn [sym cnt start offset]
            (map (fn [line angle] {:root sym :start start :angle angle :nodes line :offset offset})
                 (lines-from #{sym} lines) (line-angles 60 cnt)))
       [:x :y :z]
       (degraph lines)
       [[0 0] [1 0] [0.5 0.866]]
       [0 120 240]))

;;      C
;;    b/ \a
;;    A---B
;;      c
(defn solve-triangle-aas [triangle]
  (let [triangle (reduce (fn [t [a & os]]
                           (if (t a) t
                               (assoc t a (apply - 180 (map t os))))) triangle (combo/permutations [:A :B :C]))
        angles-sides (map (partial map triangle) [[:A :a] [:B :b] [:C :c]])
        ratio (some (fn [[a s]] (when (and a s) (/ s (Math/sin (Math/toRadians a))))) angles-sides)
        triangle (reduce (fn [t [k [a s]]] (if-not (nil? s) t
                                                  (assoc t k (* (Math/sin (Math/toRadians a)) ratio))))
                         triangle
                         (zipmap [:a :b :c] angles-sides))
        ]
    triangle))

(defn project-position [[x y] angle dist]
  (let [r (Math/toRadians angle)]
    [(+ x (* dist (Math/cos r)))
     (+ y (* dist (Math/sin r)))]))

(defn triangle-to-coordinates
  "Given the position of any node in a triangle, calculate the other positions given
   side c at specified rotation."
  [triangle rotation node pos]
  (assoc
      (case node
        :a {:b (project-position pos (+ rotation 0) (:c triangle))
            :c (project-position pos (+ rotation (:A triangle)) (:b triangle))}
        :b {:a (project-position pos (+ rotation 180) (:c triangle))
            :c (project-position pos (- (+ rotation 180) (:B triangle)) (:a triangle))}
        :c (let [start (+ rotation 180 (:A triangle))]
             {:a (project-position pos start (:b triangle))
              :b (project-position pos (+ start (:C triangle)) (:a triangle))}))
    node pos))

;; angle is along the base lines :x -> :y, :y -> :z, :z -> :x
;; Therefore, the angle of the second line is against its other base line, and must be inverted.
;; If there is no second line, it is the next base line, so the angle is 0.
(defn node-position [info node]
  (let [lines (sort-by :root (filter #(some #{node} (:nodes %)) info))
        [l1 l2] (if (= 3 (count lines))
                  [(last lines) (first lines)] ;; x and z
                  (if  (= #{:x :z} (set (map :root lines)))
                    (reverse lines)
                    lines))]
    (:c (triangle-to-coordinates (solve-triangle-aas {:A (:angle l1) :B (- 60 (or (:angle l2) 0)) :c 1})
                                 (:offset l1) :a (:start l1)))))

(defn node-positions [lines]
  (let [info (lines-to-info lines)]
    (apply hash-map
           (mapcat (fn [node]
                  [node
                   (case node
                     :x [0 0]
                     :y [1 0]
                     :z [0.5 0.866]
                     (node-position info node))])
                (lines-to-nodes lines)))))

(defn merge-points [lines p & dups]
  (let [replacements (apply hash-map (interleave dups (repeat p)))]
    (set (map #(set (replace replacements %)) lines))))

(defn merge-overlapping-points [lines]
  (let [g (group-by (fn [[_ pos]]
                      (map #(Math/round (* 100.0 %)) pos))
                    (node-positions lines))]
    (reduce #(apply merge-points %1 (map first %2))
            lines
            (filter #(>= (count %) 3) (vals g)))))
(replace {:x :q} #{:x :y :z})
(interleave [:a] [:b :c])
(merge-points #{#{:a :x :y} #{:b :x :y} #{:q :d :r}} :a :b :c :d)
(make-graph 1 1 1)
(make-graph 2 2 4)
(merge-overlapping-points (make-graph 2 2 4))
(node-positions (make-graph 1 1 1))
(group-by :root (lines-to-info (make-graph 1 1 1)))
(mapcat identity [nil '(:a :b) '(:q)])
(Math/round 4.0)

(defn gv [g]
  (let [g (merge-overlapping-points g)
        ps (node-positions g)
        [x y z] (degraph g)]
    (str "graph T" x "x" y "x" z " {"
         "graph [scale=72];"
         "node [shape=point width=0.01 height=0.01 fixedsize=true];"
         "edge [penwidth=0.1];"
         (apply str (map #(str (name (first %)) " -- " (name (second %)) ";") (lines-to-edges g)))
         (apply str (for [n (lines-to-nodes g)
                          :let [p (ps n)]
                          :when p]
                      (str (name n) " [ pos = \"" (* 5 (first p)) "," (* 5 (second p)) "!\" ];") ))
         (apply str (for [[t tn] (map list (sort-by #(vec (reverse (sort (map (comp vec reverse ps) %)))) (lines-to-tris g)) (range))
                          :let [col (mod tn 5)
                                row (int (/ tn 5))]]
                      (str
                       (apply str (map #(str "t" tn (name (first %)) " -- " "t" tn (name (second %)) ";")
                                       (combo/combinations t 2)))
                       (apply str (for [n t
                                        :let [[x y] (ps n)
                                              p [(+ x col)
                                                 (- y (inc row))]]]
                                    (str "t" tn (name n) " [ pos = \"" (first p) "," (second p) "!\" ]; ")))
                       "label" tn " [ pos=\"" col "," (- 0 row 0.2) "!\" shape=circle width=0.2 height=0.2 fixedsize=true fontsize=6 label=\"" (inc tn) "\"" "];"
                       )))
         "}")))

(comment
  (double 22/6)

  (spit "/home/pcl/projects/tris/g111-bad.gv" (gv (make-graph 1 1 1)))
  (time
   (do
     (spit "/home/pcl/projects/tris/gTTT.gv" (gv (make-graph 10 10 10)))
     ""))

  (sh/sh "lneato" "-" :in (gv (make-graph 1 0 1)))
  (sh/sh "display" "svg:-" :in (:out (sh/sh "neato" "-Tsvg" :in (gv (make-graph 2 2 4)))))

  (println (lines-to-info (make-graph 2 2 4)))

  (sh/sh "cat" :in "hi")

  (gv (make-graph 1 1 0))

  (lines-to-info (make-graph 0 0 1))
  (lines-to-tris (make-graph 1 1 0))
  (node-positions (make-graph 1 1 0))
  ({:a 1} :a)
  (read "2"))

(defn -main
  [& args]
  (if (= 3 (count args))
    (println (gv (apply make-graph (map #(Integer. %) args))))
    (println "Usage: tris x y z")))

;; if X=-Y=Z points will overlap
