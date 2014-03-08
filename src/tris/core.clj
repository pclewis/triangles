(ns tris.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]
            [clojure.java.shell :as sh]))

(defn spy [x] (println x) x)

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

(def base-lines #{#{:x :y}
                  #{:y :z}
                  #{:z :x}})

(defn break-line [lines target node]
  (set (map (fn [line] (if (in-line? line target)
                        (conj line node)
                        line)) lines)))

(def find-first (comp first filter))

(defn hypotenuse [x]
  (find-first #(not (% x)) base-lines))

(defn make-node [origin]
  (-> origin name gensym keyword))

(defn lines-from
  "Return lines originating from pts that do not intersect the opposite point(s)"
  [pts lines]
  (let [hs (apply clojure.set/intersection (map hypotenuse pts))]
    (filter #(and (some pts %) (not (some hs %))) lines)))

(defn add-line [lines start]
  (apply conj
        (reduce (fn [[new-lines new-line] target]
                  (let [new-node (make-node start)]
                    [(break-line new-lines target new-node)
                     (conj new-line new-node)]))
                [lines #{start}]
                (lines-from (hypotenuse start) lines))))

(add-line base-lines :x)

(defn add-lines [lines & starts]
  (reduce add-line lines starts))

(defn T [x] (-> (+ x 1)
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

;;aaa
;; {:start [0 0]
;;  :angle 45
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

;;
(defn triangle-to-coordinates [triangle rotation node pos]
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

(defn node-position [info node]
  (let [lines (sort-by :root (filter #(some #{node} (:nodes %)) info))
        xz (= #{:x :z} (set (map :root lines)))
        [l1 l2] (if xz (reverse lines) lines)
        l1a (:angle l1)
        l2a (- 60 (or (:angle l2) 0)) ; if only one line, then it's along an outer line
       ; l1a (if xz (- 60 l1a) l1a)
       ; l2a (if xz l2a (- 60 l2a))
        ]
    (spy lines)
    (:c (triangle-to-coordinates (spy (solve-triangle-aas {:A l1a :B l2a :c 1}))
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

(defn gv [g]
  (let [ps (node-positions g)]
    (str "graph G {"
         "graph [scale=72];"
         "node [label=\"\", shape=point];"
         (apply str (map #(str (name (first %)) " -- " (name (second %)) ";") (lines-to-edges g)))
         (apply str (for [n (lines-to-nodes g)
                          :let [p (ps n)]
                          :when p]
                      (str (name n) " [ pos = \"" (first p) "," (second p) "!\" ];") ))
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

  (spit "/home/pcl/projects/tris/g432.gv" (gv (make-graph 4 3 2)))

  (sh/sh "lneato" "-" :in (gv (make-graph 1 0 1)))
  (sh/sh "display" "svg:-" :in (:out (sh/sh "neato" "-Tsvg" :in (gv (make-graph 4 3 2)))))

  (sh/sh "cat" :in "hi")

  (gv (make-graph 1 1 0))

  (lines-to-info (make-graph 0 0 1))
  (lines-to-tris (make-graph 1 1 0))
  (node-positions (make-graph 1 1 0))
  ({:a 1} :a))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))


;(x*((y+z+2) choose 2) + ((x+2) choose 2) + (y*((x+z+2) choose 2) + ((y+2) choose 2) + (z*((y+x+2) choose 2)+ ((z+2) choose 2))-(xy+xz+yz+x+y+z+3)
