(ns tris.tris
  (:require [clojure.set]))

(def find-first (comp first filter))

(defn combinations [coll n]
  (if (= n 1)
    (map vector coll)
    (lazy-cat (map #(cons (first coll) %) (combinations (drop 1 coll) (dec n)))
              (when (< n (count coll))
                (combinations (drop 1 coll) n)))))

(defn permutations [coll]
  (if (= 1 (count coll))
    [coll]
    (mapcat (fn [h] (map #(cons h %)
                        (permutations (disj coll h))))
            coll)))

(defn deg-to-rad [d]
  #+clj (Math/toRadians d)
  #+cljs (* d (/ Math.PI 180)))

;;   z
;;  / \
;; x---y
(def base-lines
  "The outside lines of the triangle."
  #{#{:x :y}
    #{:y :z}
    #{:z :x}})

(defn lines-to-edges [lines]
  (set (map set (mapcat #(combinations % 2) lines))))

(defn lines-to-nodes [lines]
  (set (mapcat identity lines)))

(defn connected? [edges x y]
  (boolean (edges #{x y})))

(defn in-line? [line nodes]
  (every? line nodes))

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
    (filter #(apply triangle? lines edges %) (combinations nodes 3))))

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

(defn degraph [lines]
  (map #(-> #{%} (lines-from lines) count) '(:x :y :z)))

(defn line-angles [max n o]
  (map #(+ o %) (drop 1 (range 0 max (/ max (inc n))))))

;; {:root :x
;;  :start [0 0]
;;  :angle 45
;;  :offset 120
;;  :nodes #{...}
(defn lines-to-info [lines o]
  (mapcat (fn [sym cnt start offset]
            (map (fn [line angle] {:root sym :start start :angle angle :nodes line :offset offset})
                 (lines-from #{sym} lines) (line-angles 60 cnt o)))
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
                               (assoc t a (apply - 180 (map t os))))) triangle (permutations #{:A :B :C}))
        angles-sides (map (partial map triangle) [[:A :a] [:B :b] [:C :c]])
        ratio (some (fn [[a s]] (when (and a s) (/ s (Math/sin (deg-to-rad a))))) angles-sides)
        triangle (reduce (fn [t [k [a s]]] (if-not (nil? s) t
                                                  (assoc t k (* (Math/sin (deg-to-rad a)) ratio))))
                         triangle
                         (zipmap [:a :b :c] angles-sides))
        ]
    triangle))

(defn project-position [[x y] angle dist]
  (let [r (deg-to-rad angle)]
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
        [l1 l2] (if (= #{:x :z} (set (map :root lines)))
                  (reverse lines)
                  lines)]
    (:c (triangle-to-coordinates (solve-triangle-aas {:A (:angle l1) :B (- 60 (or (:angle l2) 0)) :c 1})
                                 (:offset l1) :a (:start l1)))))

(defn node-positions [lines o]
  (let [info (lines-to-info lines o)]
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

(defn overlapping-points [positions]
  (filter #(>= (count %) 3)
          (map (partial map first)
               (vals (group-by (fn [[_ pos]]
                                 (map #(Math/round (* 100.0 %)) pos))
                               positions)))) )

(defn merge-overlapping-points [lines positions]
  (reduce #(apply merge-points %1 %2)
          lines
          (overlapping-points positions)))

(defn graph-info [g ps]
  (let [edges (lines-to-edges g)
        nodes (lines-to-nodes g)
        tris (lines-to-tris g)
        [x y z] (degraph g)]
    (str x "x" y "x" z " graph" "\n"
         "Overlapping nodes: " (count (overlapping-points ps)) "\n"
         "Edges: " (count edges) " (formula: " (n-edges x y z) ")" "\n"
         "Nodes: " (count nodes) " (formula: " (n-nodes x y z) ")" "\n"
         "Tris: " (count tris) " (formula: " (n-tris x y z) ")" "\n")))

(defn gv [g]
  (let [ps (node-positions g 0)
        [x y z] (degraph g)
        g (merge-overlapping-points g ps)]
    (str "graph T" x "x" y "x" z " {"
         "graph [scale=72];"
         "node [shape=point fontsize=4 width=0.01 height=0.01 fixedsize=true];"
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
                       (apply str (for [n (lines-to-nodes g)
                                        :let [p (ps n)]
                                        :when p]
                                    (str "t" tn (name n) " [ pos = \"" (+ (first p) col) "," (- (second p) (inc row)) "!\" ];") ))
                       (apply str (for [e (lines-to-edges g)]
                                    (str "t" tn (name (first e)) " -- " "t" tn (name (second e))
                                         (if (every? (set t) e) "[ penwidth=1 color=\"#0000FF\" ]"
                                             "[ style=dashed color=\"#0000007F\" ]")
                                         ";")))
                       "label" tn " [ pos=\"" col "," (- 0 row 0.2) "!\" shape=circle width=0.2 height=0.2 fixedsize=true fontsize=6 label=\"" (inc tn) "\"" "];")
                     ))
         "info [fixedsize=false fontsize=8 shape=circle pos=\"1,4!\" label=\"" (clojure.string/replace (graph-info g ps) "\n" "\\n") "\"];"
         "}")))

(defn invert-y [m]
  (apply hash-map (mapcat #(vector (key %) [(first (val %)) (- 1 (second (val %)))]) m)))

(defn lines-to-draw [lines]
  (let [important-nodes (apply clojure.set/union (filter #(some (partial in-line? %) (combinations [:x :y :z] 2)) lines))]
    (map #(let [rs (filter #{:x :y :z} %)]
            (if (= (count rs) 2) rs
                (filter important-nodes %))) lines)))

(defn sort-tris [tris ps]
  (sort-by (fn [nodes]
             (let [pts (map ps nodes)]
               [(Math/round (* 100.0 (- 1 (apply min (map second pts)))))     ; top-most point
                (vec (sort (map first pts)))]))  ; left-to-right
           tris))

(defn svg [lines o]
  (let [ps (invert-y (node-positions lines o))
        [x y z] (degraph lines)
        g (merge-overlapping-points lines ps)
        edges (lines-to-draw g)
        nodes (lines-to-nodes g)
        tris (sort-tris (lines-to-tris g) ps)
        rows (+ 6 (int (/ (count tris) 5)))]
    [:svg {:width 800 :height (* rows 150) :viewBox (str "0 0 5 " rows ".5") :xmlns "http://www.w3.org/2000/svg" :xmlns:xlink "http://www.w3.org/1999/xlink" }
     [:defs [:g {:id "tt" :stroke "black" :stroke-dasharray "0.02,0.02" :stroke-opacity "0.2" :fill-opacity "0.2" :stroke-width 0.007}
             (for [n nodes
                   :let [[x y] (ps n)]]
               [:circle {:cx x :cy y :r 0.014}])
             (for [e edges
                   :let [[[x1 y1] [x2 y2]] (map ps e)]]
               [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2}])]]
     [:g {:stroke "black" :stroke-width "0.01"}
      [:text {:x 0.1 :y 0.1 :font-size 0.1 :stroke "black" :stroke-opacity 0.1}
       (for [line (clojure.string/split (graph-info g ps) #"\n")]
         [:tspan {:dy 0.11 :x 0} line])]
      (for [n nodes
            :let [[x y] (ps n)]]
        [:circle {:cx (* x 5) :cy (* y 5) :r 0.02}])
      (for [e edges
            :let [[[x1 y1] [x2 y2]] (map ps e)]]
        [:line {:x1 (* x1 5) :y1 (* y1 5) :x2 (* x2 5) :y2 (* y2 5)}])]
     (for [[t tn] (map vector tris (range))
           :let [row (int (/ tn 5))
                 col (mod tn 5)]]
       [:g {:stroke "blue" :stroke-width 0.01 :stroke-opacity 1}
        [:text {:x col :y (+ 5.2 row) :font-size 0.1 :stroke "black" :stroke-opacity 0.1} (inc tn)]
        [:use {:xlink:href "#tt" :x col :y (+ 5 row)}]
        (for [[[x1 y1] [x2 y2]] (combinations (map ps t) 2)]
          [:line {:x1 (+ x1 col) :y1 (+ y1 5 row) :x2 (+ x2 col) :y2 (+ y2 5 row)}])] )
     ]))

(comment
  (lines-to-edges (make-graph 0 0 0))

  )
