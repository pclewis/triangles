(defproject tris "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [org.clojure/clojurescript "0.0-2173"]
                 [hiccup "1.0.5"]
                 [garden "1.1.5"]
                 [ring/ring-core "1.2.1"]
                 [ring/ring-jetty-adapter "1.2.1"]
                 [org.clojure/data.xml "0.0.7"]]
  :plugins [[lein-cljsbuild "1.0.2"]]

  :main ^:skip-aot tris.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}

  :cljsbuild {:builds [{:source-paths ["src/cljs"]
                        :compiler {:output-to "resources/public/js/main.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}]})
