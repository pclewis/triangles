(defproject tris "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "CC0"
            :url "http://creativecommons.org/publicdomain/zero/1.0/"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2173"]
                 [hiccup "1.0.5"]
                 [hiccups "0.3.0"]
                 [garden "1.1.5"]
                 [ring/ring-core "1.2.1"]
                 [ring/ring-jetty-adapter "1.2.1"]
                 [org.clojure/data.xml "0.0.7"]]
  :plugins [[lein-cljsbuild "1.0.2"]
            [com.keminglabs/cljx "0.3.2"]]

  :main ^:skip-aot tris.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}

  :hooks [cljx.hooks]

  :source-paths ["src/clj"]

  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :clj}

                  {:source-paths ["src/cljx"]
                   :output-path "target/generated/cljs"
                   :rules :cljs}]}

  :cljsbuild {:builds [{:source-paths ["target/generated/cljs" "src/cljs"]
                        :compiler {:output-dir "resources/public/js"
                                   :output-to "resources/public/js/main.js"
                                   :source-map "resources/public/js/main.js.map"
                                   :optimizations :whitespace
                                   :pretty-print true}}]})
