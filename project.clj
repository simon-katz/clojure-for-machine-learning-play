(defproject nomis-code-playing "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[clatrix "0.5.0"]
                 [net.mikera/core.matrix "0.61.0"]
                 [org.clojure/clojure "1.8.0"]
                 [slingshot "0.12.2"]]
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[midje "1.8.3"
                                   :exclusions [org.clojure/clojure]]
                                  [org.clojure/tools.namespace "0.2.11"]]
                   :plugins [[lein-midje "3.2.1"]]}
             :uberjar {:aot :all}}
  :repl-options {:init-ns user})
