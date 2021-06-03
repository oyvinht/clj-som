(defproject som "0.1.0-SNAPSHOT"
  :description "Self-Organizing Maps (Kohonen networks) for Clojure."
  :url "https://github.com/oyvinht/clj-som"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[com.clojure-goes-fast/clj-async-profiler "0.5.0"]
                 [criterium "0.4.6"]
                 [org.clojure/clojure "1.10.1"]]
  :jvm-opts ["-Djdk.attach.allowAttachSelf"]
  :repl-options {:init-ns som.core})
