(defproject ldapadmin "0.1.0-SNAPSHOT"
  :description "An LDAP adminitration utility written in SBCL on the
  server-side and ClojureScript on the client-side. This is the
  client-side component."
  :url "FIXME"
  :license "public domain"
  :dependencies [[org.clojure/clojure "LATEST"]
                 [org.clojure/clojurescript "LATEST"]
                 [cljs-ajax "LATEST"]
                 [prismatic/dommy "LATEST"]
                 [hiccups "LATEST"]]
  :plugins [[lein-cljsbuild "LATEST"]]
  :clean-targets ^{:protect false} [:target-path "out" "resources/public/cljs"])
