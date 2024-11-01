(ns
    ftlmemes.page.gen
    (:require
     [clojure.string :as str]
     [babashka.fs :as fs]))

(def output-dir "public")

(defn gen-html! [{:gen/keys [file content]}]
  (let [file (fs/file output-dir file)]
    (println (str file))
    (spit file content)))
