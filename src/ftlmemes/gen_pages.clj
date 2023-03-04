(ns ftlmemes.gen-pages
  (:require
   [clojure.string :as str]
   [babashka.fs :as fs]))

(def output-dir "public")

(doseq [{:gen/keys [file content]} (sequence
                                    (comp
                                     (map str)
                                     (filter #(re-find #"\.clj$" %))
                                     (mapcat load-file))
                                    (file-seq
                                     (fs/file
                                      "/home/benj/repos/faster-than-light-memes/src/ftlmemes/page")))
        :let [file (fs/file output-dir file)]]
  (println (str file))
  (spit file content))


;; http://localhost:8081/hire-benjamin.html
