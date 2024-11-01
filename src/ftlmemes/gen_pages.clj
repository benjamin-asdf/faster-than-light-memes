(ns ftlmemes.gen-pages
  (:require
   [clojure.string :as str]
   [babashka.fs :as fs]
   [ftlmemes.page.gen]))

(defn gen-all []
  (run!
   ftlmemes.page.gen/gen-html!
   (sequence
    (comp
     (map str)
     (filter #(re-find #"\.clj$" %))
     (mapcat load-file))
    (file-seq (fs/file "src/ftlmemes/page/pages")))))

(gen-all)

;; http://localhost:8081/hire-benjamin.html
