(ns dev.build
  (:require
    [shadow.css.build :as cb]
    [clojure.java.io :as io]))

(defn css-release
  [& args]
  (let [build-state
          (-> (cb/start)
              (cb/index-path
                ;; /home/benj/repos/faster-than-light-memes/src/ftlmemes/page/pages
                (io/file "src" "ftlmemes" "page" "pages")
                {})
              (cb/generate
                '{:ui {:entries
                         [ftlmemes.page.pages.documentaries
                          ftlmemes.page.ui.ui
                          ftlmemes.page.pages.pareidolia]}})
              (cb/minify)
              (cb/write-outputs-to (io/file "public"
                                            "css")))]
    (doseq [mod (:outputs build-state)
            {:as warning :keys [warning-type]} (:warnings
                                                 mod)]
      (prn [:CSS (name warning-type)
            (dissoc warning :warning-type)]))))
