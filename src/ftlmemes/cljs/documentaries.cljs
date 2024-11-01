(ns ftlmemes.cljs.documentaries)

;; (js/alert "hi")

(defn update-word-button
  [w]
  (set! (.. (js/document.getElementById
              "filter-button-content")
            -innerHTML)
        w))

(defn
  filter-grid
  [pred]
  (doseq
      [e
       (js/document.querySelectorAll
        "[data-grid-config]")]
      (let [grid-config (.. e -dataset -gridConfig)
            visible? (pred
                      (js->clj
                       (js/JSON.parse grid-config)))]
        (set!
         (.. e -style -display)
         (if visible? "block" "none")))))

(defn onClickKeyword
  [e w]
  (set! (.. (js/document.getElementById "keyword-button")
            -style
            -display)
        "flex")
  (let [word w]
    (filter-grid (fn [grid-config]
                   (println grid-config)
                   (let [grid-config-words (get grid-config
                                                "keywords")]
                     (some (fn [w] (= w word))
                           grid-config-words)))))
  (update-word-button w))

(defn onClickFilterButton
  []
  (set! (.. (js/document.getElementById "keyword-button")
            -style
            -display)
        "none")
  (filter-grid (constantly true)))

(set! (.. js/window -onClickKeyword) onClickKeyword)
(set! (.. js/window -onClickFilterButton) onClickFilterButton)
