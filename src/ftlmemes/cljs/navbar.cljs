(ns navbar
  (:require
   [clojure.string :as str]
   [reagent.core :as r]
   [reagent.dom :as rdom]))

(def rand-button (js/document.getElementById "rand-page-button"))

(defn fetch-text [file cb]
  (->
   (js/fetch file)
   (.then (fn [x] (.text x)))
   (.then (fn [x] (cb x)))))

(defn pages [cb]
  (fetch-text
   "posts-list.edn"
   (fn [s]
     #_{:clj-kondo/ignore [:unresolved-symbol]}
     (cb (read-string s)))))

(defn rand-page! [_]
  (pages
   (fn [lst]
     (let [path
           (:path
            (rand-nth
             (into [] (remove (comp #{"contact.html"} :path)) lst)))]
       (set! (.-location js/window) path)))))

(.addEventListener rand-button "click" rand-page!)

(def state (r/atom {:tags #{}}))
(pages
 (fn [v]
   (let [v (map #(update % :tags set) v)]
     (swap! state assoc :pages v))))

(defn search-bar [{:keys [on-change]}]
  (let [query (r/atom "")]
    (fn []
      [:input
       {:type "text"
        :value @query
        :placeholder "Search..."
        :on-change (fn [e] (on-change (reset! query (-> e .-target .-value))))}])))

(defn toggle-tag-fn [tag on?] (fn [std] (update std :tags (if on? disj conj) tag)))

(defn tag-ui [tag the-tags]
  [:button
   {:style
    {:margin-left "1rem"
     :height "1.5rem"
     :padding "4px"
     :font-size "0.9rem"
     :color
     (if (the-tags tag) "black" "white")
     :background-color
     (if (the-tags tag) "#ffb300" "#2b2b2b")}
    :on-click
    (fn [e]
      (let [on? (the-tags tag)]
        (swap! state (toggle-tag-fn tag on?)))
      (. e stopPropagation)
      (. e preventDefault)
      (. js/window scrollTo 0 0))}
   [:div
    tag
    [:strong
     {:style
      {:padding "2px" :border "1px solid" :border-radius "20%"}}
     (count
      (filter
       (fn [{:keys [tags]}] (tags tag))
       (-> @state :pages)))]]])

(defn highlight-search [text search-term]
  (let [
        ;; s "foo bar foo"
        ;; search-term "bar"
        s text]
    (if-let [start (str/index-of s search-term)]
      (let [end (+ (count search-term) start)]
        {:prefix (subs s 0 start)
         :highlight
         (subs s start end)
         :postfix (subs s end)})
      {:postfix text})))

(defn page-ui [{:keys [path description tags search-preview]} state-tags]
  [:div {:style {:padding "4px" :margin-bottom "0.4rem"}}
   [:li.hoverable
    {:style {
             :display "block"
             :background-color
             "var(--accent-bg)"} }
    [:a
     {:href path :style {:display "block"}}
     [:div
      {:style {:display "flex"
               :justify-content "space-between"
               :align-items "center"
               :flex-wrap "wrap"}}
      [:div description]
      [:div
       (doall (map #(tag-ui % state-tags) (sort tags)))]]]]
   (when (seq (:preview-lines search-preview))
     [:div {:style {:margin-top "0.4rem"}}
      (doseq [line (:preview-lines search-preview)]
        (let [{:keys [prefix highlight postfix]} (highlight-search line (:q search-preview))]
          [:span prefix [:span {:style {:color "var(--accent)"}} highlight] postfix]))])])

(def relevant-tag? (complement #{"public" "feed"}))

(defn filtered-posts [{:keys [q pages tags]}]
  (let [{:keys [query]} q
        filter-tags (fn [page] (every? (:tags page) tags))
        filter-q (fn []
                   (let [query (str/lower-case query)
                         words (str/split query #"\s+")
                         words-match? (fn [cand] (every? (fn [word] (str/index-of cand word)) words))]
                     (fn [{:keys [path description]}]
                       (or (words-match? (str/lower-case description))
                           (words-match? (str/lower-case path))))))]
    (cond->>
        pages
        (seq tags) (filter filter-tags)
        (<= 2 (count query)) (filter (filter-q))
        :always (map (fn [page] (update page :tags #(into #{} (filter relevant-tag? %))))))))

(defn +search-result [pages search-result]
  (if-not (seq search-result)
    pages
    (into {})
    ))

(defn posts-list [{:keys [posts tags]}]
  (when (seq posts)
    [:ul {:style {:max-width "100vw"
                  :display "flex"
                  :justify-content "center"
                  :flex-direction "column"}}
     (doall
      (for [[idx page] (map-indexed vector posts)]
        ^{:key idx} [page-ui page tags]))]))

(defn tags-ui [{:keys [tags]} all-tags]
  [:div {:style {:display "flex" :justify-content "center"
                 :flex-wrap "wrap" :align-items "center"}}
   (doall (map #(tag-ui % tags) (sort all-tags)))])

(defn all-tags [{:keys [pages]}] (into #{}
                                       (comp
                                        (mapcat :tags)
                                        (filter relevant-tag?))
                                       pages))

(defn spinner []
  [:div.spinner
   {:style
    {:display :flex
     :justify-content :center}}
   [:div {:style {:grid-area :spinner-item}}]
   [:div {:style {:grid-area :spinner-item}}]
   [:div {:style {:grid-area :spinner-item}}]])

(defn search! [_q]
  ;; (.then (js/fetch "/search" (clj->js {:method :post})) (comp println :foo reader/read-string))

  )

;; debounce
;; then loading
;; when back
;; render
;; posts -> lines

(defn ui []
  (let [std @state]
    [:div
     [search-bar
      {:style {:margin-bottom "1rem"} :on-change #(swap! state assoc-in [:q :query] %)}]
     [tags-ui @state (all-tags @state)]
     (let [{:keys [tags]} std]
       [posts-list {:posts (take 3 (filtered-posts std)) :tags tags}])
     (when (-> std :loading #{:posts})
       [spinner])
     ;; (let [{:keys [tags] :as std} @state]
     ;;   [posts-list {:posts (filtered-posts std) :tags tags}])
     ]))

(rdom/render [ui] (.getElementById js/document "navbar"))

(comment
  (swap! state assoc :loading :posts)
  (-> @state :loading #{:posts})
  (def search-result
    [{:lines ["An alternative title for this post could be <i>Joy is power</i>"], :path "the-joy-of-clojure.html"} {:lines ["The code is fluid under the hands of the programmer in a perpetual dance of creation, modification, and observation. It is an intimate conversation with the ideational fabric that weaves itself into existence as the program - the programmers' thought reflections observed immediately, altered rapidly, and understood fully."], :path "conversation-1.html"} {:lines ["alternative clients for some reason)."], :path "extending-your-reach.html"} {:lines ["A simple alternative: make files."], :path "scratching-in-space.html"}])

  (let [pages (:pages @state)
        searched-pages (into {} (map (juxt :path identity)) search-result)]
    (into {}
          (map
           (fn [{:keys [path]}]
             ))
          pages)
    )



  )
