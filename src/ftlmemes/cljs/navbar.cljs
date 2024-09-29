(ns navbar
  (:require
   [clojure.string :as str]
   [reagent.core :as r]
   [reagent.dom :as rdom]))

(def rand-button (js/document.getElementById "rand-page-button"))

(defn fetch-text
  ([file cb] (fetch-text file cb {}))
  ([file cb opts]
   (->
    (js/fetch file (clj->js opts))
    (.then (fn [x] (.text x)))
    (.then (fn [x] (cb x))))))

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

(def state (r/atom {:tags #{} :navbar-open? false}))
(pages
 (fn [v]
   (let [v (map #(update % :tags set) v)]
     (swap! state assoc :pages v))))

(defn search-bar [{:keys [on-change]}]
  (fn []
    [:input
     {:style {:margin-bottom "0px"}
      :type "text"
      :value (-> @state :q :query)
      :placeholder "Search..."
      :on-change (fn [e] (on-change
                          (let [q (-> e .-target .-value)]
                            (swap! state assoc-in [:q :query] q)
                            q)))}]))

(defn toggle-tag-fn [tag on?] (fn [std] (update std :tags (if on? disj conj) tag)))

(defn tag-ui [tag the-tags]
  [:button
   {:style
    {:margin-left "1rem"
     ;; :height ""
     :padding "1rem"
     :font-size "16px"
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
      {:padding "1px"
       }}
     (str
      " "
      (count
       (filter
        (fn [{:keys [tags]}] (tags tag))
        (-> @state :pages)))
      "")]]])

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

(defn page-ui
  [{:keys [path description tags search-preview]}
   state-tags]
  [:div {:style {:margin-bottom "0.4rem" :padding "4px"}}
   [:li.hoverable {:style {:display "block"}}
    [:a {:href path :style {:display "block"}}
     [:div
      {:style {:align-items "center"
               :display "flex"
               :flex-wrap "wrap"
               :justify-content "space-between"}}
      [:div description]
      [:div
       (doall (map #(tag-ui % state-tags) (sort tags)))]]]]
   (when (seq (:preview-lines search-preview))
     [:div {:style {:margin-top "0.4rem"}}
      (doall
        (for [line (take 25
                         (:preview-lines search-preview))]
          (let [{:keys [prefix highlight postfix]}
                  (highlight-search line
                                    (:q search-preview))]
            [:span
             {:style {:display "block"
                      :margin-bottom "1px"}} prefix
             [:span {:style {:color "var(--accent)"}}
              highlight] postfix])))])])

(def relevant-tag? (complement #{"public" "feed"}))

(defn pages->lut [pages]
  (into {} (map (juxt :path identity)) pages))

(defn ->search-preview [{:keys [lines]} q]
  {:preview-lines lines :q q})

(defn filtered-posts [{:keys [q pages tags search-result]}]
  (let [{:keys [query]} q
        filter-tags (fn [page] (every? (:tags page) tags))
        path->search-result (pages->lut (:results search-result))
        filter-q (fn []
                   (let [query (str/lower-case query)
                         words (str/split query #"\s+")
                         words-match? (fn [cand] (every? (fn [word] (str/index-of cand word)) words))]
                     (fn [{:keys [path description]}]
                       (or (path->search-result path)
                           (words-match? (str/lower-case description))
                           (words-match? (str/lower-case path))))))]
    (cond->>
        pages
        (seq tags) (filter filter-tags)
        (<= 2 (count query)) (filter (filter-q))
        :always (map (fn [page] (update page :tags #(into #{} (filter relevant-tag? %)))))
        :always (map (fn [{:keys [path] :as page}]
                       (assoc
                        page
                        :search-preview
                        (when-let [prew (path->search-result path)]
                          (->search-preview prew (:q search-result)))))))))

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

(defn debounce [f interval]
  (let [id (atom nil)]
    (fn [& args]
      (when-let [last-id @id]
        (js/clearTimeout last-id))
      (reset!
       id
       (js/setTimeout
        #(apply f args)
        interval)))))

(defn on-search-sucess [search-result]
  (swap!
   state
   (fn [s]
     (-> s
         (assoc :search-result search-result)
         (update :loading (fnil disj #{}) :posts)))))

(defn search-1 [q]
  (swap! state update :loading (fnil conj #{}) :posts)
  (->
   (js/fetch
    "/search"
    (clj->js
     {:method :post
      :headers {"Content-Type" "application/edn"
                "accept" "application/edn"}
      :body (prn-str {:q (subs q 0 255)})}))
   (.then #(.text %))
   (.then (comp on-search-sucess read-string)))
  #_(js/setTimeout (fn [] (on-search-sucess search-result)) 1000))

(def search! (debounce search-1 500))

(def no-result? :no-result?)

(defn no-result-ui [{:keys [q]}] [:div "no content search results for " [:strong q]])

(defn navbar-ui []
  (let [std @state]
    [:div
     [:div {:style {:display :flex
                    :justify-content "center"
                    :align-items "center"
                    :margin-bottom "1rem"
                    :margin-top "0.5rem"}}
      [search-bar
       {:on-change
        (fn [q]
          (when (< 3 (count q)) (search! q)))}]
      [:button {:style {:margin-left "4px"} :on-click (fn [_] (swap! state dissoc :q :search-result))} "clear"]]
     [tags-ui @state (all-tags @state)]
     (let [{:keys [tags search-result]} std
           posts (filtered-posts std)]
       [:<>
        [posts-list {:posts posts :tags tags}]
        (when (no-result? search-result) [no-result-ui search-result])])
     (when (-> std :loading :posts)
       [spinner])]))

(defn ui [] [navbar-ui])

(defn topbar-button
  []
  [:div {:style {:display "flex"}}
   [:button
    {:onClick (fn []
                (let
                  [e (.getElementById js/document "topbar")]
                  (swap! state update :navbar-open? not)
                  (if (:navbar-open? @state)
                    (set! (.. e -style -display) "block")
                    (set! (.. e -style -display) "none"))
                  (.. js/console (log e))))} "navbar"]])

(rdom/render [navbar-ui] (.getElementById js/document "navbar"))
(rdom/render [topbar-button] (.getElementById js/document "topbar-button"))

(comment
  (set!
   (.getElementById js/document "topbar")
   )



  (.getElementById js/document "topbar-button")


  (println (.getElementById js/document "navbar2"))

  (reset! state nil)

  (search! "small")
  (on-search-sucess {:q "foo" :no-result? true})
  (-> @state :loading :posts)
  (swap! state assoc :loading :posts)
  (def search-result
    {:results
     [{:lines ["An alternative title for this post could be <i>Joy is power</i>"], :path "the-joy-of-clojure.html"} {:lines ["The code is fluid under the hands of the programmer in a perpetual dance of creation, modification, and observation. It is an intimate conversation with the ideational fabric that weaves itself into existence as the program - the programmers' thought reflections observed immediately, altered rapidly, and understood fully."], :path "conversation-1.html"} {:lines ["alternative clients for some reason)."], :path "extending-your-reach.html"} {:lines ["A simple alternative: make files."], :path "scratching-in-space.html"}]
     :q "alter"})
  (swap! state (fn [s]
                 (-> s
                     (assoc :search-result search-result)
                     (update :loading (fnil disj #{}) :posts))))
  (filtered-posts @state)
  (->search-preview ((pages->lut (-> @state :search-result :results)) "the-joy-of-clojure.html") "fo")
  (-> @state :loading #{:posts})
  (first (+search-result (filtered-posts @state) @state))
  {:path "screencasts.html", :description "Screencasts", :tags #{"video" "clojure" "emacs" "flow" "system"}}
  (-> @state :search-result :results))
