(ns navbar
  (:require
   [clojure.set :as set]
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
        :on-change (fn [e]
                     (on-change (reset! query (-> e .-target .-value))))}])))

(defn tag-on? [tags tag] (tags tag))

(defn toggle-tag-fn [tag on?] (fn [std] (update std :tags (if on? disj conj) tag)))

(defn tag-ui [tag the-tags]
  [:button
   {:style
    {:margin-left "1rem"
     :height "1.5rem"
     :padding "4px"
     :font-size "0.9rem"
     :color
     (if (tag-on? the-tags tag) "black" "white")
     :background-color
     (if (tag-on? the-tags tag) "#ffb300" "#2b2b2b")}
    :on-click
    (fn [e]
      (let [on? (the-tags tag)]
        (swap! state (toggle-tag-fn tag on?)))
      (. e stopPropagation)
      (. e preventDefault)
      )}
   tag])

(defn page-ui [{:keys [path description tags]} state-tags]
  [:li
   [:div {:style {:display "flex"}}
    [:div [:a {:href path} description]]
    [:div
     {:style {:margin-left "auto"}}
     (doall (map #(tag-ui % state-tags) tags))]]])

(def relevant-tag? (complement #{"public" "feed"}))

(defn filtered-posts [{:keys [q pages tags]}]
  (let [{:keys [query]} q
        filter-tags (fn [page] (seq (set/intersection (:tags page) tags)))
        filter-q (fn []
                   (let [query (str/lower-case query)
                         words (str/split query #"\s+")
                         words-match? (fn [cand] (every? (fn [word] (str/index-of cand word)) words))]
                     (fn [{:keys [path description]}]
                       (or (words-match? description)
                           (words-match? path)))))]
    (cond->>
        pages
        (seq tags) (filter filter-tags)
        (<= 2 (count query)) (filter (filter-q))
        :always (map (fn [page] (update page :tags #(filter relevant-tag? %)))))))

(defn posts-list [{:keys [posts tags]}]
  (when (seq posts)
    [:ul
     (doall
      (for [[idx page] (map-indexed vector posts)]
        ^{:key idx} [page-ui page tags]))]))

(defn tags-ui [{:keys [tags]} all-tags]
  [:div {:style {:display "flex"}}
   (doall (map #(tag-ui % tags) all-tags))])

(defn all-tags [{:keys [pages]}] (into #{}
                                       (comp
                                        (mapcat :tags)
                                        (filter relevant-tag?))
                                       pages))
(defn ui []
  [:div
   [search-bar {:style {:margin-bottom "1rem"}}
    {:on-change #(swap! state assoc-in [:q :query] %)}]
   [tags-ui @state (all-tags @state)]
   (let [{:keys [tags] :as std} @state]
     [posts-list {:posts (filtered-posts std) :tags tags}])])

(rdom/render [ui] (.getElementById js/document "navbar"))
