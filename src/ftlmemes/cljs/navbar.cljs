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

(def state (r/atom {}))
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

(defn page-ui [{:keys [path description]}]
  [:li [:a {:href path}] description])

(defn filtered-posts [{:keys [q pages]}]
  (let [{:keys [query tags]} q
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
        (<= 2 (count query)) (filter (filter-q)))))


(defn posts-list [posts]
  (when (seq posts)
    [:ul
     (doall
      (for [[idx page] (map-indexed vector posts)]
        ^{:key idx} [page-ui page]))]))

(defn ui []
  [:div
   [search-bar {:on-change #(swap! state assoc-in [:q :query] %)}]
   [posts-list (filtered-posts @state)]])

(rdom/render [ui] (.getElementById js/document "navbar-app"))
