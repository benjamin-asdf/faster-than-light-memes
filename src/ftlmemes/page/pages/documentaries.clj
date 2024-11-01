(ns ftlmemes.page.pages.documentaries
  (:require [clojure.data.json :as json]
            [shadow.css :refer (css)]
            [hiccup.util :as html]
            [hiccup.page :as hp]))

(defn
  keywords-ui
  [words]
  [:div
   {:class (css :font-bold)}
   (interpose
    [:span ", "]
    (map
     (fn [w]
       [:button
        {:data-word w
         :onClick (str "onClickKeyword(event," (format "'%s'" w) ");")}
        [:span w]])
     words))])

(defmulti render-content (fn [kind _] kind))
(defmethod render-content :content/keywords
  [_ {:keys [keywords]}]
  (keywords-ui keywords))

(defn grid-card
  [{:as opts
    :keys [content youtube-link title link]}]
  [:div
   {:data-grid-config (json/write-str opts)
    :class (css "grid-card"
                :shadow
                :bg-white
                :p-2
                {:color "black"}
                :font-semibold
                :rounded-sm
                {:min-height "350px"}
                {:min-width "250px"})}
   [:div {:class (css :flex :flex-col)}
    [:div
     {:class (css :flex :justify-center)}
     [:div
      {:class
       (css :rounded
            :overflow-hidden
            ;; {:width "250"
            ;;  :height "250"}
            )}
      (html/raw-string
       (format
        "<iframe height=\"250\" width=\"250\" src=\"%s\" title=\"%s\" frameborder=\"0\" allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share\" referrerpolicy=\"strict-origin-when-cross-origin\" allowfullscreen></iframe>
"
        youtube-link
        title))]]
    [:div
     {:class (css :w-full :text-xl
                  :py-1 :flex
                  :items-center :justify-center)} title]
    [:div
     {:class (css :border-2 :rounded :p-2 :border-black)}
     (map (fn [k] (render-content k opts)) content)]]])

(def documentaries-config
  [{:content [:content/keywords]
    :keywords ["cybernetics" "sci-fy" "AI"]
    :title "McCulloch being spry"
    :youtube-link
      "https://www.youtube.com/embed/wawMjJUCMVw?si=T3C"}
   {:content [:content/keywords]
    :keywords ["ethology" "behaviour" "animal" "cybernetics"
               "zoology"]
    :title "Konrad Lorenz"
    :youtube-link
    "https://www.youtube.com/embed/IysBMqaSAC8?si=Kz1LJ8TMj_voYQC4"}])

(defn keyword-button
  []
  [:button#keyword-button
   {:class (css {:display
                   ;; "flex"
                   "none"}
                ;; :flex
                :justify-center
                :items-center {:color "black"
                               :max-height "2rem"
                               :max-width "120px"
                               :min-width "120px"}
                :font-semibold :rounded
                :ml-6 :p-4
                :bg-white :shadow)
    :onClick "onClickFilterButton();"}
   [:span#filter-button-content {:class (css :truncate)}
    ""]])

(defn page
  []
  (hp/html5
    [:html
     [:head [:meta {:charset "UTF-8"}]
      [:meta
       {:content "width=device-width, initial-scale=1"
        :name "viewport"}]
      [:link {:href "data:," :rel "shortcut icon"}]
      [:link {:href "data:," :rel "apple-touch-icon"}]
      ;; [:link
      ;;  {:rel "stylesheet"
      ;;   :href
      ;;   "https://cdn.simplecss.org/simple.min.css"}]
      [:link {:href "/css/ui.css" :rel "stylesheet"}]
      [:style]
      [:script
       {:src
          "https://cdn.jsdelivr.net/npm/scittle@0.6.19/dist/scittle.js"}]
      [:title "FTLM - Historical Science Documentaries"]]
     [:body
      {:class (css :w-full
                   :h-full
                   :bg-black
                   {:color "white"}
                   :font-mono)}
      [:div
       {:class (css :flex :items-center
                    :justify-end :w-full)}
       [:div
        ;; title area
        {:class (css :flex :items-center
                     :justify-center :w-full)}
        [:div {:class (css :flex :items-center :gap-2)}
         [:div
          {:class (css :mt-6 :mb-4 :font-bold :text-4xl)}
          "Documentaries"
          [:div {:class (css :mt-2 :font-bold :text-sm)}
           "Old stuff that is fascinating to me"]]]]
       [:div
        {:class (css :absolute
                     ;; :ml-auto
                     {:right "5%"}
                     :self-end
                     ;; :ml-20
                     ;; {:max-height "26px"}
                     ;; :bg-red-200
                     )}
        (keyword-button)]]
      [:div
       {:class (css :grid
                    [:sm :grid-cols-1]
                    [:md :grid-cols-2]
                    [:lg :grid-cols-3]
                    :gap-3
                    :p-3 :px-6
                    :w-full :h-full)}
       (map grid-card documentaries-config)]
      [:script
       {:src "documentaries.cljs"
        :type "application/x-scittle"}]]]))

[{:gen/file "documentaries.html"
  :gen/content (page)}]

(comment
  (ftlmemes.page.gen/gen-html!
   {:gen/file "documentaries.html"
    :gen/content (page)}))
