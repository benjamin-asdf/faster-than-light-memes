(ns
    ftlmemes.page.pages.documentaries
  (:require
   [shadow.css :refer (css)]
   [hiccup.util :as html]
   [hiccup.page :as hp]))


(def primary "#00FF00")
(def secondary "#00FFFF")
(def tert "#FF00FF")
(def button-text-color "#FFFFFF")

(defn
  grid-card
  [{:keys []}]
  [:div
   {:class (css
             :shadow
             :bg-red-500
             {:min-height "350px"}
             {:min-width "250px"}
             )}
   ""])

(defn
  page
  []
  (hp/html5
      [:html
       [:head
        [:meta {:charset "UTF-8"}]
        [:meta
         {:name "viewport"
          :content "width=device-width, initial-scale=1"}]
        [:link
         {:rel "shortcut icon"
          :href "data:,"}]
        [:link
         {:rel "apple-touch-icon"
          :href "data:,"}]
        ;; [:link
        ;;  {:rel "stylesheet"
        ;;   :href "https://cdn.simplecss.org/simple.min.css"}]
        [:link
         {:rel "stylesheet"
          :href "/css/ui.css"}]
        [:style]
        [:title
         "FTLM - Historical Science Documentaries"]]
       [:body
        {:class (css
                  :w-full
                  :h-full
                  :bg-black
                  {:color "white"}
                  :font-mono)}
        [:div
         ;; title area
         {:class (css
                   :flex
                   :items-center
                   :justify-center
                   :w-full
                   :h-full
                   )}
         [:div
          {:class (css
                    :mt-6
                    :font-bold
                    :text-4xl)}
          "Documentaries"]]
        [:div
         {:class
          (css :bg-purple-800
               :grid
               :grid-cols-3
               :gap-3
               :p-3
               :w-full
               :h-full
               )}

         (list
          (grid-card {})
          (grid-card {})
          (grid-card {}))
         ]

        ;;     <script src=\"https://cdn.jsdelivr.net/npm/scittle@0.6.15/dist/scittle.nrepl.js\" type=\"application/javascript\"></script>
        ;; ")
        ;; [:script {:type "application/x-scittle" :src ""}]
        ]]))


[{:gen/file "documentaries.html"
  :gen/content (page)}]

(ftlmemes.page.gen/gen-html!
 {:gen/file "documentaries.html"
  :gen/content (page)})
