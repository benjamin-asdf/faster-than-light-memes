(ns ftlmemes.page.pages.documentaries
  (:require [clojure.data.json :as json]
            [shadow.css :refer (css)]
            [hiccup.util :as html]
            [hiccup.page :as hp]))

(defn quote-ui [q]
  [:p
   {:class (css :my-2 :italic)}
   q])

(defn einstein-quote
  []
  [:div
   (quote-ui
     "I fully agree with you about the significance and educational value of methodology as well as history and philosophy of science. So many people today—and even professional scientists seem to me like somebody who has seen thousands of trees but has never seen a forest. A knowledge of the historic and philosophical background gives that kind of independence from prejudices of his generation from which most scientists are suffering. This independence created by philosophical insight is in my opinion the mark of distinction between a mere artisan or specialist and a real seeker after truth. (Einstein to Thornton, 7 December 1944, EA 61–574)
")
   [:p "Peter Pesic had this in one of his talks. "]
   [:p "I too hope we can become such seekers. "]])

(defn home-button

  [:a
   {:class (css :rounded
                :p-3
                ;; :bg-red-100
                {
                 :background "#feb48f"
                 :color "black"
                 :height "25px"
                 :width "25px"})
    :href "/"} "Home"])

(defn keywords-ui
  [words]
  [:div
   {:class (css :font-bold :text-center :w-full)}
   (interpose [:span " "]
              (map-indexed (fn [idx w]
                             [:button
                              {:data-word w
                               :onClick (str "onClickKeyword(event,"
                                             (format "'%s'" w)
                                             ");")}
                              [:span
                               (str w
                                    (when (< idx (dec (count words)))
                                      ","))]])
                           words))])


(comment
  (def words ["foo" "bar"])
  (keywords-ui words))

(defmulti render-content (fn [kind _] kind))
(defmethod render-content :content/keywords
  [_ {:keys [keywords]}]
  (keywords-ui keywords))

(defmethod render-content :content/ui [_ {:keys [ui]}] ui)

(defn grid-card
  [{:as opts
    :keys [content youtube-link title link preview
           youtube-link-no-embed]}]
  [:div
   {:class (css "grid-card"
                :shadow
                :bg-white
                :p-2
                {:color "black"}
                :font-semibold
                :rounded
                {:min-height "350px"}
                {:min-width "250px"})
    :data-grid-config (json/write-str opts)}
   [:div {:class (css :flex :flex-col :h-full)}

    [:div {:class (css :flex :justify-center)}
     (list
      (when youtube-link
        [:div
         {:class (css :rounded
                      :overflow-hidden
                      ;; {:width "250"
                      ;;  :height "250"}
                      )}
         (html/raw-string
          (format
           "<iframe height=\"250\" width=\"250\" src=\"%s\" title=\"%s\" frameborder=\"0\" allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share\" referrerpolicy=\"strict-origin-when-cross-origin\" allowfullscreen></iframe>
"
           youtube-link
           title))])
      preview)]

    [:div
     {:class (css :flex :flex-col
                  :justify-between :h-full
                  :mt-2 :rounded
                  :border-2 :p-2
                  :border-black :bg-red-100)}
     [:div
      {:class (css :w-full
                   :text-xl :py-1
                   :flex :items-center
                   :text-center :justify-center)} title]
     (map (fn [k] (render-content k opts)) content)
     ]]])

(def documentaries-config
  [{:content [:content/keywords]
    :keywords ["cybernetics" "sci-fy" "AI"
               "neurophilosophy"]
    :title "McCulloch being spry"
    :youtube-link
      "https://www.youtube.com/embed/wawMjJUCMVw?si=T3C"}
   {:content [:content/keywords]
    :keywords ["ethology" "behaviour" "animals"
               "cybernetics" "zoology"]
    :title "Konrad Lorenz"
    :youtube-link
      "https://www.youtube.com/embed/IysBMqaSAC8?si=Kz1LJ8TMj_voYQC4"}
   {:content [:content/keywords]
    :keywords ["theoretical" "physics" "time-travel"
               "sci-fy"]
    :preview
      [:a
       {:href
          "https://youtu.be/C6_gxoLwrWw?si=3SPJcI4UrW9o1h7B"}
       [:img
        {:height "250"
         :src
           "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSXHRHFpjiFI7n-U0cpRcXD18czNsWJ_npzFA&s"
         :width "250"}]]
    :title
      "David Deutsch - Time Travel amongst other things"}
   {:content [:content/ui :content/keywords]
    :keywords ["meta" "history" "science"]
    :title "Albert Einstein"
    :ui (einstein-quote)}
   {:content [:content/keywords]
    :keywords ["computer-science" "computer" "Ada Lovelace"]
    :preview
      [:a
       {:href
          "https://youtu.be/QgUVrzkQgds?si=2Qrbb0jKpA2_llgY"}
       [:img
        {:height "250"
         :src
           "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a4/Ada_Lovelace_portrait.jpg/800px-Ada_Lovelace_portrait.jpg"
         :width "250"}]]
    :title
      "BC DOCUMENTARY : Calculating Ada - The Countess of Computing"}
   {:content [:content/keywords]
    :keywords ["theoretical" "physics" "cybernetics"
               "design" "beauty" "elegance" "art"
               "philosophy"]
    :title
      "David Deutsch - Why Are Flowers Beautiful? A not-yet classic."
    :youtube-link
      "https://www.youtube.com/embed/gT7DFCF1Fn8?si=hhJ_59vg_CDjqB92"}
   {:content [:content/keywords]
    :keywords ["cybernetics" "philosophy" "dennett"]
    :title
      "Daniel Dennett - \"Where am I?\" - This inspired the movie The Matrix"
    :youtube-link
      "https://www.youtube.com/embed/KP7rTp2vwTs?si=9sgkXa44k1zJszqr"}
   {:content [:content/keywords]
    :keywords ["neurophilosophy" "cybernetics" "programming"
               "computer-science" "Dave Ackley"
               "György Buzsáki" "Joscha Bach"
               "Christoph von der Malsburg"]
    :title "Vectors of Cognitive AI: Self-Organization"
    :youtube-link
      "https://www.youtube.com/embed/NEf8LnTD0AA?si=5fCPywETdZRxzzhq"}
   {:content [:content/keywords]
    :keywords ["neurophilosophy" "cybernetics" "philosophy"
               "Heinz Von Foerster" "Humberto Maturana"
               "second-order-cybernetics"]
    :title "Heinz Von Foerster and Humberto Maturana"
    :youtube-link
      "https://www.youtube.com/embed/Mc6YFUoPWSI?si=ATn27aprI9xwcnw0"}
   {:content [:content/keywords]
    :keywords ["cybernetics" "philosophy" "Stafford Beer"]
    :title "Cybernetics, History & Origins - Stafford Beer"
    :youtube-link
      "https://www.youtube.com/embed/XbcBWdeIcyY?si=ALW4r7o2AhpmPna-"}
   {:content [:content/keywords]
    :keywords ["programming" "lisp" "scheme"
               "Gerald Sussman" "Harold Abelson"
               "philosophy"]
    :title "SCIP Lectures"
    :youtube-link
      "https://www.youtube.com/embed/-J_xL4IGhJA?si=y0wbVLHgYfVxq_Jb"}
   {:content [:content/keywords]
    :keywords ["neurophilosophy" "neuroscience" "cortex"
               "thalamus" "Murray Sherman"]
    :title "Murray Sherman - Thalamocortical System Part I"
    :youtube-link
      "https://www.youtube.com/embed/aB2M1gg_1sU?si=XifGjAmP-CRr03bm"}
   {:content [:content/keywords]
    :keywords
      ["neurophilosophy" "cybernetics" "computer-science"
       "hyperdimensional-computing" "biological-computing"]
    :title
      "Pentti Kanerva - The computer and the brain - If Von Neuman would have lived longer"
    :youtube-link
      "https://www.youtube.com/embed/1g5VEcnG6fI?si=3FAcvI3AuZuAAvkm"}
   {:content [:content/keywords]
    :keywords ["neurophilosophy" "engineering"
               "György Buzsáki" "rhythms"]
    :title
      " BS 172 \"The Brain from the Inside Out\" with György Buzsáki"
    :youtube-link
      "https://www.youtube.com/embed/pJhlMsc2UKQ?si=foBnhrtW_TQ_k9xJ"}])

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
         {:class (css :hidden [:md :block]
                      :absolute {:left "5%" :top "5%"})}
         (home-button)]
        [:div
         {:class (css :flex :items-center
                      :justify-end :w-full)}
         [:div
          ;; title area
          {:class (css :flex :items-center
                       :justify-center :w-full)}
          [:div
           {:class (css :mt-6 :mb-4
                        :flex :flex-col
                        :items-center :justify-center)}
           [:div {:class (css :font-bold :text-4xl)}
            "Documentaries / Historical"]
           [:div {:class (css :mt-2 :font-bold)}
            "timeless, relevant, joyful"]]]
         [:div
          {:class (css :absolute
                       ;; :ml-auto
                       {:right "5%"}
                       :self-end
                       ;; :ml-20
                       ;; {:max-height "26px"}
                       ;; :bg-red-200
                       )} (keyword-button)]]
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

;; (do
;;   (require '[ftlmemes.page.gen])
;;   (ftlmemes.page.gen/gen-html!
;;    {:gen/file "documentaries.html"
;;     :gen/content (page)}))
