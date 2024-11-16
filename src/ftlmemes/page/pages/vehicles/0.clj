(ns ftlmemes.page.pages.vehicles.0
  (:require
    ;; [clojure.data.json :as json]
    [shadow.css :refer (css)]
    ;; [hiccup.util :as html]
    [ftlmemes.page.ui.ui :as ui]
    [hiccup.page :as hp]))

;; -----------------------

(defn divider
  []
  [:div
   {:class (css :my-2
                :min-h-1 :w-full
                :rounded-lg :bg-slate-800)}])

(defn home-button
  []
  [:a
   {:class (css :rounded
                :p-3
                {:background "#feb48f"
                 :color "black"
                 :height "25px"
                 :width "25px"})
    :href "/"} "Home"])

(defn link-style [] (css :font-bold {:color "#feb48f"}))

;; -------------------------------------

(defn picture-iframe
  [pic version width height]
  [:iframe
   (let [width 300
         height 350]
     {:height height
      :src
        (format
          "https://vehicles.faster-than-light-memes.xyz/art/pe/%s/%s?width=%s&height=%s"
          pic
          version
          width
          height)
      :width width})])




;; vehicles:

;;
;; - Braitenberg as a kind of summary public of his thinking
;; - doesn't capture his more advanced theories of cortex (neuronal ensembles)
;; -
;;
;;


;; -----------------------------------
;; reasons:
;;
;; ---------------------
;; 1. go slow
;; 2. go bottom up
;; 3. have a feel for it while it grows (bottom up)
;; 4. if you make an AGI, you better be doing it sensual
;; ---------------------
;;
;; The concept of sensuality and software is largely lost, but a part of the
;; AI-lab hacker ethos.
;; Where software is all about *expression*
;;
;;
;;

[]
