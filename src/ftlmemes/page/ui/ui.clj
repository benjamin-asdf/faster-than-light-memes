(ns ftlmemes.page.ui.ui
  (:require [shadow.css :refer (css)]
            [hiccup.util :as html]
            [hiccup.page :as hp]))

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
