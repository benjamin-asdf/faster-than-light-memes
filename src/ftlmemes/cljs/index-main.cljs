(ns index-main)

(defn- get-client-rect
  "Extract an element's client rectangle in the browser
  for use by react-pdf to display the PDF page in the
  correct size."
  [node]
  (let [rect (.getBoundingClientRect node)]
    {:left (.-left rect)
     :top (.-top rect)
     :right (.-right rect)
     :bottom (.-bottom rect)
     :width (.-width rect)
     :height (.-height rect)}))

(defn rect-scroll-move-with-mouse-1
  [node [x y] virtual-rect-ratio]
  (let [{:keys [width height top bottom left right]} (get-client-rect node)
        scroll-height  (.-scrollHeight node)
        scroll-width (.-scrollWidth node)
        virtual-top (+ top (* height  (- 1 virtual-rect-ratio)))
        virtual-bottom (- bottom (* height (- 1 virtual-rect-ratio)))
        virtual-height (- virtual-bottom virtual-top)
        virtual-dist-y (- y virtual-top)
        virtual-rel-dist-y (/ virtual-dist-y virtual-height)
        virtual-left (+ left (* width  (- 1 virtual-rect-ratio)))
        virtual-right (- right (* width (- 1 virtual-rect-ratio)))
        virtual-width (- virtual-right virtual-left)
        virtual-dist-x (- x virtual-left)
        virtual-rel-dist-x (/ virtual-dist-x virtual-width)
        scroll-y (* virtual-rel-dist-y scroll-height)
        scroll-x (* virtual-rel-dist-x scroll-width)]
    [scroll-x scroll-y]))

(defn rect-scroll-move-with-mouse
  "Scroll a rect together with the mouse.
  Such that the user can target elements in the scroll.
  The user achieves full motion over the rect by moving inside a smaller, virtual
  rect.

   node: the target scroll node.
  `[x y]`: presumably the mouse x and y.
  `virtual-rect-ratio`: The relative size of the virtual overlay react. 1 means
  the user needs to use the whole rect for movement. 0 is undefinend.
  "
  ([node [x y]] (rect-scroll-move-with-mouse node [x y] 0.8))
  ([node [x y] virtual-rect-ratio]
   (let [[scroll-x scroll-y]
         (rect-scroll-move-with-mouse-1 node [x y] virtual-rect-ratio)]
     (.scrollTo node scroll-x scroll-y))))


(defn rotate-on-mouse-move [node mouse-x mouse-y]
  (let [{:keys [left top width height right]} (get-client-rect node)
        centerX (+ left (/ width 2))
        centerY (+ top (/ height 2))
        dx (- centerX mouse-x)
        dy (- centerY mouse-y)
        angle-rad (Math/atan2 dy dx)
        rotation-offset-radians (- Math/PI (/ Math/PI 2))]
    (.setAttribute node "style" (str "transform: rotate(" (+ angle-rad rotation-offset-radians) "rad)"))))

(def head-elem (js/document.getElementById "head-img"))

(.addEventListener head-elem
                   (fn [event]
                     (rotate-on-mouse-move head-elem (.-pageX event) (.-pageY event))))
