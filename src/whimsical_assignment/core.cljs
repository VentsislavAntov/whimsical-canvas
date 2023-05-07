(ns whimsical-assignment.core
  (:require
    [reagent.core :as r]
    [reagent.dom :as d]
    [whimsical-assignment.shapes :as shapes]))
  
;; Constants
;; Corrections used on all shapes to bring them in sight
(def xCorrection -400)
(def yCorrection 140)
(def groupRef (atom nil))
(def previousGroupRef (atom nil))

;; -------------------------
;; Views
;; Draws the shape which is either a circle or a rectangle. If something else, throw error as we only expect circles or rectangles
(defn draw-shape [ctx shape]
  (cond
    (= (:shape-type shape) :rect)
    (let [{:keys [x y width height fill group]} shape] 
      (set! (.-fillStyle ctx) fill)
      (.beginPath ctx)
      (.rect ctx (+ x xCorrection) (+ y yCorrection) width height)
      (.fill ctx))
    (= (:shape-type shape) :circle)
    (let [{:keys [x y radius fill group]} shape]
      (set! (.-fillStyle ctx) fill)
      (.beginPath ctx)
      (.arc ctx (+ x xCorrection) (+ y yCorrection) radius 0 (* 2 Math/PI))
      (.fill ctx))
    :else
    (throw (js/Error. "Invalid shape type"))))

;; Reusable function for drawing an outline for the groups
(defn draw-rectangle-outline [ctx x y width height]
  (set! (.-strokeStyle ctx) "purple")
  (set! (.-lineWidth ctx) 5)
  (.beginPath ctx)
  (.rect ctx x y width height)
  (.stroke ctx))

;; The canvas is in a constant position even when on different screen sizes, so this will work cross-devices even though the outline properties are hardcoded here.
(defn draw-outline [group]
  (let [canvas (.getElementById js/document "my-canvas")
        ctx (.getContext canvas "2d")]
    (cond
      (= group 0)
      ;; draw outline for group 0
      (draw-rectangle-outline ctx 210 33 375 185)
      (= group 1)
      ;; draw outline for group 1
      (draw-rectangle-outline ctx 649 51 240 185)
      (= group 2)
      ;; draw outline for group 2
      (draw-rectangle-outline ctx 221 219 210 250)
      (= group 3)
      ;; draw outline for group 3
      (draw-rectangle-outline ctx 484 220 170 200)
      (= group 4)
      ;; draw outline for group 4
      (draw-rectangle-outline ctx 650 232 340 250))))

;; Render function. On each render draw everything.
(defn render-shapes []
  (let [canvas (.getElementById js/document "my-canvas") ;; Get the canvas element by id
        ctx (.getContext canvas "2d") ;; Get the 2D context of the canvas
        group @groupRef] ;; Get the value of the group atom
    (doseq [shape shapes/shapes]
      (draw-shape ctx shape)
      (draw-outline group))))

;; Handles the mouseclicks. Trigger a canvas clear and redraw only on group ref change for performance reasons
(defn handle-click [event]
  (let [canvas (.getElementById js/document "my-canvas")
        xCanvas (.-clientX event)
        yCanvas (.-clientY event)]
    ;; Setting the groupRef to nil because no group clicked on is a valid option. No outlines then
    (reset! groupRef nil)
    ;; Looping through the shapes to see if mouse click lands on any of them. Note that the circle shape is not perfectly taken into account, but rather a rectangle area around it, 
    ;; but this is I believe good enough for the assignment
    (doseq [shape shapes/shapes]
      (cond
        (= (:shape-type shape) :rect)
        (let [{:keys [x y width height group]} shape]
          (when (and (>= xCanvas (+ x xCorrection))
                     (<= xCanvas (+ x xCorrection width))
                     (>= yCanvas (+ y yCorrection))
                     (<= yCanvas (+ y yCorrection height)))
            (reset! groupRef group)))
        (= (:shape-type shape) :circle)
        (let [{:keys [x y radius group]} shape]
          (when (and (>= xCanvas (+ x xCorrection (- radius)))
                     (<= xCanvas (+ x xCorrection radius))
                     (>= yCanvas (+ y yCorrection (- radius)))
                     (<= yCanvas (+ y yCorrection radius)))
            (reset! groupRef group)))))
    ;; Only clear and redraw if the groupRef value has changed
    (when (not= @groupRef @previousGroupRef)
      ;; Clear the canvas
      (.clearRect (.getContext canvas "2d") 0 0 (.-width canvas) (.-height canvas))
    ;; Redraw everything
      (render-shapes))
    ;; Store the current groupRef value as the new previousGroupRef
    (reset! previousGroupRef @groupRef)))

(defn home-page []
   [:canvas {:id "my-canvas"
             :width (.-innerWidth js/window)
             :height 1000
             :on-click #(handle-click %)}])

;; -------------------------
;; Initialize app
;; Call the render-shapes function after the app is mounted
(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app"))
  (render-shapes))

(defn ^:export init! []
  (mount-root))
