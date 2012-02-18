(ns imogen.drawing
  "Drawing code for our particular creatures."
  (:import [java.awt.image BufferedImage]
           [java.awt Color Graphics2D Graphics RenderingHints]))

;; we'll be interpreting all of a creature's gene here, so we'll need
;; some helpers

(defn float-to-num-vertices
  "Converts a float 0-1 to an int number of vertices. This will give us an int between 3 and 9."
  [x]
  (int (+ 3 (* x 6))))

(defn next-gene-set
  "A polygon is just a sequence of floats 0-1. The first four make up
  the colour, and the fifth tells us how many x,y pairs to consume
  from `ts`. We return a pair of [consumed, unconsumed]."
  [[r g b a vs & ts]]
  (when vs
   (let [num-verts (float-to-num-vertices vs)
         [xs_ys remaining] (split-at (* 2 num-verts) ts)]
     ;; we only return remaining if there's enough for another poly,
     ;; which I take to mean color def (4), num verts (1), and then 3
     ;; x,y pairs.
     (when (>= (count xs_ys) 6)
       [{:color [r g b a] :coords (apply map vector (partition 2 xs_ys))} remaining]))))

(defn unfoldr
  "Unfoldr is roughly the opposite of reduce. It takes a seed and a
  function and produces a sequence. `f` should return a pair of
  consumed/unconsumed, or nil to signal that we're done."
  [f seed]
  (lazy-seq
   (let [res (f seed)]
     (when (not (nil? res))
       (cons (first res) (unfoldr f (second res)))))))

;; Therefore:

(defn polygons
  "Turn a sequences of floats (genes) into a sequence of
  `{:color :coords}` polygon definitions ready for drawing."
  [genes]
  (unfoldr next-gene-set genes))

;; Now for the nitty-gritty Java-interop drawing code.

(defn make-color
  "A wrapper over java.awt.Color"
  [r g b a]
  (Color. (float r) (float g) (float b) (float a)))

(defn draw-polygon
  "Draw the polygon in the obvious manner. We draw to a provided Graphics2D."
  [{:keys [color coords]} {:keys [graphics width height]}]
  (let [c (apply make-color color)]
    (doto graphics
      (.setColor c)
      (.fillPolygon (int-array (map #(* width %) (first coords)))
                    (int-array (map #(* height %) (second coords)))
                    (count (first coords))))))

(defn draw-genes
  "Creates a new image according to `env` and renders polygons into it."
  [genes {:keys [width height]}]
  (let [image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        graphics (.createGraphics image)
        render-env {:graphics graphics :width width :height height}]
    (doto graphics
      (.setRenderingHint RenderingHints/KEY_ANTIALIASING
                         RenderingHints/VALUE_ANTIALIAS_ON)
      (.setColor Color/white)
      (.fillRect 0 0 width height))
    (doseq [p (polygons genes)]
      (draw-polygon p render-env))
    (.dispose graphics)
    image))

;; Color-difference code cribbed from an earlier version, hopefully
;; pretty fast.

(defn subtract-and-shift
  [a b mask shift]
  (Math/abs (int (bit-shift-right (- (bit-and a mask) (bit-and b mask)) shift))))

(defn color-dist
  [a b]
  (let [a (int a)
        b (int b)
        dr (subtract-and-shift a b 0xff0000 16)
        dg (subtract-and-shift a b 0xff00 8)
        db (subtract-and-shift a b 0xff 0)]
    (int (+ dr (+ dg db)))))

(defn image-pixels
  "Return an array of TYPE_INT_RGB"
  [#^BufferedImage img]
  (let [width (.getWidth img)
        height (.getHeight img)]
    (.getRGB img 0 0 width height nil 0 width)))

(defn image-distance
  [image1 image2]
  (reduce + 0 (map color-dist (image-pixels image1) (image-pixels image2))))

;; High-level API

(defn render-creature
  "Takes a creature and an environment and returns a creature with an
  :image field."
  [creature environment]
  (assoc creature :image (draw-genes (:genes creature) environment)))

(defn calculate-distance
  "calculate the distance of a creature's :image from the env's :image"
  [creature env]
  (let [new-creature (if (:image creature) creature (render-creature creature))]
    (assoc creature :fitness (image-distance (:image creature) (:image env)))))
