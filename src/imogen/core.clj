(ns imogen.core
  "A stochastic hill-climber (I think.) A rough simulation of
evolutionary processes to match an image by overlaying a bunch of
polygons on each other.

<http://mattdw.github.com/experiments/image-evo/> is roughly the same
thing, in Javascript."
  (:use [imogen.genetic :only [population run-population Fitness]]
        [imogen.drawing :only [render-creature calculate-distance image-pixels]]
        [seesaw.core]
        [seesaw.chooser :only [choose-file]])
  (:import [imogen.genetic Creature]
           [javax.imageio ImageIO]
           [java.awt Image]))

(population #(Creature. 0 0 []) {} 10)

(defn polygonal-creature
  "Creates an empty creature, but with a given number of prepopulated genes."
  ([] (polygonal-creature 200))
  ([length]
   (Creature. 0 0 (take length (repeatedly rand)))))

(defn starting-population
  []
  (population polygonal-creature "env"))

(extend Creature
  Fitness
  {:calculate-fitness calculate-distance}
  {:get-fitness :fitness})


(def current-population (atom starting-population))
(def running (atom false))

(defn update-interface [])

(defn population-hook
  "This gets run for every new generation."
  [new-pop]
  (reset! population new-pop)
  (update-interface))

(defn run-loop
  []
  (future
   (when @running
     (run-population (starting-population) population-hook)
     (recur))))


;; params

(def inputs (atom {:render-width 200
                   :output-width 800
                   :population-size 10}))

(def spinner-settings
  {:render-width [50 500 50]
   :output-width [100 5000 100]
   :population-size [6 50 1]})

(defn update-input
  [kw]
  (fn [e]
    (swap! inputs assoc-in [kw] (selection e))))

(defn widget-for-setting
  [setting-key]
  (let [[f t b] (setting-key spinner-settings)]
    (doto (spinner :model (spinner-model
                         (setting-key @inputs)
                         :from f :to t :by b))
        (listen :selection (update-input setting-key)))))

;; commands

;; play/pause
;; restart
;; load
;; save =


;; to open
(defn open []
  (let
      [file (choose-file :filters [["Images" ["png" "jpeg" "jpg" "gif" "tif" "tiff"]]])
       image (-> (ImageIO/read file)
                 (.getScaledInstance (:render-width @inputs) -1 Image/SCALE_DEFAULT))]
    {:image image :pixels (image-pixels image) :width (.getWidth image) :height (.getHeight image)}))


;; to save
(defn save [image]
  #_(render-creature creature {:width _ :height _})
  (ImageIO/write image "png"
                 (choose-file :type :save
                              :success-fn #(if (.endsWith (.getAbsolutePath %) ".png")
                                             %
                                             (str (.getAbsolutePath %) ".png")))))

(defn make-control-panel []
  (grid-panel :columns 2 :vgap 5 :hgap 5
              :items ["Render width:" (widget-for-setting :render-width)
                      "Output width:" (widget-for-setting :output-width)
                      "Population size:" (widget-for-setting :population-size)
                      :fill-v :fill-v]))

(defn make-content-panel []
  (grid-panel :columns 3 :vgap 10 :hgap 10
              :items ["Input" "Output" (make-control-panel)]))

(defn -main [& args]
  (-> (frame :title "Imogen" :content (make-content-panel) :on-close :exit)
      pack!
      show!))
