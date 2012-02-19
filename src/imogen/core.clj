(ns imogen.core
  "A stochastic hill-climber (I think.) A rough simulation of
evolutionary processes to match an image by overlaying a bunch of
polygons on each other.

<http://mattdw.github.com/experiments/image-evo/> is roughly the same
thing, in Javascript."
  (:use [imogen.genetic :only [population run-population Fitness]]
        [imogen.drawing :only [render-creature calculate-distance image-pixels]]
        [seesaw.core]
        [seesaw.chooser :only [choose-file]]
        [clojure.string :only [join]])
  (:import [imogen.genetic Creature]
           [javax.imageio ImageIO]
           [java.awt Image]
           [java.awt.image BufferedImage]))

(defn polygonal-creature
  "Creates an empty creature, but with a given number of prepopulated genes."
  ([] (polygonal-creature 200))
  ([length]
   (Creature. 0 0 (take length (repeatedly rand)))))

(extend Creature
  Fitness
  {:calculate-fitness calculate-distance
   :get-fitness :fitness})


(def current-population (atom nil))
(def running (atom false))
(def input-image (atom nil))
(def output-image (atom nil))
(def play-pause-button (atom nil))
(def info-panel-textarea (atom nil))

(defn update-interface []
  (.repaint @output-image))

(defn population-hook
  "This gets run for every new generation."
  [new-pop]
  (reset! current-population new-pop)
  (text! @info-panel-textarea
         (let [pop @current-population
               best (-> pop :members first)]
             (str "Generation:\t" (-> pop :generation) "\n"
                  "Ages:\t" (join "," (map :age (:members pop))) "\n"
                  "Best Fitness:\t" (:fitness best) "\n"
                  "Best Length:\t" (count (:genes best)) "\n"
                  "Avg px Error:\t" (int (/ (:fitness best)
                                            (* (-> pop :env :width)
                                               (-> pop :env :height)))))))
  (update-interface))

(defn run-loop
  []
  (loop [gens (run-population @current-population population-hook)]
    (when @running
      (first gens)
      (recur (rest gens)))))


;; params

(def inputs (atom {:render-width 150
                   :output-width 1000
                   :population-size 15
                   :num-genes 400
                   :max-age 15}))

(def spinner-settings
  {:render-width [50 500 50]
   :output-width [100 5000 100]
   :population-size [6 50 1]
   :num-genes [50 5000 10]
   :max-age [3 1000 1]})

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

(defn start []
  (reset! running true)
  (config! @play-pause-button :text "Pause")
  (future-call run-loop))

(defn stop []
  (reset! running false)
  (config! @play-pause-button :text "Start"))


(defn play-pause []
  (if @running (stop) (start)))

;; restart
;; load
;; save =

(defn get-scaled-buffered-image [file-handle]
  (let [in-image (-> (ImageIO/read file-handle)
                      (.getScaledInstance (:render-width @inputs)
                                          -1 Image/SCALE_DEFAULT))
        width (.getWidth in-image)
        height (.getHeight in-image)
        b-image (BufferedImage. width height
                                BufferedImage/TYPE_INT_ARGB)
        g (.getGraphics b-image)]
    (.drawImage g in-image 0 0 width height nil)
    (.dispose g)
    b-image))


;; to open
(defn open []
  (when-let [file (choose-file :filters [["Images" ["png" "jpeg" "jpg"
                                                "gif" "tif" "tiff"]]])]
    (let
        [image (get-scaled-buffered-image file)
         width (.getWidth image)
         height (.getHeight image)
         env {:image image :pixels (image-pixels image)
              :width width :height height
              :max-age (:max-age @inputs)}]
      (stop)

      (config! [@input-image @output-image] :size [width :by height])
      (pack! (all-frames))
      
      (reset! current-population (population
                                  #(polygonal-creature (:num-genes @inputs))
                                  env
                                  (:population-size @inputs)))
      (.repaint @input-image)
      (start))))


;; to save
(defn save [creature env]
  (when-let [fh (choose-file :type :save)]
    (let [image (:image (render-creature creature {:width (:output-width @inputs)
                                                   :height (* (/ (:output-width @inputs) (:width env))
                                                              (:height env))}))]
      (ImageIO/write image "png" fh))))

(defn save-current-best []
  (when-let [popl @current-population]
    (save (first (:members popl)) (:env popl))))


(defn make-settings-panel []
  (grid-panel :columns 2 :vgap 5 :hgap 5
              :items ["Render width:" (widget-for-setting :render-width)
                      "Output width:" (widget-for-setting :output-width)
                      "Population size:" (widget-for-setting :population-size)
                      "Number of genes:" (widget-for-setting :num-genes)
                      "Max age:" (widget-for-setting :max-age)]))


(defn start-stop-button []
  (reset! play-pause-button
          (button :action (action :name "Start"
                                  :handler (fn [_] (play-pause))))))

(defn save-button []
  (button :action (action :name "Save" :handler (fn [_] (save-current-best)))))

(defn load-button []
  (button :action (action :name "Load" :handler (fn [_] (open)))))

(defn make-control-panel []
  (flow-panel
   :items [(start-stop-button)
           (load-button)
           (save-button)]))

(defn draw-input-image [c g]
  (when-let [popl @current-population]
    (.drawImage g (:image (:env popl)) 0 0 nil)))

(defn input-panel []
  (let [p (seesaw.core/canvas
           :paint draw-input-image)]
    (reset! input-image p)
    p))

(defn draw-output-image [c g]
  (when-let [popl @current-population]
    (.drawImage g (:image (first (:members popl))) 0 0 nil)))

(defn output-panel []
  (let [p (seesaw.core/canvas
           :paint draw-output-image)]
    (reset! output-image p)
    p))

(defn info-panel []
  (reset! info-panel-textarea
          (text :multi-line? true
                :editable? false
                :margin 20
                :rows 20
                :size [250 :by 250])))

(defn make-content-panel []
  (horizontal-panel
   :items [(grid-panel :columns 2 :vgap 10 :hgap 10
                       :items [(input-panel)
                               (output-panel)])
           (vertical-panel
                       :items [(make-settings-panel)
                               (make-control-panel)])
           (info-panel)]))

(defn -main [& args]
  (-> (frame :title "Imogen" :content (make-content-panel) :on-close :exit)
      pack!
      show!))
