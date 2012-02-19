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

;; Here I set up a bunch of atoms to access various things.

;; The first two are important program state.

(def current-population (atom nil))
(def running (atom false))

;; The rest are just references to parts of the UI we need to talk to.
;; Let me know if you know of a better way of coordinating UI.

(def input-image (atom nil))
(def output-image (atom nil))
(def play-pause-button (atom nil))
(def info-panel-textarea (atom nil))

(defn update-interface
  "Tell our output image to repaint."
  []
  (.repaint @output-image))

(defn population-hook
  "This gets run for every new generation. We use it to save the
  population to an atom, update the info panel, and tell the
  output-image view to repaint."
  [new-pop]
  (reset! current-population new-pop)
  (text! @info-panel-textarea
         (let [best (-> new-pop :members first)]
             (str "Generation:\t" (-> new-pop :generation) "\n"
                  "Ages:\t" (join "," (map :age (:members new-pop))) "\n"
                  "Best Fitness:\t" (:fitness best) "\n"
                  "Best Length:\t" (count (:genes best)) "\n"
                  "Avg px Error:\t" (int (/ (:fitness best)
                                            (* (-> new-pop :env :width)
                                               (-> new-pop :env :height)))))))
  (update-interface))

(defn run-loop
  "The main run loop; just pulls generations off the infinite sequence
  of iterations. `doall` would have been adequate, except that we need
  a way to stop it when the pause button is hit, or a new run is
  started."
  []
  (loop [gens (run-population @current-population population-hook)]
    (when @running
      (first gens)
      (recur (rest gens)))))



(def inputs
  "Settings for the main parameters. Exposed in the UI."
  (atom {:render-width 150
         :output-width 1000
         :population-size 15
         :num-genes 400
         :max-age 15}))

(def spinner-settings
  "Spinner [from to step] settings for the settings' UI widgets."
  {:render-width [50 500 50]
   :output-width [100 5000 100]
   :population-size [6 50 1]
   :num-genes [50 5000 10]
   :max-age [3 1000 1]})

(defn update-input
  "When a setting is changed in the UI, store the value back to the
  `input` atom/map."
  [kw]
  (fn [e]
    (swap! inputs assoc-in [kw] (selection e))))

(defn widget-for-setting
  "Create a spinner widget for the given key, binding the
  `update-listener` to it."
  [setting-key]
  (let [[f t b] (setting-key spinner-settings)]
    (doto (spinner :model (spinner-model
                         (setting-key @inputs)
                         :from f :to t :by b))
        (listen :selection (update-input setting-key)))))

(defn start
  "Starts `run-loop` without touching `current-population` -- just
  resumes whatever run we're currently in the middle of."
  []
  (reset! running true)
  (config! @play-pause-button :text "Pause")
  (future-call run-loop))

(defn stop
  "Pauses the current run, without clearing any state."
  []
  (reset! running false)
  (config! @play-pause-button :text "Start"))


(defn play-pause
  "Toggles between running and paused."
  []
  (if @running (stop) (start)))


(defn get-scaled-buffered-image
  "A helper to scale the input image to the
  appropriate (`:render-width`) size, and convert the resulting (on
  OSX) `apple.awt.OSXImage` back to a `BufferedImage`."
  [file-handle]
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

(defn open
  "Choose an image from the file-browser, load it, and start the
  population running."
  []
  (stop)
  (when-let [file (choose-file :filters [["Images" ["png" "jpeg" "jpg"
                                                "gif" "tif" "tiff"]]])]
    (let
        [image (get-scaled-buffered-image file)
         width (.getWidth image)
         height (.getHeight image)
         env {:image image :pixels (image-pixels image)
              :width width :height height
              :max-age (:max-age @inputs)}]
      (stop)  ; stop any existing run

      ;; update the UI
      (config! [@input-image @output-image] :size [width :by height])
      (pack! (all-frames))

      ;; start the new population evolving.
      (reset! current-population (population
                                  #(polygonal-creature (:num-genes @inputs))
                                  env
                                  (:population-size @inputs)))
      (.repaint @input-image)
      (start))))


(defn save
  "Save the given creature to a PNG image, scaled
  to `:output-width`. Prompts for filename."
  [creature env]
  (when-let [fh (choose-file :type :save)]
    (let [image (:image (render-creature
                         creature {:width (:output-width @inputs)
                                   :height (* (/ (:output-width @inputs)
                                                 (:width env))
                                              (:height env))}))]
      (ImageIO/write image "png" fh))))

(defn save-current-best
  "Save the current fittest-in-population to an image. Calls `save`."
  []
  (when-let [popl @current-population]
    (save (first (:members popl)) (:env popl))))


(defn make-settings-panel
  "Lay out a panel for all the settings."
  []
  (grid-panel :columns 2 :vgap 5 :hgap 5
              :items [:fill-v :fill-v
                      "Render width:" (widget-for-setting :render-width)
                      "Output width:" (widget-for-setting :output-width)
                      "Population size:" (widget-for-setting :population-size)
                      "Number of genes:" (widget-for-setting :num-genes)
                      "Max age:" (widget-for-setting :max-age)
                      :fill-v :fill-v]))

;; More UI elements

(defn start-stop-button
  []
  (reset! play-pause-button
          (button :action (action :name "Start"
                                  :handler (fn [_] (play-pause))))))

(defn save-button []
  (button :action (action :name "Save" :handler (fn [_] (save-current-best)))))

(defn load-button []
  (button :action (action :name "Load" :handler (fn [_] (open)))))

(defn make-control-panel
  "The main control buttons for pause/load/save."
  []
  (flow-panel
   :items [(start-stop-button)
           (load-button)
           (save-button)]))

(defn draw-input-image
  "A handler for repainting the input image."
  [c g]
  (when-let [popl @current-population]
    (.drawImage g (:image (:env popl)) 0 0 nil)))

(defn input-panel []
  (let [p (seesaw.core/canvas
           :paint draw-input-image)]
    (reset! input-image p)
    p))

(defn draw-output-image
  "A handler for repainting the current best to output image."
  [c g]
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
                :margin 10
                :size [300 :by 100])))

(defn make-content-panel []
  (horizontal-panel
   :items [(grid-panel :columns 2 :vgap 10 :hgap 10
                       :items [(input-panel)
                               (output-panel)])
           (vertical-panel
                       :items [(make-settings-panel)
                               (make-control-panel)])
           (info-panel)]))

(defn -main
  "Crack open the main window."
  [& args]
  (-> (frame :title "Imogen" :content (make-content-panel) :on-close :exit)
      pack!
      show!))
