(ns imogen.genetic)

;; Let's start setting up some of the genetic stuff.

(defprotocol Evolving
  "A set of methods a creature will have to respond to to evolve. All
methods return either `this` or a new instance."
  (make-new [this] "Return an empty/default copy")
  (mutate [this] "Mutates a creature. This is where you'd add Spidey powers.")
  (calculate-fitness [this environment]
    "Calculate the creature's fitness (based on environment), and
    store it in e.g. the key :fitness, then return the creature.")
  (get-fitness [this]
    "Fetch the fitness")
  (cross-breed [this other] "Combine two creatures genes to create offspring"))


;; A few helpers, based on an assumption that genes will be a sequence
;; of floats between 0 and 1.

(defn alter-rand [xs f]
  "Use `f` to alter the value at a random index, replacing with 0, 1,
   or many values."
  (let [n (rand-int (count xs))
        [hs ts] (split-at n xs)]
    (concat hs (f (first ts)) (rest ts))))

(defn drop-rand [xs]
  "Drop the value at a random index."
  (alter-rand xs (fn [_] nil)))

(defn insert-rand [xs]
  "Insert a new value into the sequence."
  (alter-rand xs (fn [x] (if (nil? x) [(rand)] [(rand) x]))))

(defn append-rand [xs]
  "Append a new value to the end"
  (concat xs [(rand)]))

(defn change-rand [xs]
  "Swaps the value at a random index for a new rand."
  (alter-rand xs (fn [x] [(rand)])))

(def mutators
  "Default set of mutation operations."
  [drop-rand insert-rand append-rand change-rand])

;; Creature provides some reasonable default behaviours for some of
;; the Evolving methods.
(defrecord Creature
    [^int age ^int generation genes]

  Evolving
  (make-new [this] (assoc this
                :age 0
                :generation (inc (:generation this))
                :genes ()))
  (mutate [this] (update-in this [:genes] (rand-nth mutators)))
  (cross-breed [this other]
    (let [args [this other]
          genes (apply map #(rand-nth %&) (map :genes args))]
      (assoc (Creature. 0 0 ())
        :genes genes
        :generation (inc (apply max (map :generation args))))))
  (calculate-fitness [this env]
    (println this)
    ;; A sane default fitness isn't really possible, but nonetheless
    (assoc this :fitness (apply - 0 (seq (:genes this)))))
  (get-fitness [this]
    (:fitness this)))

;; Now we'll start on some population-level stuff; function that
;; manage a group of creatures.

(defrecord Population
    [env pop-size members])

(defn population [ctor env pop-size]
  (Population. env pop-size (take pop-size (repeatedly ctor))))

(defn cull
 "Kill off all but a certain number of the population. Defaults to keeping 1/3."
 ([pop]
    (cull pop (int (/ (:pop-size pop) 3))))
 ([pop keepn]
    (update-in pop [:members] #(take keepn %))))

(defn regenerate
  "Rebuild a (probably just-culled) population up to size"
  [{:keys [pop-size env members] :as pop}]
  (->>
   (concat (map #(update-in % [:age] inc) members)
           (take (- pop-size (count members))
                 (repeatedly
                  #(mutate
                    (cross-breed (first members)
                                 (first (shuffle (rest members))))))))
   (map #(calculate-fitness % env))
   (sort-by get-fitness)
   (assoc pop :members)))

(defn iterate-population
  "Run a single evaluate/cull/regenerate cycle"
  [{:keys [env members] :as pop} hook-func]
  (let [new-pop (-> pop
                    cull
                    regenerate)]
    (hook-func new-pop)
    new-pop))

(defn run-population
  "generate a lazy sequence of successive generations, calling hook-func for each."
  [start-pop hook-func]
  (iterate #(iterate-population % hook-func) start-pop))
