(ns agar.io
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.spec :as s]
            [quil.core :as q]
            [quil.middleware :as qm]
            [clojure.core.matrix :as m])
  (:use [clojure.core.matrix.operators :only [+ - * /]]))

(derive ::start ::frozen)
(derive ::start ::spectation)
(derive ::pause ::frozen)
(derive ::game-over ::active)
(derive ::game-over ::spectation)
(s/def ::state #{::start ::active ::pause ::game-over})
(s/def ::transition #{[::start ::active] ;http://www.61131.com/sfchowto.htm
                      [::active ::pause]
                      [::pause ::active]
                      [::active ::game-over]
                      [::game-over ::start]})

(def ^:const game-size 300)
(def ^:const screen-width 1000)
(def ^:const screen-height 1000)
(def ^:const screen-center (/ [screen-width screen-height] 2))
(def ^:const speed 50)
(def ^:const growth (/ 5))
(def ^:const eps 10)

;todo: read this from a file
(defn new-game []
  (let [size [game-size game-size]
        ;counts
        food-count 10
        virus-count 2
        ai-count 5
        total-count (+ food-count virus-count ai-count 1)
        ;diameters
        food-diameter (repeat food-count 10)
        virus-diameter (repeat virus-count -25)
        ai-diameter (repeatedly ai-count #(+ (rand 20) 10))
        ;colors
        food-color (repeat food-count [255 255 0])
        virus-color (repeat virus-count [0 255 0])
        ai-color (repeatedly ai-count #(mapv rand [255 0 255]))
        ;all but player
        rest-position (repeatedly (dec total-count) #(mapv rand size))
        rest-diameter (concat ai-diameter food-diameter virus-diameter)
        rest-color (concat ai-color food-color virus-color)]
    {:state ::start
     :count total-count
     :zoom 2
     :view (cycle (range total-count))
     :position (vec (cons (/ size 2) rest-position))
     :diameter (vec (cons 20 rest-diameter))
     :color (vec (cons [255 255 255] rest-color))
     :versor (m/zero-matrix total-count 2)}))

(defn soa->aos [game] ;https://www.youtube.com/watch?v=ZHqFrNyLlpA
  (reduce-kv #(mapv (fn [struct value] (assoc struct %2 value)) %1 %3)
             (repeat (:count game) {})
             (dissoc game :state :count :zoom :view)))

(defn growth-matrix [{:keys [position diameter count]}]
  (m/compute-matrix [count count]
    #(let [distance² (m/magnitude-squared (- (position %2) (position %1)))
           coverage (* (- (diameter %2) (diameter %1))
                       (+ (diameter %2) (diameter %1))
                       0.25)]
      (cond
        (> coverage distance²) (- (diameter %1))
        (> (- coverage) distance²) (* (diameter %2) growth)
        :else 0))))

(defn error-matrix [{:keys [position diameter count]}]
  (m/compute-matrix [count count]
    #(let [distance-vector (- (position %2) (position %1))
           distance² (m/magnitude-squared distance-vector)]
      (+ (* (cond-> (diameter %2) (neg? (diameter %2)) (/ eps))
            (- (diameter %1) (diameter %2))
            (/ distance-vector (+ distance² 1) 100))
         (/ (- [(rand) (rand)] 0.5) 1000)))))

(defn check [{:keys [diameter] :as new} {:keys [versor] [player] :view :as old}]
  (let [active (mapv #(if (> % eps) 1 0) diameter)]
    (-> new
        ;walls
        (update :position m/clamp 0 game-size)
        ;state management
        (cond->
          ;assure rotation
          (= (:state old) ::active)
          (assoc-in [:versor player] (versor player))
          ;game-over clause
          (or (<= (diameter player) eps) (= (reduce + active) 1))
          (assoc :state ::game-over))
        ;freeze zeros
        (update :versor #(mapv * %1 %2) active))))

(defn step [{:keys [state position diameter versor] :as game}]
  (if (isa? state ::active)
    (-> game
        ;training
        ;ai-rotates
        (assoc :versor (mapv #(m/normalise (reduce + %)) (error-matrix game)))
        ;movement
        (update :position + (mapv * (/ speed (+ diameter 1)) versor))
        ;resize
        (update :diameter + (mapv #(reduce + %) (growth-matrix game)))
        ;validate
        (check game))
    game))

(defn cell! [{[x y] :position d :diameter c :color}]
  (q/stroke 255 255 0)
  (apply q/fill c)
  (q/ellipse x y d d))

(defn draw! [{:keys [state zoom view position] :as game}]
  ;zoom!
  (q/push-matrix)
  (q/translate (screen-center 0) (screen-center 1))
  (q/scale zoom)
  (q/translate (- (screen-center 0))
               (- (screen-center 1)))
  ;in-game!
  (q/with-translation (- screen-center (-> view first position))
    ;grid!
    (q/background 30)
    (q/stroke 255)
    (doseq [bar (range 0 (+ game-size eps) (* screen-width 0.05))]
      (q/line bar 0 bar game-size)
      (q/line 0 bar game-size bar))
    ;cells!
    (doall (map cell! (sort-by :diameter (soa->aos game)))))
  (q/pop-matrix)
  ;status!
  (when-not (= state ::active)
    (q/fill 0 255 0)
    (q/text-size 20)
    (q/text (str state) 5 20))
  ;helpers!
  (when (isa? state ::spectation)
    (let [mouse [(q/mouse-x) (q/mouse-y)]]
      ;distance!
      (q/stroke 255 155 0)
      (q/line (screen-center 0) (screen-center 1) (mouse 0) (mouse 1))
      (q/text-size 20)
      (q/fill 255 155 0)
      (q/text (str (/ (m/distance mouse screen-center) zoom))
              5 (- screen-height 10)))))

(defn player-rotates [{[player] :view :as game} {:keys [x y]}]
  (if (= (:state game) ::active)
    (assoc-in game [:versor player] (m/normalise (- [x y] screen-center)))
    game))

(defn pause [game _]
  (if (= (:state game) ::active)
    (assoc game :state ::pause)
    game))

(defn unpause [game _]
  (if (= (:state game) ::pause)
    (assoc game :state ::active)
    game))

(defn switch-player
  ([{:keys [diameter] [player] :view :as game}]
   (if (<= (diameter player) eps)
     (recur (update game :view next))
     game))
  ([game _]
   (if (isa? (:state game) ::spectation)
     (switch-player (update game :view next))
     game)))

(defn zoom-wheel [game spin]
  (if (isa? (:state game) ::spectation)
    (update game :zoom + (* spin 0.1))
    game))

(defn key-handle [game {key :key-code}] ;todo: start-menu, spacebar leap
  (case (:state game)
        ::active (case key 10 (assoc game :state ::game-over) game)
        ::pause (case key 10 (assoc game :state ::active) game)
        ::start (case key 10 (assoc game :state ::active) game)
        ::game-over (case key 10 (new-game)) game))

(q/defsketch agar.io
  :size [screen-width screen-height]
  :setup new-game
  :update step
  :draw draw!
  :mouse-moved player-rotates
  :mouse-exited pause
  :mouse-entered unpause
  :mouse-pressed switch-player
  :mouse-wheel zoom-wheel
  :key-pressed key-handle
  :middleware [qm/pause-on-error qm/fun-mode])
