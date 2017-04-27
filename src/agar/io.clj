(ns agar.io
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.spec :as s]
            [quil.core :as q]
            [quil.middleware :as qm]
            [clojure.core.matrix :as m])
  (:use [clojure.core.matrix.operators :only [+ - * /]]))

(derive ::start ::frozen)
(derive ::pause ::frozen)
(derive ::game-over ::frozen)
(s/def ::state #{::start ::active ::pause ::game-over})
(s/def ::transition #{[::start ::active]
                      [::active ::pause]
                      [::pause ::active]
                      [::active ::game-over]
                      [::game-over ::start]})

(def ^:const game-size 500)
(def ^:const screen-width 1000)
(def ^:const screen-height 1000)
(def ^:const screen-center (/ [screen-width screen-height] 2))
(def ^:const speed 50)
(def ^:const growth (/ 5))

(defn new-game []
  (let [size [game-size game-size]
        ai-count 20
        ai-diameter (repeatedly ai-count #(+ (rand 12) 10))
        ai-position (repeatedly ai-count #(mapv rand size))
        ai-color (repeatedly ai-count #(mapv rand [255 255 255]))]
    {:state ::start
     :count (+ 1 ai-count)
     :zoom 2
     :position (vec (cons (/ size 2) ai-position))
     :diameter (vec (cons 20 ai-diameter))
     :color (vec (cons [255 255 255] ai-color))
     :versor (m/zero-matrix (+ 1 ai-count) 2)}))

(defn soa->aos [game] ;https://www.youtube.com/watch?v=ZHqFrNyLlpA
  (reduce-kv #(mapv (fn [struct value] (assoc struct %2 value)) %1 %3)
             (repeat (:count game) {})
             (dissoc game :state :count :zoom)))

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
      (* (diameter %2)
         (/ distance-vector (+ distance² 1))
         (- (diameter %1) (diameter %2))))))

(defn check [{:keys [diameter] :as new} {:keys [versor] :as old}]
  (-> new
      ;assure rotation
      (assoc-in [:versor 0] (versor 0))
      ;walls
      (update :position m/clamp 0 game-size)
      ;state management
      (cond-> (<= (diameter 0) 0) (assoc :state ::game-over))))

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

(defn draw! [{:keys [state zoom] [player-position _] :position :as game}]
  ;zoom!
  (q/push-matrix)
  (q/translate (screen-center 0) (screen-center 1))
  (q/scale zoom)
  (q/translate (- (screen-center 0))
               (- (screen-center 1)))
  ;in-game!
  (q/with-translation (- screen-center player-position)
    ;grid!
    (q/background 30)
    (q/stroke 255)
    (doseq [bar (range 0 (+ game-size 0.1) (* screen-width 0.05))]
      (q/line bar 0 bar game-size)
      (q/line 0 bar game-size bar))
    ;cells!
    (doall (map cell! (sort-by :diameter (soa->aos game)))))
  ;screen!
  (q/pop-matrix)
  (when-not (= state ::active)
    (q/fill 0 255 0)
    (q/text-size 20)
    (q/text (str state) 5 20)
    ;helpers!
    (let [mouse [(q/mouse-x) (q/mouse-y)]]
      ;distance!
      (q/stroke 255 155 0)
      (q/line (screen-center 0) (screen-center 1) (mouse 0) (mouse 1))
      (q/text-size 20)
      (q/fill 255 155 0)
      (q/text (str (/ (m/distance mouse screen-center) zoom))
              5 (- screen-height 10)))))

(defn player-rotates [game {:keys [x y]}]
  (if (isa? (:state game) ::active)
    (assoc-in game [:versor 0] (m/normalise (- [x y] screen-center)))
    game))

(defn pause [game _]
  (if (= (:state game) ::active)
    (assoc game :state ::pause)
    game))

(defn unpause [game _]
  (if (= (:state game) ::pause)
    (assoc game :state ::active)
    game))

(defn io-handle [game {key :key-code}] ;todo: start-menu, spacebar leap
  (case (:state game)
        ::active (case key 10 (assoc game :state ::pause) game)
        ::pause (case key 10 (assoc game :state ::active) game)
        ::start (case key 10 (assoc game :state ::active) game)
        ::game-over (case key 10 (new-game)) game))

(defn cycle-state [game _] ;todo*: bound transition to certain action
  (let [transit-map (group-by first (s/form ::transition))
        transit-set (-> game :state transit-map set)]
    (if (= (count transit-set) 1)
      (assoc game :state (-> transit-set first second))
      game)))

(q/defsketch agar.io
  :size [screen-width screen-height]
  :setup new-game
  :update step
  :draw draw!
  :mouse-moved player-rotates
  :mouse-exited pause
  :mouse-entered unpause
  :key-pressed io-handle
  :mouse-pressed cycle-state
  :middleware [qm/fun-mode])
