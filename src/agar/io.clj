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
(def ^:const width 1000)
(def ^:const height 1000)
(def ^:const center (/ [width height] 2))
(def ^:const speed 50)
(def ^:const growth (/ 5))

(defn new-game []
  (let [size [game-size game-size]
        ai# 25
        ai-diameter (repeatedly ai# #(+ (rand 12) 10))
        ai-position (repeatedly ai# #(mapv rand size))
        ai-color (repeatedly ai# #(mapv rand [255 255 255]))]
    {:state ::start
     :count (+ 1 ai#)
     :position (vec (cons (/ size 2) ai-position))
     :diameter (vec (cons 20 ai-diameter))
     :color (vec (cons [255 255 255] ai-color))
     :versor (m/zero-matrix (+ 1 ai#) 2)}))

(defn soa->aos [game] ;https://www.youtube.com/watch?v=ZHqFrNyLlpA
  (reduce-kv #(mapv (fn [s v] (assoc s %2 v)) %1 %3)
             (repeat (:count game) {})
             (dissoc game :state :count)))

(defn compare-cell [px dx py dy] ;todo: reuse error matrix
  (let [distance² (m/magnitude-squared (- px py))
        metrics (* (- dy dx) (+ dy dx) 0.25)]
    (cond
      (> metrics distance²) (- dx)
      (> (- metrics) distance²) (* dy growth)
      :else 0)))

(defn comparison-matrix [{p :position d :diameter n :count}] ;slow, bugs
  (m/compute-matrix [n n] #(compare-cell (p %1) (d %1) (p %2) (d %2))))

(defn error-matrix [{p :position d :diameter n :count}] ;slow (neat)
  (m/compute-matrix [n n] #(* (- (p %2) (p %1)) (- (d %1) (d %2)))))

(defn lnz [v1 v2] ;wtf
  (if (and (< (m/magnitude v2) (m/magnitude v1)) (> (m/magnitude v2) 0)) v2 v1))

(defn closest-food [{d :diameter :as game}] ;bugs
  (mapv (fn [r] (m/normalise (reduce lnz (map * d r)))) (error-matrix game)))

(defn game-check [{:keys [diameter] :as game}] ;order matters
  (cond-> game (<= (diameter 0) 0) (assoc :state ::game-over)))

(defn step [{:keys [state position diameter versor] :as game}] ;frame dependent!
  (if (isa? state ::active)
    (-> game
      ;training
      ;ai-rotates
      (update :versor #(assoc %2 0 (nth %1 0)) (closest-food game))
      ;movement
      (update :position + (mapv * (/ speed (+ diameter 1)) versor))
      (update :position m/clamp 0 game-size)
      ;resize
      (update :diameter + (mapv #(reduce + %) (comparison-matrix game)))
      game-check)
    game))

(defn cell! [{[x y] :position d :diameter c :color}]
  (q/stroke 255 255 0)
  (apply q/fill c)
  (q/ellipse x y d d))

(defn draw! [{:keys [state position] :as game}]
  (q/with-translation (- center (position 0))
    ;grid
    (q/background 30)
    (q/stroke 255)
    (doseq [l (range 0 (+ game-size 0.1) (* width 0.05))]
      (q/line l 0 l game-size)
      (q/line 0 l game-size l))
    ;cells!
    (doall (map cell! (sort-by :diameter (soa->aos game)))))
  ;screen!
  (when-not (= state ::active)
    (q/fill 0 255 0)
    (q/text-size 20)
    (q/text (str state) 5 20)))

(defn player-rotates [game {:keys [x y]}]
  (if (isa? (:state game) ::active)
    (assoc-in game [:versor 0] (m/normalise (- [x y] center)))
    game))

(defn pause [game _]
  (if (= (:state game) ::active)
    (assoc game :state ::pause)
    game))

(defn unpause [game _]
  (if (= (:state game) ::pause)
    (assoc game :state ::active)
    game))

(defn io-handle [game {key :key-code}] ;todo: start-menu, spacebar action
  (case (:state game)
        ::active (case key 10 (assoc game :state ::pause) game)
        ::pause (case key 10 (assoc game :state ::active) game)
        ::start (case key 10 (assoc game :state ::active) game)
        ::game-over (case key 10 (new-game)) game))

(q/defsketch agar.io
  :size [width height]
  :setup new-game
  :update step
  :draw draw!
  :mouse-moved player-rotates
  :mouse-exited pause
  :mouse-entered unpause
  :key-pressed io-handle
  :mouse-pressed (fn [game _] (assoc game :state ::game-over))
  :middleware [qm/fun-mode])
