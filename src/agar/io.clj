(ns agar.io
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.spec :as s]
            [quil.core :as q]
            [quil.middleware :as qm]
            [clojure.core.matrix :as m])
  (:use [clojure.core.matrix.operators :only [+ - * /]]))

(s/def ::state #{:start :active :pause :game-over})
(s/def ::transition #{[:start :active]
                      [:active :pause]
                      [:pause :active]
                      [:active :game-over]
                      [:game-over :start]})

(def ^:const width 2000)
(def ^:const height 2000)
(def ^:const screen-size (/ [width height] 4))
(def ^:const center (/ screen-size 2))
(def ^:const ai# 250)

(defn new-game []
  (let [ai-positions (repeatedly ai# (fn [] [(rand width) (rand height)]))]
    {:state :start
     :diameters (vec (cons 30 (repeatedly ai# #(+ (rand 25) 5))))
     :positions (vec (cons [0 0] ai-positions))
     :versors (vec (repeat (+ 1 ai#) [0 0]))
     :speeds (vec (repeat (+ 1 ai#) 2))
     :colors (vec (repeatedly (+ 1 ai#) (fn [] (mapv rand [255 255 255]))))}))

(defn step [{:keys [state versors speeds] :as game}]
  (if-not (= state :active) game
    (-> game
      ;train
      ;ai-rotates
      ;movement
      (update :positions + (map * speeds versors)))))
      ;score

(defn draw! [{:keys [state colors positions diameters] :as game}]
  (q/with-translation (- center (first positions))
    ;background!
    (q/background 231)
    ;cells!
    (doall
      (map #(do (apply q/fill %1) (q/ellipse (get %2 0) (get %2 1) %3 %3))
           colors positions diameters)))
    ;screen!
  (when-not (= state :active)
    ;(q/background 100 100 100 0.1)
    (q/fill 0)
    (q/text-size 20)
    (q/text (str state) (center 0) (center 1))))

(defn player-rotates [game {:keys [x y]}]
  (case (:state game)
        :active (assoc-in game [:versors 0] (m/normalise (- [x y] center)))
        game))

(defn pause [game _]
  (case (:state game)
        :active (assoc game :state :pause)
        game))

(defn unpause [game _]
  (case (:state game)
        :pause (assoc game :state :active)
        game))

(defn io-handle [game {key :key-code}] ;todo: start-menu, spacebar
  (case (:state game)
        :active (case key 10 (assoc game :state :pause) game)
        :pause (case key 10 (assoc game :state :active) game)
        :start (case key 10 (assoc game :state :active) game)
        :game-over (case key 10 (assoc game :state :start) game)))

(q/defsketch agar.io
  :size screen-size
  :setup new-game
  :update step
  :draw draw!
  :mouse-moved player-rotates
  :mouse-exited pause
  :mouse-entered unpause
  :key-pressed io-handle
  :mouse-pressed (fn [game _] (println game) game)
  :middleware [qm/fun-mode])
