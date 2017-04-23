(ns agar.io
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.spec :as s]
            [quil.core :as q]
            [quil.middleware :as qm]
            [clojure.core.matrix :as m])
  (:use [clojure.core.matrix.operators :only [+ - * /]]))

(derive ::start ::frozen)
(derive ::pause ::frozen)
(derive ::game-over ::active)
(s/def ::state #{::start ::active ::pause ::game-over})
(s/def ::transition #{[::start ::active]
                      [::active ::pause]
                      [::pause ::active]
                      [::active ::game-over]
                      [::game-over ::start]})

(def ^:const width 500)
(def ^:const height 500)
(def ^:const center (/ [width height] 2))
(def ^:const speed 2)

(defn new-game []
  (let [size [2000 2000]
        ai# 200
        ai-diameter (repeatedly ai# #(+ (rand 40) 5))
        ai-position (repeatedly ai# #(mapv rand size))
        ai-color (repeatedly ai# #(mapv rand [255 255 255]))]
    {:state ::start
     :count (+ 1 ai#)
     :position (vec (cons [0 0] ai-position))
     :diameter (vec (cons 30 ai-diameter))
     :color (vec (cons [255 255 255] ai-color))
     :versor (m/zero-matrix (+ 1 ai#) 2)}))

;struct of arrays to array of structs (may be slow)
;soa is easier to update, and aos is easier to draw
(defn soa->aos [game] ;https://www.youtube.com/watch?v=ZHqFrNyLlpA
  (reduce-kv #(mapv (fn [s v] (assoc s %2 v)) %1 %3)
             (repeat (:count game) {})
             (dissoc game :state :count)))

(defn step [{:keys [state position diameter versor] :as game}] ;todo, bugs
  (if (isa? state ::active)
    (let [distance (map m/magnitude (- position (position 0)))]
      (as-> game at
        ;training
        ;ai-rotates
        ;movement
        (update at :position + (* speed versor))
        ;score
        (if (some zero? (m/ge distance (/ (- diameter (diameter 0)) 2)))
          (assoc at :state ::game-over :versor (assoc versor 0 [0 0]))
          at)))
    game))

(defn cell! [{[x y] :position d :diameter c :color}]
  (q/stroke 255 255 0)
  (apply q/fill c)
  (q/ellipse x y d d))

(defn draw! [{:keys [state position] :as game}]
  (q/background 30)
  ;cells!
  (q/with-translation (- center (position 0))
    (doall (map cell! (sort-by :diameter (soa->aos game)))))
  ;screen!
  (when-not (= state ::active)
    (q/fill 0 255 0)
    (q/text-size 20)
    (q/text (str state) 5 20)))

(defn player-rotates [game {:keys [x y]}]
  (if (= (:state game) ::active)
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
        ::game-over (case key 10 (assoc game :state ::start) game)))

(q/defsketch agar.io
  :size [width height]
  :setup new-game
  :update step
  :draw draw!
  :mouse-moved player-rotates
  :mouse-exited pause
  :mouse-entered unpause
  :key-pressed io-handle
  :middleware [qm/fun-mode])
