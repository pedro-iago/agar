(ns agar.ai
  (:refer-clojure :exclude [+ - * /])
  (:use [clojure.core.matrix.operators :only [+ - * /]])
  (:require [clojure.spec :as s]
            [clojure.core.matrix :as m]
            [push.core :as push]))

(s/def ::complex (s/map-of #{:re :im} number?))
(s/def ::circle (s/tuple ::complex number?))
(s/def ::capture
  (s/fspec :args (s/cat :you ::circle :him ::circle)
           :ret (s/or :eaten #{-1} :noop #{0} :ate #{1})))

;unit test
(def you [{:re 3 :im 0} 4.00])
(def him [{:re 0 :im 0} 5.01])

(defn capture-0 [[{xa :re ya :im} ra] [{xb :re yb :im} rb]]
  (let [Δ (m/distance [xa ya] [xb yb])]
    (if (> (+ ra rb) Δ)
      (compare ra rb)
      0)))

(defn capture-1 [[{xa :re ya :im} ra] [{xb :re yb :im} rb]] ;in-game
  (let [Δ² (m/magnitude-squared (- [xa ya] [xb yb]))
         ∟r (* (- ra rb) (+ ra rb))]
    (cond
      (> ∟r Δ²) 1
      (> (- ∟r) Δ²) -1
      :else 0)))

(defn capture-2 [[{xa :re ya :im} ra] [{xb :re yb :im} rb]]
  (let [[Δx Δy Δr] (- [xa ya ra] [xb yb rb])
         Δ (m/magnitude [Δx Δy])]
    (cond
      (> Δr Δ) 1
      (> (- Δr) Δ) -1
      :else 0)))

(capture-0 you him)
(capture-1 you him)
(capture-2 you him)

;push programing
(require '[push.type.definitions.complex :refer [->Complex map->Complex]]
         '[push.interpreter.core :as i]
         '[push.instructions.core :refer [build-instruction]]
         '[push.instructions.dsl :refer [consume-top-of calculate push-onto]])

;instructions search
;https://github.com/Vaguery/klapaucius
;https://github.com/lspector/Clojush/wiki

(count
  (sequence (filter #(re-matches #"scalar(.*)" (subs (str %) 1)))
          (push/known-instructions (push/interpreter))))

;interpreter
(def instructions
  #{:scalar-compare ;new
    :scalar-add
    :scalar-subtract
    :scalar-multiply
    :scalar-liftstack
    :scalar-return
    :scalar>?
    :scalar<?
    :complex-norm
    :complex-subtract
    :exec-if
    :code-do
    0})

(def scalar-compare
  (build-instruction "scalar-compare"
    ":scalar-compare pops the top two scalar items and pushes -1 if the top item is less than the second, 1 if the top item is greater than the second, 0 otherwise."
    :tags #{:comparison}
    (consume-top-of :scalar :as :arg2)
    (consume-top-of :scalar :as :arg1)
    (calculate [:arg1 :arg2] #(compare %1 %2) :as :check)
    (push-onto :scalar :check)))

(def interpreter
  (-> (push/interpreter :instructions instructions)
      (i/register-instruction scalar-compare)))

;unit test
(def args
  (concat (update you 0 map->Complex)
          (update him 0 map->Complex)))

(def program-0
  '(0
    :scalar-liftstacks
    :scalar-add
    :complex-subtract
    :complex-norm
    :scalar>?
    :exec-if
    :scalar-compare
    0
    :scalar-return))

(-> interpreter ;that's capture-0!
    (push/run (concat args program-0) 1000)
    (push/get-stack :return))

(def program-1
  '(0
    :scalar-liftstack
    :scalar-add
    :scalar->code
    :scalar-subtract
    :code-do
    :scalar-multiply
    :complex-subtract
    :complex-norm
    2
    :scalar-power
    0
    :scalar-liftstack
    :scalar>?
    :exec-if
    1
    '(:scalar-add 0 :scalar<? :exec-if -1 0)
    :scalar-return))

(-> interpreter ;that's capture-1!
    (push/run (concat args program-1) 1000)
    (push/get-stack :return))

;genetic programing (DRAFT)

;spec example (how to spec a program?)
(def spec (s/cat :0 int? :1 (s/alt :0 nil? :1 string?) :2 int?))
(def data [1 'foo 0])

(defn score [spec data] ;https://www.youtube.com/watch?v=xvk-Gnydn54
  (if-let [explain-data (s/explain-data spec data)]
    (-> explain-data
        ::s/problems
        first
        :path
        first
        str
        (subs 1)
        read-string)
    100))

(score spec data)

;see push data, try to spec
(sequence
  (map first)
  (s/exercise instructions 10))

(def program-0ish
  '(0
    :complex-subtract ;4 equivalent permutations
    :scalar-liftstack
    :scalar-add
    :complex-norm
    :scalar>?
    :exec-if
    :scalar-compare
    0
    :scalar-return))

;permutations
(require '[clojure.math.combinatorics :as combo])

(def run-push
  (comp (random-sample 0.001)
        (take 1000)
        (map #(push/run interpreter (concat args %) 1000))
        (map #(push/get-stack % :return))
        (map first)
        (filter #{-1 0 1})))

(defn fitness [program]
  (->> (combo/permutations program)
       (sequence run-push)
       frequencies))

(fitness program-0)

(fitness program-1)

;(defn random-program [instructions-set])
