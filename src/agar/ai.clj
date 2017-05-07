(ns agar.ai
  (:refer-clojure :exclude [+ - * /])
  (:use [clojure.core.matrix.operators :only [+ - * /]])
  (:require [clojure.spec :as s]
            [clojure.core.matrix :as m]))

(s/def ::re (s/double-in :infinite? false :NaN? false))
(s/def ::im ::re)
(s/def ::complex (s/keys :req-un [::re ::im]))
(s/def ::circle (s/cat :position ::complex :radius ::re))
(s/def ::args (s/cat :you ::circle :him ::circle))
(s/def ::ret (s/or :eaten #{-1} :noop #{0} :eat #{1}))

;unit test
(def you [{:re 3.0 :im 0.0} 4.00])
(def him [{:re 0.0 :im 0.0} 5.01])

(defn capture-0 [{xa :re ya :im} ra {xb :re yb :im} rb]
  (let [Δ (m/distance [xa ya] [xb yb])]
    (if (> (+ ra rb) Δ)
      (compare ra rb)
      0)))

(defn capture-1 [{xa :re ya :im} ra {xb :re yb :im} rb]
  (let [Δ² (m/magnitude-squared (- [xa ya] [xb yb]))
        ∟r (* (- ra rb) (+ ra rb))]
    (cond
      (> ∟r Δ²) 1
      (> (- ∟r) Δ²) -1
      :else 0)))

(defn capture-2 [{xa :re ya :im} ra {xb :re yb :im} rb]
  (let [Δ (m/magnitude (- [xa ya] [xb yb]))
        Δr (- ra rb)]
    (cond
      (> Δr Δ) 1
      (> (- Δr) Δ) -1
      :else 0)))

(apply capture-0 (concat you him))
(apply capture-1 (concat you him))
(apply capture-2 (concat you him))

;push programing
(require '[push.core :as push]
         '[push.type.definitions.complex :refer [->Complex map->Complex]]
         '[push.interpreter.core :as i]
         '[push.instructions.core :refer [build-instruction]]
         '[push.instructions.dsl :refer [consume-top-of calculate push-onto]])

;https://github.com/Vaguery/klapaucius
;https://github.com/lspector/Clojush/wiki
(filter #(re-matches #"scalar(.*)" (subs (str %) 1))
        (push/known-instructions (push/interpreter)))

(def instructions
  #{:scalar-compare ;new
    :scalar-add
    :scalar-subtract
    :scalar-multiply
    :scalar-liftstack
    :scalar-return
    :scalar>?
    :scalar<?
    :scalar-power
    :scalar->code
    :complex-norm
    :complex-subtract
    :exec-if
    :code-do
    -1
    0
    1
    2})

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

(defn log [& exec]
  (-> interpreter
      (push/run exec 100)
      (push/get-stack :log)))

(defn λ [program]
  (fn [& args]
    (-> interpreter
        (push/run (concat args program) 100)
        (push/get-stack :return)
        first)))

(defn typed [instruction]
  (cond
    (s/valid? ::complex instruction)
    (map->Complex instruction)
    :else instruction))

;unit test
(def args (map typed (concat you him)))

(def program-0
  (list
    0
    :scalar-liftstack
    :scalar-add
    :complex-subtract
    :complex-norm
    :scalar>?
    :exec-if
    :scalar-compare
    0
    :scalar-return))

(def program-1
  (list
    0
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
    (list
      :scalar-add
      0
      :scalar<?
      :exec-if
      -1
      0)
    :scalar-return))

(def program-2
  (list
    :scalar-subtract
    :complex-subtract
    :complex-norm
    0
    :scalar-liftstack
    :scalar>?
    :exec-if
    1
    (list
      :scalar-add
      0
      :scalar<?
      :exec-if
      -1
      0)
    :scalar-return))

(def capture-3 (λ program-0))
(def capture-4 (λ program-1))
(def capture-5 (λ program-2))

(apply capture-3 args)
(apply capture-4 args)
(apply capture-5 args)

;clojure.spec (how to spec a program?)
(s/def ::program
  (s/coll-of (s/or :list ::program
                   :flat instructions)
             :gen-max 5))

(defn score [spec data] ;https://www.youtube.com/watch?v=xvk-Gnydn54
  (if-let [explain-data (s/explain-data spec data)]
    (->> explain-data
         ::s/problems
         (map #(apply + (:in %)))
         (apply max))
    100))

(score ::program program-0)
(score ::program program-1)
(score ::program program-2)

(def program-0ish ;shrink?
  '(0
    :complex-subtract
    :scalar-liftstack
    :scalar-add
    :complex-add
    :complex-norm
    :scalar>?
    :exec-if
    :scalar-compare
    0
    :complex-add
    :scalar-return))

(get instructions :complex-add)
(score ::program program-0ish)
(apply (λ program-0ish) args)

;permutations
(require '[clojure.math.combinatorics :as combo])

(combo/count-permutations program-0)
(combo/count-permutations program-1)
(combo/count-permutations program-2)

(combo/count-subsets program-0)
(combo/count-subsets program-1)
(combo/count-subsets program-2)

(count program-0)
(count program-1)
(count program-2)

;test.check (how to shrink a program?)
(require '[clojure.spec.gen :as gen]
         '[clojure.spec.test :as test])

(gen/quick-check 1000
  (gen/for-all* [(s/gen ::args)]
    #(= (apply capture-0 (map typed %))
        (apply capture-3 (map typed %)))))

(gen/quick-check 1000
  (gen/for-all* [(s/gen ::args)]
    #(= (apply capture-1 (map typed %))
        (apply capture-4 (map typed %)))))

(gen/quick-check 1000
  (gen/for-all* [(s/gen ::args)]
    #(= (apply capture-2 (map typed %))
        (apply capture-5 (map typed %)))))
