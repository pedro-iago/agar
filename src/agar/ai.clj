(ns agar.ai
  (:refer-clojure :exclude [+ - * /])
  (:use [clojure.core.matrix.operators :only [+ - * /]])
  (:require [clojure.spec.alpha :as s]
            [clojure.core.matrix :as m]))

(s/def ::re (s/double-in :infinite? false :NaN? false))
(s/def ::im ::re)
(s/def ::complex (s/keys :req-un [::re ::im]))
(s/def ::circle (s/cat :position ::complex :radius ::re))
(s/def ::args (s/cat :you (s/spec ::circle) :him (s/spec ::circle)))
(s/def ::ret (s/or :eaten #{-1} :noop #{0} :eat #{1}))

;unit test
(def you (list {:re 3.0 :im 0.0} 4.00))
(def him (list {:re 0.0 :im 0.0} 5.01))

(defn capture-0 [[{xa :re ya :im} ra] [{xb :re yb :im} rb]]
  (let [Δ (m/distance [xa ya] [xb yb])]
    (if (> (+ ra rb) Δ)
      (compare ra rb)
      0)))

(defn capture-1 [[{xa :re ya :im} ra] [{xb :re yb :im} rb]]
  (let [Δ² (m/magnitude-squared (- [xa ya] [xb yb]))
        ∟r (* (- ra rb) (+ ra rb))]
    (cond
      (> ∟r Δ²) 1
      (> (- ∟r) Δ²) -1
      :else 0)))

(defn capture-2 [[{xa :re ya :im} ra] [{xb :re yb :im} rb]]
  (let [Δ (m/magnitude (- [xa ya] [xb yb]))
        Δr (- ra rb)]
    (cond
      (> Δr Δ) 1
      (> (- Δr) Δ) -1
      :else 0)))

(capture-0 you him)
(capture-1 you him)
(capture-2 you him)

;push programing
(require '[push.core :as push]
         '[push.type.definitions.complex :refer [->Complex map->Complex]]
         '[push.interpreter.core :as i]
         '[push.instructions.core :refer [build-instruction]]
         '[push.instructions.dsl :refer [consume-top-of calculate push-onto]])

;https://github.com/Vaguery/klapaucius
;https://github.com/lspector/Clojush/wiki
(filter #(re-matches #"code-(.*)" (subs (str %) 1))
        (push/known-instructions (push/interpreter)))

(def scalar-compare
  (build-instruction "scalar-compare"
    ":scalar-compare pops the top two scalar items and pushes -1 if the top item is less than the second, 1 if the top item is greater than the second, 0 otherwise."
    :tags #{:comparison}
    (consume-top-of :scalar :as :arg2)
    (consume-top-of :scalar :as :arg1)
    (calculate [:arg1 :arg2] #(compare %1 %2) :as :check)
    (push-onto :scalar :check)))

(def ^:dynamic instructions
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
    :code-quote
    -1
    0
    1
    2})

(def ^:dynamic interpreter
  (-> (push/interpreter)
      (i/register-instruction scalar-compare)))

(defn typed [instruction]
  (cond
    (seq? instruction) (map typed instruction)
    (vector? instruction) (mapv typed instruction)
    (s/valid? ::complex instruction) (map->Complex instruction)
    :else instruction))

(defn log
  ([exec] (log exec 100))
  ([exec count-limit]
   (-> interpreter
       (push/run (typed exec) count-limit)
       (push/get-stack :log))))

(defn λ
  ([program] (λ program 100))
  ([program count-limit]
   (fn [& args]
     (-> interpreter
         (push/run (concat (typed args) program) count-limit)
         (push/get-stack :return)
         first))))

;unit test
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

(capture-3 you him)
(capture-4 you him)
(capture-5 you him)

;clojure.spec (how to spec a program?)
(s/def ::program
  (s/coll-of (s/or :set instructions
                   :sub ::program)
             :kind list?))

(defn score [spec data] ;https://www.youtube.com/watch?v=xvk-Gnydn54
  (try
    (if-let [explain-data (s/explain-data spec data)]
      (->> explain-data
           ::s/problems
           (map #(apply + (:in %)))
           (apply min))
      (if (= ::s/invalid (s/conform spec data)) 0
        (->> spec
             s/form
             flatten
             (map #(if (set? %) (seq %) %))
             flatten
             count
             (- 100)
             (max (count data)))))
    (catch Throwable e 0)))

(def program-0ish ;shrink?
  (list
    0
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

(score ::program program-0)
(score ::program program-0ish)
((λ program-0ish) you him)

;math.combinatorics (permutations is a problem!)
(require '[clojure.math.combinatorics :as combo])

(combo/count-permutations program-0)
(combo/count-permutations program-1)
(combo/count-permutations program-2)

(combo/count-subsets program-0)
(combo/count-subsets program-1)
(combo/count-subsets program-2)

(count (flatten program-0))
(count (flatten program-1))
(count (flatten program-2))

(count program-0)
(count program-1)
(count program-2)

;test.check (how to shrink a program?)
(require '[clojure.spec.gen.alpha :as gen]
         '[clojure.spec.test.alpha :as test])

(comment
  (gen/quick-check 1000
    (gen/for-all* [(s/gen ::args)]
      #(= (apply capture-0 %)
          (apply capture-3 %))))

  (gen/quick-check 1000
    (gen/for-all* [(s/gen ::args)]
      #(= (apply capture-1 %)
          (apply capture-4 %))))

  (gen/quick-check 1000
    (gen/for-all* [(s/gen ::args)]
      #(= (apply capture-2 %)
          (apply capture-5 %))))

  (gen/quick-check 100
    (gen/for-all* [(s/gen ::program) (s/gen ::args)]
      #(= ((λ (concat %2 %1)))
          (apply (λ %1) %2)
          ((λ %1) %2)))))

;genetic programing (todo: restructure)
(require '[clojure.walk :refer [walk postwalk]]
         '[clojure.set :refer [union subset?]])

(def ^:dynamic seq-prob 0.3)
(def ^:dynamic sub-prob 0.5)

(defn rand-sub [coll]
  (->> (combo/count-subsets coll)
       rand
       m/floor
       (combo/nth-subset coll)
       seq))

(defn rand-seq [coll] ;slow
  (cond->> (postwalk #(if (seq? %) (cons `list %) %) (set coll))
           (< (rand) seq-prob)
           (list (rand-nth `[s/+ s/*]))
           (< (rand) sub-prob)
           (list `s/spec)))

(defn rand-cat [coll n]
  `(s/cat ~@(mapcat #(list (keyword (str %))
                           (rand-seq (rand-sub coll)))
                    (range n))))

(defn rand-spec [coll]
  (eval (rand-cat coll (count coll))))

;genetic process (todo: restructure)
(def ^:dynamic mutate-prob 0.2)
(def ^:dynamic crossover-prob 1)
(def ^:dynamic new-node-prob 0.05)

(defn mutable? [node]
  (or (s/valid? ::program))

  (or (when (seq? node)
        (contains? (union (set `[s/+ s/*]) #{'clojure.spec/spec})
                   (first node)))
      (set? node)))

(defn mutate [program]
  (postwalk #(if (and (mutable? %) (< (rand) mutate-prob))
               (rand-seq (rand-sub instructions))
               %)
            program))

(defn crossover [program1 program2] ;atom... ugh
  (let [chosen-node (first (walk #(when (and (mutable? %)
                                             (< (rand) crossover-prob))
                                    %)
                                 #(remove nil? %)
                                 program1))
        crossed-over? (atom false)]
    (if chosen-node
      (postwalk #(if (and (mutable? %)
                          (< (rand) crossover-prob)
                          (not @crossed-over?))
                   (do (reset! crossed-over? true) chosen-node)
                   %)
                program2)
      program2)))

;population of (s/keys :req-un [::spec-program ::score])
(defn initial-population [popsize max-cat-length]
  (for [i (range popsize)]
    {:program (rand-cat instructions (inc (rand-int max-cat-length)))}))

(defn select-best [population tournament-size] ;against itself?
  (let [selected (repeatedly tournament-size #(rand-nth population))]
    (-> (sort-by :score selected) reverse first)))

(defn perfect-fit [population]
  (first (filter #(= 100 (:score %)) population)))

(require '[clojure.pprint :refer [write]])

(defn evolve [pop-size max-gen tournament-size test-data]
  (spit "event.log" (str \newline \newline
                         ":Timestamp " (java.util.Date.)
                         " :seq-prob " seq-prob
                         " :sub-prob " sub-prob
                         " :mutate-prob " mutate-prob
                         " :crossover-prob " crossover-prob
                         " :new-node-prob " new-node-prob
                         " :pop-size " pop-size
                         " :max-gen " max-gen
                         " :tournament-size " tournament-size
                         \newline)
        :append true)
  (loop [n max-gen
         creatures (initial-population pop-size (count test-data))]
    (let [scored-creatures (map #(assoc % :score (score (eval (:program %)) test-data))
                                creatures)
          is-time-to-log (zero? (mod (- max-gen n) (quot max-gen 10)))]
     (if (or (zero? n) (perfect-fit scored-creatures))
       (let [winner (:program (second scored-creatures))
             result (s/conform (eval winner) test-data)]
         (spit "event.log" (str (write result :pretty true :stream nil) \newline) :append true)
         (spit "event.log" (write winner :pretty true :stream nil) :append true)
         scored-creatures)
       (let [elites (take 2 (reverse (sort-by :score scored-creatures)))
             new-creatures (for [i (range (- (count creatures) 2))]
                             ;; add a random node to improve diversity
                             (if (< (rand) new-node-prob)
                               {:program (rand-cat instructions (count test-data))}
                               (let [creature1 (select-best scored-creatures tournament-size)
                                     creature2 (select-best scored-creatures tournament-size)]
                                 (mutate (crossover creature1 creature2)))))]
         (when is-time-to-log
           (spit "event.log" (str ":generation " (- max-gen n) \newline
                                  ":best-scores " (mapv :score elites) \newline)
                 :append true))
         (recur (dec n) (vec (into new-creatures elites))))))))
