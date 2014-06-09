(ns game-utils.helpers)

(defn now []
  (System/currentTimeMillis))

(defn pairs [col]
  (partition 2 col))

(defn triplets [col]
  (partition 3 col))

(defn lerp [a b t]
  (+ a (* t (- b a))))

(defn clamp [n min-value max-value]
  (min max-value (max min-value n)))

(defn clamped-lerp [a b t]
  (clamp (lerp a b t) a b))

(defn push-towards [n target amount]
  (if (< n target)
    (min target (+ n amount))
    (max target (- n amount))))

(defn midpoint [min max]
  (+ min (/ (- max min) 2)))

(defn relative [n min max]
  "Get a value from 0 to 1 indicating the position of
  n relative to the min and max."
  (clamp (/ (- n min) (- max min)) 0 1))

(defn relative-in-scale [note scale]
  (relative note (first scale) (last scale)))

(defn is-point-in-rect? [point rect-start rect-size]
  (let [start-x (:x rect-start)
        end-x (+ start-x (:x rect-size))
        start-y (:y rect-start)
        end-y (+ start-y (:x rect-size))
        pos-x (:x point)
        pos-y (:y point)]
    (and
     (>= pos-x start-x) (>= pos-y start-y)
     (<= pos-x end-x) (<= pos-y end-y))))

(defn update-at [index function coll]
  (for [i (range (count coll))]
    (if (= i index)
      (function (nth coll i))
      (nth coll i))))

(defn replace-at [index value coll]
  (update-at index (fn [_] value) coll))

(defn find-where [pred coll]
  (first (filter pred coll)))

(defn in-range? [n min max]
  (and (<= min n) (>= max n)))

(defn apply-hash [fn hash]
  (apply fn (interleave (keys hash) (vals hash))))

(defn indices-of [f coll]
  (keep-indexed #(if (f %2) %1 nil) coll))

(defn first-index-of [f coll]
  (first (indices-of f coll)))

(defn find-thing [value coll]
  (first-index-of #(= % value) coll))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn update-print [x]
  (if (= (rand-int 3) 0) (dbg x)))

(defmacro if-> [obj pred then else]
  `(if (~pred ~obj)
     (-> ~obj ~then)
     (-> ~obj ~else)))

(defmacro react-fn [obj condition then-fn]
  (if ~condition (then-fn ~obj) ~obj))

(defmacro react [obj condition then]
  `(if ~condition (-> ~obj ~then) ~obj))

(defmacro react* [obj & statements]
  (assert (= (mod (count statements) 2) 0)
          "react* expects pairs of condition-reactions")

  `(-> ~obj
      ~@(for [stmt-pair (pairs statements)]
           `(helpers/react ~(first stmt-pair)
                           ~(second stmt-pair)))))

(defn map-over-keys [fn hash]
  (apply merge (for [[k v] hash] {(fn k) v})))

(defn map-over-values [fn hash]
  (apply merge (for [[k v] hash] {k (fn v)})))

(defn add-cursor [string interval]
  (let [tick (mod (now) interval)]
    (if (< tick (/ interval 2))
      (str string \_)
      (str string \|))))

(defn split-at-first [value list]
  (let [split-index (first-index-of #(= value %) list)
        split-index (if (nil? split-index) (count list) split-index)

        [before-value after-value] (split-at split-index list)]
    [before-value (drop 1 after-value)]))

(defn name->symbol [arg]
  (cond (symbol? arg) arg
        (keyword? arg) (symbol (name arg))
        :else (symbol (str arg))))

(defmacro defn-defaults [name args & body]
  "Create a function that can provide default values for arguments.

  Arguments that are optional should be placed in a hash with
  their names mapped to their default values.

  When invoking the function, :<optional-argument-name> <value>
  will specify the value the argument should take on."

  (if (map? (last args))
    `(defn
       ~name
       ~(let [mandatory-args (drop-last args)
              options (map-over-keys name->symbol (last args))
              option-names (vec (keys options))]
          (vec (concat mandatory-args
                       [(symbol "&") {:keys option-names
                                      :or options}])))
       ~@body)
    `(defn ~name ~args ~@body)))

(defn choose [pred x y]
  (if (pred x y) [x y] [y x]))
