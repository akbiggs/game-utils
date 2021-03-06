(ns game-utils.point)

(defn create [x y]
  {:x x :y y})

(defn make-component-operation [op]
  (fn [p1 p2] {:x (op (:x p1) (:x p2))
               :y (op (:y p1) (:y p2))}))

(defn make-scalar-operation [op]
  (fn [p n] {:x (op (:x p) n)
             :y (op (:y p) n)}))

(def add (make-component-operation +))
(def minus (make-component-operation -))
(def times (make-component-operation *))
(def divide (make-component-operation /))
(def quotient (make-component-operation quot))

(def scalar-divide (make-scalar-operation /))
(def scalar-times (make-scalar-operation *))
(def neg #(scalar-times % -1))

(def zero (create 0 0))
(def one (create 1 1))

(def unit-x (create 1 0))
(def unit-y (create 0 1))

(def down unit-y)
(def up (neg down))

(def right unit-x)
(def left (neg right))
