(ns game-utils.input
  (:require [quil.core :as q]
            [game-utils.helpers :as helpers]))

(defn create []
  {:mouse-pos {:x 0 :y 0}
   :mouse-down? false
   :mouse-up? true
   :mouse-down-duration 0
   :mouse-tapped? false
   :mouse-just-released? false
   :time-since-last-click 0

   :last-key-pressed nil
   :last-keycode-pressed nil
   :last-key-tapped nil
   :last-keycode-tapped nil

   :cycles-key-held 0
   })

(defn update [elapsed-time previous-input]
  (let [mouse-down? (or (= (q/mouse-button) :left)
                        (= (q/mouse-button) :right))
        previous-mouse-down? (:mouse-down? previous-input)
        old-down-duration (:mouse-down-duration previous-input)

        mouse-tapped? (and mouse-down? (not previous-mouse-down?))
        mouse-just-released? (and (not mouse-down?) previous-mouse-down?)
        time-since-last-click (if mouse-just-released?
                                0
                                (+ (:time-since-last-click previous-input)
                                   elapsed-time))
        last-key-pressed (if (q/key-pressed?) (q/raw-key) nil)]
    {:mouse-pos {:x (q/mouse-x) :y (q/mouse-y)}
     :mouse-down? mouse-down?
     :mouse-up? (not mouse-down?)

     :mouse-tapped?
     mouse-tapped?

     :mouse-double-clicked?
     (and mouse-tapped? (<= time-since-last-click 200))

     :mouse-just-released?
     mouse-just-released?

     :mouse-down-duration
     (if mouse-down?
       (+ old-down-duration elapsed-time)
       0)

     :time-since-last-click
     time-since-last-click

     :last-key-pressed
     last-key-pressed

     :last-keycode-pressed
     (if (q/key-pressed?) (q/key-code) nil)

     :last-key-tapped
     (if (and (q/key-pressed?)
              (not= (:last-key-pressed previous-input) (q/raw-key)))
       (q/raw-key)
       nil)

     :last-keycode-tapped
     (if (and (q/key-pressed?)
              (nil? (:last-keycode-pressed previous-input)))
       (q/key-code)
       nil)

     :cycles-key-held
     (if (and (not (nil? last-key-pressed))
              (= last-key-pressed (:last-key-pressed previous-input)))
       (inc (:cycles-key-held previous-input))
       0)}))

(defn- event-in-rect? [event hitbox-start hitbox-size input]
  (and event (helpers/is-point-in-rect? (:mouse-pos input) hitbox-start hitbox-size)))

(defn just-double-clicked? [hitbox-start hitbox-size input]
  (event-in-rect? (:mouse-double-clicked? input)
                  hitbox-start hitbox-size
                  input))

(defn just-selected? [hitbox-start hitbox-size input]
  (event-in-rect? (:mouse-tapped? input)
                  hitbox-start hitbox-size
                  input))

(defn make-click-event [button]
  #(= (q/mouse-button) button))

(defn make-tap-event [button]
  #(and (:mouse-tapped? %) (= (q/mouse-button) button)))

(def left-mouse-click? (make-click-event :left))
(def middle-mouse-click? (make-click-event :center))
(def right-mouse-click? (make-click-event :right))

(def left-mouse-tapped? (make-tap-event :left))
(def right-mouse-tapped? (make-tap-event :right))
(def middle-mouse-tapped? (make-tap-event :center))

(defn key-tapped? [key user-input]
  (= (:last-key-tapped user-input) key))

(defn key-down? [key user-input]
  (= (:last-key-pressed user-input) key))

(defn key-up? [key user-input]
  (not (key-down? key user-input)))

(defn key-held? [key user-input]
  (let [{:keys [last-key-pressed cycles-key-held]} user-input]
    (and (= last-key-pressed key) (>= cycles-key-held 30))))
