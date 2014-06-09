(ns game-utils.effects
  (:require [game-utils.point :as point]
            [game-utils.helpers :as helpers]
            [quil.core :as q]))

(helpers/defn-defaults radial-motion
  [base-pos r {speed 1 offset 0 now (helpers/now)}]
  "Gives back positions moving in a circle around base-pos
  of radius r. speed is the amount of change between positions
  over time, now is the current time and offset is the amount of
  radians the rotation should be displaced by."

  (let [theta (q/radians (mod (+ (q/degrees offset)
                                 (quot (* now speed) 16))
                              360))
        x-displacement (* r (q/cos theta))
        y-displacement (* r (q/sin theta))]
    (point/add base-pos
               (point/create x-displacement y-displacement))))

(defn shaking-motion
  [base-pos displacement speed now]
  ())
