(ns game-utils.game
  (:require [quil.core :as q]
            [game-utils.helpers :as helpers]
            [game-utils.input :as input]
            [game-utils.time :as time]))

(defn- create-setup [world-setup world-update world-draw]
  (fn []
    (q/frame-rate 60)

    (q/set-state! :input (atom (input/create))
                  :time (atom (time/create (helpers/now)))
                  :world (atom (world-setup (q/width) (q/height)))
                  :update-world (atom world-update)
                  :draw-world (atom world-draw))))

(defn- create-update-context [input time]
  {:input input
   :elapsed-time time})

(defn- update! []
  (swap! (q/state :time) #(time/update (helpers/now) %))

  (let [dt (:elapsed-time @(q/state :time))]
    (swap! (q/state :input) #(input/update dt %))

    (let [in @(q/state :input)
          update-world @(q/state :update-world)
          update-context (create-update-context in dt)]
      (swap! (q/state :world) #(update-world update-context %)))))

(defn- draw! []
  (let [draw-world @(q/state :draw-world)
        world @(q/state :world)]
    (draw-world world)))

(defn- game-loop []
  (update!)
  (draw!))

(defn- update-input-on-key-pressed! []
  (swap! (q/state :input) #(input/update-with-key-press (q/raw-key) %)))

(defn- update-input-on-key-released! []
  (swap! (q/state :input) #(input/update-with-key-release (q/raw-key) %)))

(defn create [title size
              world-setup world-update world-draw]
  (q/sketch :title title
            :setup (create-setup world-setup world-update world-draw)
            :draw game-loop
            :size size
            :key-pressed update-input-on-key-pressed!
            :key-released update-input-on-key-released!))

(defn reload-loop! [world-update world-draw]
  (reset! (q/state :update-world) world-update)
  (reset! (q/state :draw-world) world-draw))
