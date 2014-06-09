(ns game-utils.game
  (:require [quil.core :as q]
            [game-utils.helpers :as helpers]
            [game-utils.input :as input]
            [game-utils.time :as time]
            [game-utils.profiler :as profiler]))

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
  (profiler/p :update-time
              (swap! (q/state :time) #(time/update (helpers/now) %)))

  (let [dt (:elapsed-time @(q/state :time))]
    (profiler/p :update-input
                (swap! (q/state :input) #(input/update dt %)))

    (let [in @(q/state :input)
          update-world @(q/state :update-world)
          update-context (create-update-context in dt)]
      (profiler/p :update-world
                  (swap! (q/state :world) #(update-world % update-context))))))

(defn- draw! []
  (let [draw-world (profiler/p :deref-draw-fn @(q/state :draw-world))
        world (profiler/p :deref-world @(q/state :world))]
    (profiler/p :invoke-draw (draw-world world))))

(defn- game-loop []
  (update!)
  (draw!))

(defn- profiled-game-loop []
  (profiler/profile :info :Update (update!))
  (profiler/sampling-profile :info 0.25 :Draw (draw!)))

(defn- update-input-on-key-pressed! []
  (swap! (q/state :input) #(input/update-with-key-press (q/raw-key) %)))

(defn- update-input-on-key-released! []
  (swap! (q/state :input) #(input/update-with-key-release (q/raw-key) %)))

(helpers/defn-defaults create [title size-or-fullscreen
                               world-setup world-update world-draw
                               {profiled? false}]
  (q/sketch :title title
            :setup (create-setup world-setup world-update world-draw)
            :draw (if profiled? profiled-game-loop game-loop)
            :size size-or-fullscreen
            :renderer :opengl
            :features (if (= size-or-fullscreen :fullscreen) [:present] [])
            :key-pressed update-input-on-key-pressed!
            :key-released update-input-on-key-released!))

(defn reload-loop! [world-update world-draw]
  (reset! (q/state :update-world) world-update)
  (reset! (q/state :draw-world) world-draw))
