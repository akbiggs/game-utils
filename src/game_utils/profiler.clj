(ns game-utils.profiler
  (:require [taoensso.timbre.profiling :as profiling
             :only [p profile sampling-profile]]))

(defmacro p [id & body]
  `(profiling/p ~id ~@body))

(defmacro profile [level id & body]
  `(profiling/profile ~level ~id ~@body))

(defmacro sampling-profile [level probability id & body]
  `(profiling/sampling-profile ~level ~probability ~id ~@body))
