(ns wator.water
  (:require [wator
             [cell :as cell]
             [fish :as fish]
             [config :as config]]))

(defn make [] {::cell/type ::water})

(defmethod cell/tick ::water [water]
  (if (> (rand) config/water-evolution-rate)
    (fish/make)
    water))
