(ns wator.shark
  (:require [wator
             [cell :as cell]
             [animal :as animal]]))

(defn make []
  )

(defmethod cell/tick ::shark [shark loc world]
  (animal/tick shark loc world)
  )

(defmethod animal/move ::shark [shark loc world]
  (animal/do-move shark loc world))

(defmethod animal/reproduce ::shark [shark loc world]
  (animal/do-reproduce shark loc world))

(defn eat [shark loc world]
  )
