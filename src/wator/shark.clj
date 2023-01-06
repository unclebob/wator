(ns wator.shark
  (:require [clojure.spec.alpha :as s]
            [wator
             [config :as config]
             [cell :as cell]
             [animal :as animal]]))

(s/def ::shark (s/and #(= ::shark (::cell/type %))
                      ::animal/animal))
(defn is? [cell]
  (= ::shark (::cell/type cell)))

(defn make []
  {:post [(s/valid? ::shark %)]}
  (merge {::cell/type ::shark}
         (animal/make)))

(defmethod animal/make-child ::shark [fish]
  (make))

(defmethod animal/get-reproduction-age ::shark [shark]
  config/shark-reproduction-age)

(defmethod cell/tick ::shark [shark loc world]
  (animal/tick shark loc world)
  )

(defmethod animal/move ::shark [shark loc world]
  (animal/do-move shark loc world))

(defmethod animal/reproduce ::shark [shark loc world]
  (animal/do-reproduce shark loc world))

(defn eat [shark loc world]
  )
