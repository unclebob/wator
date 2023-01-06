(ns wator.shark
  (:require [clojure.spec.alpha :as s]
            [wator
             [config :as config]
             [cell :as cell]
             [water :as water]
             [animal :as animal]]))

(s/def ::health int?)
(s/def ::shark (s/and #(= ::shark (::cell/type %))
                      ::animal/animal
                      (s/keys :req [::health])))
(defn is? [cell]
  (= ::shark (::cell/type cell)))

(defn make []
  {:post [(s/valid? ::shark %)]}
  (merge {::cell/type ::shark
          ::health config/shark-starting-health}
         (animal/make)))

(defmethod animal/make-child ::shark [fish]
  (make))

(defmethod animal/get-reproduction-age ::shark [shark]
  config/shark-reproduction-age)

(defn health [shark]
  (::health shark))

(defn set-health [shark health]
  (assoc shark ::health health))

(defn decrement-health [shark]
  (update shark ::health dec))

(defmethod cell/tick ::shark [shark loc world]
  (if (= 1 (health shark))
    [nil {loc (water/make)}]
    (-> shark
        (decrement-health)
        (animal/tick loc world)))
  )

(defmethod animal/move ::shark [shark loc world]
  (animal/do-move shark loc world))

(defmethod animal/reproduce ::shark [shark loc world]
  (animal/do-reproduce shark loc world))

(defn eat [shark loc world]
  )
