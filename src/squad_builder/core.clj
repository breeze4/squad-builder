(ns squad-builder.core)

;; elements
;(def elements [:fire :wind :earth :water :plant :metal :energy :void :light :shadow])

;; element matchups
(def elements {
               :fire   {:strong #{:wind}
                        :weak   #{:fire :energy}}
               :wind   {:strong #{:earth}
                        :weak   #{:wind :void}}
               :earth  {:strong #{:water}
                        :weak   #{:earth :metal}}
               :water  {:strong #{:fire}
                        :weak   #{:water :plant}}
               :plant  {:strong #{:void}
                        :weak   #{:plant :light}}
               :metal  {:strong #{:plant}
                        :weak   #{:metal :shadow}}
               :energy {:strong #{:metal}
                        :weak   #{:shadow :energy}}
               :void   {:strong #{:energy}
                        :weak   #{:void :light}}
               })

(defn- strong-against [attacker-type defender-type]
  (get-in elements [attacker-type :strong defender-type]))

(defn- weak-against [attacker-type defender-type]
  (get-in elements [attacker-type :weak defender-type]))

(defn- score-type-pair [attacker-type defender-type]
  (let [is-strong (strong-against attacker-type defender-type)
        is-weak (weak-against attacker-type defender-type)
        base-score 100]
    (cond is-strong (* base-score 1.5)
          is-weak (* base-score 0.5)
          :normal base-score)))

(defn- score-types [atk-type def-types]
  (map #(score-type-pair atk-type %) def-types))

(defn- score-type-lists [atk-types def-types]
  (reduce + (flatten (map #(score-types % def-types) atk-types))))

;; perspective is attacker is the one who has the choice in attacking or not :)
(defn score-pair-dragons [attacker defender]
  (let [attacker-types (:types attacker)
        defender-types (:types defender)
        atk-vs-def-scores (score-type-lists attacker-types defender-types)
        def-vs-atk-scores (score-type-lists defender-types attacker-types)
        diff (- atk-vs-def-scores def-vs-atk-scores)]
    diff))

(def d1 {:types [:fire :wind]})
(def d2 {:types [:earth :water]})


;;
