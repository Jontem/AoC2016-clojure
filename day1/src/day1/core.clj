(ns day1.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn turn [instruction state]
  "Return the degrees of the turn"
  (let [degrees (:degrees state)]
    (cond
      (str/starts-with? instruction "L")
      (conj state {:degrees (mod (- degrees 90) 360)})
      (str/starts-with? instruction "R")
      (conj state {:degrees (mod (+ degrees 90) 360)}))))

(defn get-steps [instruction]
  (read-string
    (subs instruction 1)))

(defn walk [degrees steps state]
  "Return state after turning"
  (cond
    (= degrees 0) (conj state { :y (+ (:y state) steps)})
    (= degrees 90) (conj state { :x (+ (:x state) steps)})
    (= degrees 180) (conj state { :y (- (:y state) steps)})
    (= degrees 270) (conj state { :x (- (:x state) steps)})))

(defn readinput []
  (str/split
    (slurp "/Users/jonathanmourtada/projects/clojure_advent/day1/input") #","))

(defn sanitize [inputs]
  (map str/trim inputs))

(defn move []
  (let [inputs (sanitize (readinput))]
    (reduce
      (fn [state input]
        (let [turned-state (turn input state)]
          (let [moved-state (walk (:degrees turned-state) (get-steps input) turned-state)]
            moved-state))) {:x 0 :y 0 :degrees 0 } inputs)))

(defn day1 []
  (let [completed-state (move)]
    (+
      (Math/abs (:x completed-state))
      (Math/abs (:y completed-state)))))

(defn -main
  "Main function"
  [& args]
  (println (day1)))