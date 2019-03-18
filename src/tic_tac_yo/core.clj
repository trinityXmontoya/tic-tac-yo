(ns tic-tac-yo.core
  (:require [clojure.string :as s]
            [clojure.set :as set])
  (:gen-class))

;-----------------
;BOARD
;-----------------
(def winning-combos
  [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [0, 4, 8], [2, 4, 6]])

(def board
  (atom (vec (repeat 9 nil))))

(defn open-spots
  "Returns indices of open spots"
  []
  (keep-indexed
    (fn [idx val] (when (nil? val) idx))
    @board))

(defn all-spots-filled?
  []
  (not-any? nil? @board))

;-----------------
;FORMATTING
;-----------------
(defn format-spot
  "Returns user-friendly formatted spot
   Ex: ' X '"
  [spot]
  (if (nil? spot) "   " (str " " spot " ")))

(defn format-row
  "Returns user-friendly formatted row
   Ex: ' X |   | O '"
  [row]
  (s/join "|" (map format-spot row)))

(defn print-formatted-board
  "Returns a user-friendly formatted board
   Ex: ' X |   | O
        -----------
           | O |
        -----------
           | X | X '"
  []
  (dorun (map
           (fn [row]
               (println (format-row row))
               (println "-----------"))
           (partition 3 @board))))

;-----------------
;CHECK WIN
;-----------------
(defn declare-winner-and-exit
  [winner]
  (print-formatted-board)
  (println winner "wins! Game YOver.")
  (System/exit 0))

(defn indicies-of-set
  "Returns set of indices of all spots occupied by provided string"
  [s]
  (set (keep-indexed
    (fn [idx val] (when (= val s) idx))
    @board)))

(defn wins?
  [s]
  (some #(set/subset? (set %) (indicies-of-set s)) winning-combos))

(defn check-for-win
  []
  (cond
    (wins? "X") (declare-winner-and-exit "X")
    (wins? "O") (declare-winner-and-exit "O")
    (all-spots-filled?) (declare-winner-and-exit "No one")))

;-----------------
;COMPUTER + USER MOVES
;-----------------
(defn ask-for-user-move
  []
  (print-formatted-board)
  (println "Please select your spot 0 - 8, yo.")
  (s/trim (read-line)))

(defn valid-input?
  "Checks whether the given string input is an open spot."
  [input]
  (contains? (set (map str (open-spots))) input))

(defn validate-input
  [input]
  "Checks whether the given string input is an open spot. Returns cast to number if it is."
  (when (valid-input? input) (read-string input)))

(defn fill-spot
  [spot x-or-o]
  (let [updated-board (map-indexed #(if (= %1 spot) x-or-o %2) @board)]
    (reset! board updated-board)
    (check-for-win)))

(declare handle-user-input)
(defn make-computer-move
  []
  (let [spot (rand-nth (open-spots))]
    (println "Computer is thinking, yo...")
    (fill-spot spot "O")
    (handle-user-input (ask-for-user-move))))

(defn make-user-move
  [spot]
  (fill-spot spot "X")
  (print-formatted-board)
  (make-computer-move))

(defn handle-user-input
  [input]
  (if-let [spot (validate-input input)]
    (make-user-move spot)
    (do
      (println input " is not a playable spot. Try again, yo.")
      (recur (ask-for-user-move)))))

;-----------------
;GAME
;-----------------
(defn play-ball
  []
  (println "Welcome to Tic Tac Yo, yo.")
  (println "You will be playing as X, yo.")
  (handle-user-input (ask-for-user-move)))

(defn -main
  []
  (play-ball))



