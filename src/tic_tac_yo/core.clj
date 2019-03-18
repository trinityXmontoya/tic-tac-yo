(ns tic-tac-yo.core
  (:require [clojure.string :as s])
  (:gen-class))

(declare handle-user-input)

;-----------------
;BOARD
;-----------------
(def winning-combos
  [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [0, 4, 8], [2, 4, 6]])

(def board
  (atom (repeat 9 nil)))

(def x-spots
  "Returns indices of spots currently occupied by X"
  (atom ()))

(def o-spots
  "Returns indices of spots currently occupied by O"
  (atom ()))

(defn open-spots
  "Returns indices of open spots"
  []
  (keep-indexed #(if (nil? %2) %1) @board))

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
        ------------
           | O |
        ------------
           | X | X  '"
  []
  (doall (map
           #(do
              (println (format-row %))
              (println "-----------"))
           (partition 3 @board))))

;-----------------
;CHECK WIN
;-----------------
(defn declare-winner-and-exit
  [winner]
  (print-formatted-board)
  (println (str winner " wins! Game YOver."))
  (System/exit 0))

(defn check-for-win
  []
  (cond
    (some #(.containsAll @o-spots %) winning-combos) (declare-winner-and-exit "O")
    (some #(.containsAll @x-spots %) winning-combos) (declare-winner-and-exit "X")
    (all-spots-filled?) (declare-winner-and-exit "No one")
    :else nil))

;-----------------
;COMPUTER + USER MOVES
;-----------------
(defn ask-for-user-move
  []
  (do
    (print-formatted-board)
    (println "Please select your spot 0 - 8")
    (read-line)))

(defn validate-input
  [input]
  "Checks whether the given string input is an open spot. Returns cast to number if it is."
  (if (.contains (map str (open-spots)) input) (read-string input)))

(defn record-play
  [spot x-or-o]
  (case x-or-o
    "X" (swap! x-spots conj spot)
    "O" (swap! o-spots conj spot)))

(defn fill-spot
  [spot x-or-o]
  (let [updated-board (map-indexed #(if (= %1 spot) x-or-o %2) @board)]
    (reset! board updated-board)
    (record-play spot x-or-o)
    (check-for-win)))

(defn make-computer-move
  []
  (let [spot (rand-nth (open-spots))]
    (do
      (println "Computer is thinking...")
      (fill-spot spot "O")
      (if (all-spots-filled?) (check-for-win) (handle-user-input (ask-for-user-move))))))

(defn make-user-move
  [spot]
  (fill-spot spot "X")
  (print-formatted-board)
  (if (all-spots-filled?) (check-for-win) (make-computer-move)))

(defn handle-invalid-user-input
  [input]
  (println (str input " is not a playable spot. Try again."))
  (handle-user-input (ask-for-user-move)))

(defn handle-user-input
  [input]
  (if-let [spot (validate-input input)]
    (make-user-move spot)
    (handle-invalid-user-input input)))

;-----------------
;GAME
;-----------------
(defn play-ball
  []
  (println "Welcome to Tic Tac Yo.")
  (println "You will be playing as X.")
  (handle-user-input (ask-for-user-move)))

(defn -main
  []
  (play-ball))



