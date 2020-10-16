(ns harj.chessengine
  (:require [clojure.string :as str]))

;; Board Representation
(defn blank-board []
  "Create board as vector of vectors: {:piece :queen :color :white}]"
  (->> {:piece nil :color nil}
       (repeat 8) vec
       (repeat 8) vec))

(defn initial-board []
  "Set board to opening positions"
  (let [home-row [:rook :knight :bishop :queen :King :bishop :knight :rook]
        pawn-row (repeat 8 :pawn)
        empty-row (vec (repeat 8 {}))
        colors (fn [color pieces]
                 (vec (for [p pieces]
                        {:piece p :color color})))]

    (vec
      (concat [(colors :white home-row)
               (colors :white pawn-row)]
               (repeat 4 empty-row)
               [(colors :black pawn-row)
               (colors :black home-row)]))))

(defn print-board [board]
  (let [p->str (fn [p]
                 (str (first (name (get p :color " ")))
                      (first (name (get p :piece ".")))))]
  (doseq [row (reverse board)]
    (println (str/join " " (map p->str row))))))

;Getters
(defn get-pos [board [row col]]
  "Takes board and position as vector of rank and file"
  (get-in board [row col]))

(defn set-pos [board [row col] p]
  (assoc-in board [row col] p))

;; Move Generation
(defn out-of-bounds? [[row col]]
  (or (< 7 row)
      (< 7 col)))

(defn valid-move? [board [start-row start-col] [finish-row finish-col]]
  "Check if piece is allowed to make that move"
  (let [turn (if (@game-status :white_turn?) :white :black)
        square (get-pos board [start-row start-col])
        piece (:piece square)
        color (:color square)
        diagonal? (= (Math/abs (- finish-row start-row)) (Math/abs (- finish-col start-col)))]
    (when (not (empty? square))
      (if (not (= turn color))
        false
        (case piece
          :pawn (case color
                  :white (if (= start-row 1)
                           (or (= finish-row (inc start-row)) (= finish-row (+ start-row 2)))
                           (= finish-row (inc start-row)))
                  :black (if (= start-row 6)
                           (or (= finish-row (dec start-row)) (= finish-row (- start-row 2)))
                           (= finish-row (dec start-row))))
          :rook (or (= finish-col start-col) (= finish-row start-row))
          :bishop diagonal?
          :queen (or (or (= finish-col start-col) (= finish-row start-row))
                     diagonal?)
          :King (or (or (= finish-col (inc start-col)) (= finish-col (dec start-col)))
                    (or (= finish-row (inc start-row) (= finish-row (dec start-col)))))
          :knight (or (and (or (= finish-row (+ start-row 2)) (= finish-row (- start-row 2)))
                           (or (= finish-col (inc start-col)) (= finish-col (dec start-col))))
                      (and (or (= finish-col (+ start-col 2)) (= finish-col (- start-col 2)))
                           (or (= finish-row (inc start-row)) (= finish-row (dec start-row)))))
          )))))

(defn blocked? [board [start-row start-col] [finish-row finish-col]]
  (let [rows (if (< start-row finish-row)
               (range (inc start-row) finish-row)
               (range (dec start-row) finish-row -1))
        cols (if (< start-col finish-col)
               (range (inc start-col) finish-col)
               (range (dec start-col) finish-col -1))
        start-square (get-pos board [start-row start-col])
        finish-square (get-pos board [finish-row finish-col])]
    (cond
      (= (:piece start-square) :knight) false
      (= (:color start-square) (:color finish-square)) true
      (= start-row finish-row) (not (every? nil? (for [i cols]
                                              (:piece (get-pos board [start-row i])))))
      (= start-col finish-col) (not (every? nil? (for [i rows]
                                                   (:piece (get-pos board [i start-col])))))
      (= (Math/abs (- start-row finish-row)) (Math/abs (- start-col finish-col))) (not (every? nil? (loop [x rows y cols pieces '()]
                                                                                             (if (and (empty? x) (empty? y))
                                                                                               pieces
                                                                                               (recur (rest x) (rest y) (conj pieces (:piece (get-pos board [(first x) (first y)]))))))))
    :else (println "Move not possible - does not lie on same file, rank or diagonal"))))

(defn move [board start finish]
  "User enters move as a vector of start and finish square [[1 1] [3 1]]. If valid, adds move to move-history and updates game status."
  (cond
    (out-of-bounds? finish) (println "Move is out of bounds")
    (and (valid-move? board start finish) (not (blocked? board start finish))) (do
                                                                                 (swap! move-history conj [start finish])
                                                                                 (swap! game-status update :moves inc)
                                                                                 (swap! game-status update :white_turn? not))
    :else (println "Not valid move")))

(defn update-board [board start finish]
  "Takes a board and returns the new board after the move is made"
    (-> board
      (set-pos start {})
      (set-pos finish {:piece (:piece (get-pos board start)) :color (:color (get-pos board start))})))

;; Game state

;Moves and updating board
(def move-history
  (atom []))

(def game-status
  (atom {:moves 0
         :white_turn? true}))

(defn new-game []
  (reset! move-history [])
  (swap! game-status assoc :moves 0)
  (swap! game-status assoc :white_turn? true))

(defn current-board []
  (loop [board (initial-board)
         moves @move-history]
    (let [current-move (first moves)
          start (first current-move)
          finish (second current-move)]
      (if (nil? current-move)
        board
        (recur (update-board board start finish) (rest moves))))))

;Scoring
(defn color-pieces [board color]
  (mapcat (fn [row]
            (filter (fn [sq] (= (get sq :color) color)) row))
          board))

(defn color-score [color-pieces]
  (let [pieces-scores {:pawn 1
                      :knight 3
                      :bishop 3
                      :rook 5
                      :queen 8
                      :King 9
                      }]
    (loop [score 0 pieces color-pieces]
      (if (empty? pieces)
        score
        (recur (+ score (get pieces-scores (get (first pieces) :piece))) (rest pieces))))))

(defn game-score [board]
  (let [white_captured_value (- 39 (color-score (color-pieces board :white)))
        black_captured_value (- 39 (color-score (color-pieces board :black)))]

    {:white_score (- white_captured_value black_captured_value)
     :black_score (- black_captured_value white_captured_value)
     }))


