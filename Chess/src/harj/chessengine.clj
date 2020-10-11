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
                 (str (first (name (get p :color "")))
                      (first (name (get p :piece ".")))))]
  (doseq [row (reverse board)]
    (println (str/join " " (map p->str row))))))

;Getters
(defn get-pos [board [row col]]
  "Takes board and position as vector of rank and file"
  (get-in board [(dec row) (dec col)]))

(defn set-pos [board [row col] p]
  (assoc-in board [(dec row) (dec col)] p))

;; Move Generation
(defn out-of-bounds? [[row col]]
  (or (<= 7 (dec row))
      (<= 7 (dec col))))

(defn valid-move? [board [start-row start-col] [finish-row finish-col]]
  (let [square (get-pos board [start-row start-col])
        piece (:piece square)
        color (:color square)
        diagonal? (and (= (- finish-row start-row)) (=(- finish-col finish-row)))]
    (when (not (empty? square))
      (case piece
        :pawn (case color
                :white (if (= start-row 2)
                         (or (= finish-row (inc start-row)) (= finish-row (+ start-row 2)))
                         (= finish-row (inc start-row)))
                :black (if (= start-row 7)
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
        ))))

(defn blocked? [board [start-row start-col] [finish-row finish-col]]
  (let [rows (if (< start-row finish-row)
               (range (inc start-row) finish-row)
               (range (dec start-row) finish-col -1))
        cols (if (< start-col finish-col)
               (range (inc start-col) finish-col)
               (range (dec start-col) finish-col -1))]
    (cond
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
  (cond
    (out-of-bounds? finish) (println "Move is out of bounds")
    (and (valid-move? board start finish) (not (blocked? board start finish))) (-> board
                                                                                   (set-pos start {})
                                                                                   (set-pos finish {:piece (:piece (get-pos board start)) :color (:color (get-pos board start))}))
    :else (println "Not valid move")))

;; Game state
(def game
  {:moves 0
   :turn :white
   :white_score 0
   :black_score 0
   :white_captured_pieces []
   :black_captured_pieces []
   })

(defn reset-game [state]
  (assoc state
    :moves 0
    :turn :white
    :white_score 0
    :black_score 0
    :white_captured_pieces nil
    :black_captured_pieces nil))


;; Scratch code
(comment
  )