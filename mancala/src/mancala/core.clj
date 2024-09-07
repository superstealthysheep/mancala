(ns mancala.core
  (:gen-class))

;; `scoring-wells` maps cell_idx -> owner_id
(def scoring-wells {6 0, 13 1})

;; `playable-wells` maps owner_id -> set of wells from which they may play
(def playable-wells {0 #{0 1 2 3 4 5}, 1 #{7 8 9 10 11 12}})

(def start-state {:board (-> (vec (repeat 14 4))
                             (assoc 6 0)
                             (assoc 13 0))
                  :player 0})

(defn print-state [{:keys [board player]}]
  (let [bottom-wells (subvec board 0 6)
        score-0 (board 6)
        top-wells (subvec board 7 13)
        score-1 (board 13)]
    (println (if (= player 1) "> " "  ")
             (format "%2d " score-1)
             (vec (reverse top-wells)))
    (println (if (= player 0) "> " "  ")
             "   "
             bottom-wells
             (format " %2d" score-0))))

(print-state start-state)

;; don't want to use this yet
(defn step-forward [pos]
  (mod (inc pos) 14))

;; DONE: account for cascading moves
;; DONE: account for landing in scoring well
;; This code is getting pretty messy. I wonder if there's a cleaner way to express this. 
;  - Nicer syntax?
;  - A smarter way of representing this?
;  - Breaking this into smaller functions?
(defn propagate-move [{:keys [board player in-hand pos] :as move-state}]
  (cond ;; this feels better than nested ifs, but the pairing is not clear imo
    (= in-hand 0)
    move-state,
    (and (contains? scoring-wells pos) (not= (scoring-wells pos) player))
    (recur (assoc move-state :pos (mod (inc pos) 14))),
    (and (= in-hand 1) (not= (board pos) 0) (not (contains? scoring-wells pos)))
    (do (printf "Cascade! from %d\n" pos)
        (print-state move-state)
        (recur (-> move-state ;; cascade
                   (assoc-in [:board pos] 0)
                   (assoc :pos (mod (inc pos) 14))
                   (assoc :in-hand (+ in-hand (board pos)))))),
    (and (= in-hand 1) (contains? scoring-wells pos)) ;; and implicitly this is the current player's scoring well
    (do (printf "Player %d gets another turn!\n" player)
        (recur (-> move-state
                   (update-in [:board pos] inc)
                   (assoc :pos (mod (inc pos) 14))
                   (update :in-hand dec)
                   (assoc :player (bit-xor player 1))))),
    :else (recur (-> move-state
                     (update-in [:board pos] inc)
                     (assoc :pos (mod (inc pos) 14))
                     (update :in-hand dec)))))

(defn valid-move? [{:keys [board player]} pos]
  (and (contains? (playable-wells player) pos)
       (not= (board pos) 0)))

(defn make-move [{:keys [board player] :as state}]
  (println "Where to move from?")
  (let [pos (mod (Integer/parseInt (read-line)) 14)]
    (if-not (valid-move? state pos)
      (do (println "Invalid move. Try again.")
          (recur state))
      (do (printf "Moving from %d\n" pos)
          (propagate-move {:board (assoc board pos 0)
                           :player (bit-xor player 1)
                           :in-hand (board pos)
                           :pos (mod (inc pos) 14)})))))

(defn print-then-apply [state fn]
  (print-state state)
  (fn state))

(defn game-over? [{:keys [board player]}]
  (if (= player 0)
    (every? #(= 0 %1) (subvec board 0 6))
    (every? #(= 0 %1) (subvec board 8 13))))

(defn run-game [state]
  (print-state state)
  (if (game-over? state)
    state
    (recur (make-move state))))

;; (run-game start-state)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (run-game start-state)
  )

;; (-main)