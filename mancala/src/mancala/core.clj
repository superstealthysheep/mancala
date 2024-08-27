(ns mancala.core
  (:gen-class))

;; `scoring-wells` maps cell_idx -> owner_id
(def scoring-wells {6 0, 13 1})

(def start-state {:board (-> (vec (repeat 14 3))
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
;; move-state requires keys '(:board :player :in-hand :pos)
(defn propagate-move [move-state]
  (let [{:keys [board player in-hand pos]} move-state]
    (if (= in-hand 0)
      move-state
      (if (and (contains? scoring-wells pos)
               (not= (scoring-wells pos) player)) ; could maybe express more concisely?
        (recur (assoc move-state :pos (mod (inc pos) 14))) ;; pass over enemy scoring well
        (if (and (= in-hand 1)
                 (not= (board pos) 0)
                 (not (contains? scoring-wells pos)))
          (do (printf "Cascade! from %d\n" pos)
              (print-state move-state)
              (recur (-> move-state ;; cascade
                         (assoc-in [:board pos] 0)
                         (assoc :pos (mod (inc pos) 14))
                         (assoc :in-hand (+ in-hand (board pos))))))
          (if (and (= in-hand 1) (contains? scoring-wells pos))
            (do (printf "Player %d gets another turn!\n" player)
                (recur (-> move-state
                           (update-in [:board pos] inc)
                           (assoc :pos (mod (inc pos) 14))
                           (update :in-hand dec)
                           (assoc :player (bit-xor player 1))))) ;; flip player
            (recur (-> move-state ;; regular case
                       (update-in [:board pos] inc)
                       (assoc :pos (mod (inc pos) 14))
                       (update :in-hand dec)))))))))

;; (propagate-move (-> start-state
;;                     (assoc-in [:board 0] 0)
;;                     (assoc :pos 1)
;;                     (assoc :in-hand 3)))

;; TODO: validate input
(defn make-move
  [{:keys [board player]}]
  (println "Where to move from?")
  (let [pos (mod (Integer/parseInt (read-line)) 14)]
    (printf "Moving from %d\n" pos)
    (update (propagate-move {:board (assoc board pos 0)
                             :player player
                             :in-hand (board pos)
                             :pos (mod (inc pos) 14)})
            :player #(bit-xor %1 1))))

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

(-main)