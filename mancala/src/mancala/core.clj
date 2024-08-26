(ns mancala.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; cell_idx -scoring-wells-> owner_id
(def scoring-wells {6 0, 13 1})
scoring-wells
(contains? scoring-wells 6)
(contains? scoring-wells 0)

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

;; TODO: account for chained moves
;; TODO: account for landing in scoring well
;; (defn propagate-move [{:keys [board player in-hand pos]}]
;; move-state requires keys '(:board :player :in-hand :pos)
(defn propagate-move [move-state]
  (let [{:keys [board player in-hand pos]} move-state]
    (if (= in-hand 0)
      move-state
      (if (and (contains? scoring-wells pos)
               (not= (scoring-wells pos) player)) ; could maybe express more concisely?
        (recur (assoc move-state :pos (mod (inc pos) 14))) ;; skip enemy scoring cell
        (recur (-> move-state ;; don't skip
                   (assoc :pos (mod (inc pos) 14))
                   (assoc :in-hand (dec in-hand))))))))
  ;; (if (= in-hand 0)
  ;;   {:board board :player player :in-hand in-hand :pos pos}
  ;;   (if (and (contains? scoring-wells pos)
  ;;            (not= (scoring-wells pos) player))
  ;;     (recur {:board board ;; skip the cell
  ;;             :player player
  ;;             :in-hand in-hand
  ;;             :pos (mod (inc pos) 14)})
  ;;     (recur {:board (update board pos inc) ;; don't skip
  ;;             :player player
  ;;             :in-hand (dec in-hand)
  ;;             :pos (mod (inc pos) 14)}))))

;; (propagate-move (assoc (assoc start-state :pos 0) :in-hand 3))
(propagate-move (-> start-state
                    (assoc-in [:board 0] 0)
                    (assoc :pos 1)
                    (assoc :in-hand 3)))

;; TODO: validate input
(defn make-move
  [{:keys [board player]}]
  (println "Where to move from?")
  (let [pos (mod (Integer/parseInt (read-line)) 14)]
    (printf "Moving from %d\n" pos)
    (assoc (propagate-move {:board (assoc board pos 0)
                            :player player
                            :in-hand (board pos)
                            :pos (mod (inc pos) 14)})
           :player (bit-xor player 1))))

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

(run-game start-state)

(print-state start-state)
;; (-> start-state
;;     (print-then-apply make-move)
;;     (print-then-apply make-move)
;;     (print-then-apply make-move)
;;     (print-state))


start-state
;; how best to represent the board state?
;; We have regular wells and scoring wells
;; KISS for now
;; Just have board-state only include the regular wells? 