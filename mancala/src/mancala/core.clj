(ns mancala.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def start-state {:board (vec (repeat 12 3))
            :scores [0 0]
            :player 0
            :in-hand 0
            :pos nil})

(defn print-state [{:keys [board scores player]}]
  (let [bottom-wells (subvec board 0 6)
        top-wells (subvec board 6 12)] 
    (printf "%2d " (scores 1))
    (println (vec (reverse top-wells)))
    (print "   ")
    (print bottom-wells))
    (printf " %2d\n" (scores 0)))

(print-state start-state)

(defn propagate-move [{:keys [board scores player in-hand pos]}]
  (if (= in-hand 0) 
    {:board board :scores scores :player player :in-hand in-hand :pos pos}
    (propagate-move {:board (update board pos inc)
                    :scores scores
                    :player player
                    :in-hand (dec in-hand)
                    :pos (inc pos)})))
                  
;; (propagate-move (assoc (assoc start-state :pos 0) :in-hand 3))
(propagate-move (-> start-state
                    (assoc-in [:board 0] 0)
                    (assoc :pos 1)
                    (assoc :in-hand 3)))

(defn make-move
  [{:keys [board scores player]}]

  (println "Where to move from?")
  (let [pos (mod (Integer/parseInt (read-line)) 12)
        in-hand (board pos)]
    (println curr-pos))

  {:board board
   :scores scores
   :player player
   :in-hand in-hand
   :pos pos})
(make-move start-state)

start-state
;; how best to represent the board state?
;; We have regular wells and scoring wells
;; KISS for now
;; Just have board-state only include the regular wells? 
