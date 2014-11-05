(ns cloj-dice.core)

(require '[clojure.string :as str])

(def statuses (atom {:game "active" :computer {:score 0 :turn 5} :user {:score 0 :turn 5}}))

(defn score-for [player] (get-in @statuses [player :score]))

(defn turn-for [player] (get-in @statuses [player :turn]))

(defn game-status [] (get @statuses :game))

(defn update-score-for [player score]
  (swap! statuses assoc-in [player :score] (+ (score-for player) score)))

(defn update-computer-turn [turn]
  (swap! statuses assoc-in [:computer :turn] (- (turn-for :computer) turn)))

(defn single-die-roll [n]
  (+ 1 (rand-int 6)))

(defn single-turn [turn]
  (map single-die-roll (range turn)))

(defn choose-computer-score [result]
  (let [n (count (filter #(= 3 %) result))]
    (if (> n 0)
      (update-computer-turn n)
      (do
       (update-computer-turn 1)
       (update-score-for :computer (apply min result))))))

(defn map-user-input [input]
    (map #(read-string %) (str/split input #",")))

(defn valid-selections? [selections result]
  (let [comparison (map #(some #{%} result) selections)]
    (if (some #(= nil %) comparison) false true)))

(defn handle-user-choice [input result]
  (let [selections (map-user-input input)]
    (if (true? (valid-selections? selections result))
      (do
        (update-score-for :user (reduce + (filter #(not= 3 %) selections)))
        (swap! statuses assoc-in [:user :turn] (- (turn-for :user) (count selections))))
      (do
        (println "Please choose numbers that you rolled")
        (handle-user-choice (read-line) result)))))

(defn user-turn [turn]
  (let [result (single-turn turn)]
    (println (str "You rolled a " (apply str result) ". choose one or more of these, separated by commas."))
    (handle-user-choice (read-line) result)
    (println (str "your current score is " (score-for :user) ". you have " (turn-for :user) " turns left."))))

(defn simulate-computer-turn []
  (loop [x (turn-for :computer)]
    (when (> x 0)
      (choose-computer-score (single-turn x))
      (recur (turn-for :computer)))))

(defn user-input [input]
  (cond
   (= input "r") (user-turn (turn-for :user))
   (= input "q") (swap! statuses assoc :game "inactive")))

(defn determine-outcome []
  (if (= (game-status) "inactive")
    (println "Bye!")
    (if (= (score-for :user) (score-for :computer))
      (println "You tied.")
      (if (< (score-for :user) (score-for :computer)) (println "You win!") (println "You lose!")))))

(defn user-sequence []
  (loop [turns-remaining (turn-for :user)]
    (when (and (> turns-remaining 0) (= (game-status) "active") (<= (score-for :user) (score-for :computer)))
       (println "Press 'r' to roll or 'q' to quit.")
       (user-input (read-line))
      (recur (turn-for :user))))
  (determine-outcome))

(defn start-game []
  (simulate-computer-turn)
  (println "Welcome to Dice. Please enter your name:")
  (def player-name (read-line))
  (println (str "Hello, " player-name ". The computer rolled a " (score-for :computer)))
  (user-sequence))

(defn -main [] (start-game))
