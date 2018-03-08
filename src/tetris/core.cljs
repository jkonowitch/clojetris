(ns tetris.core
    (:use-macros [cljs.core.async.macros :only [go-loop]])
    (:require
      [clojure.browser.event :as events]
      [reagent.core :as r]
      [cljs.core.async :refer [<! timeout chan put!]]))


;; ------------------------
;; Logic

(defn in-bounds? [[y x]]
  (and (<= 0 x 9) (<= 0 y 19)))

(defn no-conflicts? [board proposed-segments]
  (every? nil? (map #(get-in board %) proposed-segments)))

(defn piece->board [{val :name keys :segments} board]
  (reduce #(assoc-in %1 %2 val) board keys))

(defn maybe-next-board
  [{prev-board :board prev-piece :piece}
   {next-segments :segments :as next-piece}]
  (let [nil-piece (assoc prev-piece :name nil)
        clean-board (piece->board nil-piece prev-board)]
    (when (and (every? in-bounds? next-segments)
               (no-conflicts? clean-board next-segments))
      (piece->board next-piece clean-board))))

(defn next-state
  "Takes a segment transformation and the current state. Returns the next state.
   Simply returns the current state if the segment trasnformation is illegal."
  [transform {board :board piece :piece :as state}]
  (let [next-piece (update piece :segments transform)]
    (if-let [next-board (maybe-next-board state next-piece)]
      {:board next-board :piece next-piece}
      state)))

(def Θ (/ js/Math.PI 2))

(def cosΘ (int (.cos js/Math Θ)))

(def sinΘ (int (.sin js/Math Θ)))

(defn rotate-90 [[y x]]
  (let [x' (- (* x cosΘ) (* y sinΘ))
        y' (+ (* y cosΘ) (* x sinΘ))]
    [y' x']))

(defn rotate-about-point
  "Takes the 'center' of a set of segments and returns a fn that will
   rotate a vector of points 90 degrees around it."
  [center]
  (comp (partial map vec)
        (partial map #(map + % center)) ; translate back
        (partial map rotate-90)
        (partial map #(map - % center)))) ; translate to origin

(defn do-rotation [segments]
  (let [center (first segments)
        rotate (rotate-about-point center)]
    (into [center] (rotate (rest segments)))))

(def translate #(partial next-state (partial map %)))

(def initialize (translate identity))

(def down (translate #(update % 0 inc)))

(def right (translate #(update % 1 inc)))

(def left (translate #(update % 1 dec)))

(defmulti up #(get-in % [:piece :name]))

(defmethod up "O"
  [state]
  (next-state identity state))

(defmethod up :default
  [state]
  (next-state do-rotation state))

(defn lines-to-clear [board]
  (keep-indexed #(if (not-any? nil? %2) %1) board))

(defn clear-lines [[i & idxs] board]
  (let [blank-row (vec (repeat 10 nil))
        board-without-line (concat (subvec board 0 i)
                                   (subvec board (inc i)))
        new-board (into [blank-row] board-without-line)]
    (if (empty? idxs) new-board (recur idxs new-board))))

;; -------------------------
;; Views

(defn cell-component [n i cell]
  (let [element (str "div.cell" (when cell (str "." cell)))]
    ^{:key (str n i)} [(keyword element)]))

(defn row-component [n row]
  ^{:key n} [:div.row (map-indexed (partial cell-component n) row)])

(defn board-component [game-state]
  (let [board (:board @game-state)]
    [:div.board (map-indexed row-component board)]))

;; ------------------------
;; Game State

(def pieces [{:name "I", :segments [[0 1] [0 0] [0 2] [0 3]]}
             {:name "O", :segments [[0 0] [0 1] [1 0] [1 1]]}
             {:name "T", :segments [[0 1] [0 0] [0 2] [1 1]]}
             {:name "S", :segments [[0 1] [0 2] [1 0] [1 1]]}
             {:name "Z", :segments [[0 1] [0 0] [1 1] [1 2]]}
             {:name "J", :segments [[1 1] [0 0] [1 0] [1 2]]}
             {:name "L", :segments [[1 1] [0 2] [1 0] [1 2]]}])

(def starting-piece (rand-nth pieces))
(def blank-board (->> (repeat 10 nil)
                      (vec)
                      (repeat 20)
                      (vec)))

(def game-state (r/atom (initialize {:board blank-board :piece starting-piece})))

;; ------------------------
;; Initialize app

(defn mount-root []
  (r/render [board-component game-state] (.getElementById js/document "app")))

(defn init! []
  (mount-root))

;; ------------------------
;; Game Loop

(go-loop []
   (<! (timeout 500))
   (let [next-state (down @game-state)]
     (if (= next-state @game-state)
       (do
         (swap! game-state assoc :piece (rand-nth pieces))
         (when-let [lines (not-empty (lines-to-clear (:board @game-state)))]
           (swap! game-state update-in [:board] (partial clear-lines lines))))
       (reset! game-state next-state)))
   (recur))

;; ------------------------
;; User Input Loop

(def command-map {38 up
                  40 down
                  37 left
                  39 right})

(def keycode-transducer
  (comp (map #(.-keyCode %))
        (map #(get command-map %))
        (remove nil?)))

(def user-input-chan (chan 1 keycode-transducer))

(go-loop []
  (let [command (<! user-input-chan)]
    (reset! game-state (command @game-state)))
  (recur))

(events/listen (.querySelector js/document "body") "keydown" #(put! user-input-chan %))
