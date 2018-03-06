(ns tetris.core
    (:use-macros [cljs.core.async.macros :only [go-loop]])
    (:require
      [reagent.core :as r]
      [cljs.core.async :refer [<! timeout]]))


;; ------------------------
;; Logic

; (defn translate-to-origin [offset segment]
;   (map - segment offset))
;
; (defn rotate-90-degrees [piece]
;   (let [center (first piece)
;         translated (map (partial translate-to-origin center) (rest piece))]
;     (cons center translated)))

(defn multi-assoc-in [board keys val]
  (loop [board board
         keys keys]
    (if (empty? keys)
      board
      (recur (assoc-in board (first keys) val) (rest keys)))))

(defn next-board [board piece]
  (multi-assoc-in board (:segments piece) (:name piece)))

(defn in-bounds? [[y x]]
  (and (<= 0 x 9) (<= 0 y 19)))

(defn no-conflicts? [board segments]
  (every? nil? (map #(get-in board %) segments)))

(defn down [{board :board piece :active-piece :as state}]
  (let [next-piece (update piece :segments #(map (fn [e] (update e 0 inc)) %))
        intermediate-board (multi-assoc-in board (:segments piece) nil)
        next-segments (:segments next-piece)]
    (if (and (every? in-bounds? next-segments) (no-conflicts? intermediate-board next-segments))
      {:board (next-board intermediate-board next-piece) :active-piece next-piece}
      state)))

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


(def pieces [{:name "I", :segments [[0 0] [0 1] [0 2] [0 3]]}
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

(def game-state (r/atom {:board (next-board blank-board starting-piece)
                         :active-piece starting-piece}))

;; ------------------------
;; Initialize app

(defn mount-root []
  (r/render [board-component game-state] (.getElementById js/document "app")))

(defn init! []
  (mount-root))

;; ------------------------
;; Game Loop

(go-loop []
   (<! (timeout 100))
   (let [next-state (down @game-state)]
     (if (= next-state @game-state)
       (swap! game-state assoc :active-piece (rand-nth pieces))
       (reset! game-state next-state)))
   (recur))
