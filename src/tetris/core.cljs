(ns tetris.core
    (:use-macros [cljs.core.async.macros :only [go-loop]])
    (:require
      [reagent.core :as r]
      [cljs.core.async :refer [<! timeout]]))

;; -------------------------
;; Views

; (def ze-state (r/atom {:title "OKEY DOKEY", :times 1}))
;
; (defn where-we-at [state]
;   [:span (:times state)])
;
; (defn home-page [state]
;   [:div
;     [:h2 "Welcome to " (:title @state) (where-we-at @state)]])

(defn cell-component [n i cell]
  (let [element (str "div.cell" (when cell (str "." cell)))]
    ^{:key (str n i)} [(keyword element)]))

(defn row-component [n row]
  ^{:key n} [:div.row (map-indexed (partial cell-component n) row)])

(defn board-component [game-state]
  (let [board (:board @game-state)]
    [:div.board (map-indexed row-component board)]))
; (def in-bounds-x? #(<= 0 % 9))
; (def in-bounds-y? #(<= 0 % 19))
;
; (defn translate-to-origin [offset segment]
;   (map - segment offset))
;
; (defn rotate-90-degrees [piece]
;   (let [center (first piece)
;         translated (map (partial translate-to-origin center) (rest piece))]
;     (cons center translated)))
(def I-piece {:name "I", :segments [[0 0] [0 1] [0 2] [0 3]]})

(defn multi-assoc-in [board keys val]
  (loop [board board
         keys keys]
    (if (empty? keys)
      board
      (recur (assoc-in board (first keys) val) (rest keys)))))

(defn next-board [board last-piece next-piece]
  (let [intermediate-board (multi-assoc-in board (:segments last-piece) nil)]
    (multi-assoc-in intermediate-board (:segments next-piece) (:name next-piece))))

(defn in-bounds? [[y x]]
  (and (<= 0 x 9) (<= 0 y 19)))

(defn no-conflicts? [board segments]
  (println (map #(get-in board %) segments))
  (every? nil? (map #(get-in board %) segments)))

(defn down [{board :board piece :active-piece :as state}]
  (let [next-piece (update piece :segments #(map (fn [e] (update e 0 inc)) %))
        next-segments (:segments next-piece)]
    (if (and (every? in-bounds? next-segments) (no-conflicts? board next-segments))
      {:board (next-board board piece next-piece) :active-piece next-piece}
      state)))


;; downward translation logic
;; apply translate to segment
;; if translated is not out of bounds AND no-collision
  ;; commit translated segment and new board
;; else
  ;; commit new board and release segment, no more downward translations for this segment


;; ------------------------
;; Initialize app

(def game-state (r/atom {:board (->> (repeat 10 nil)
                                     (vec)
                                     (repeat 20)
                                     (vec))
                         :active-piece I-piece}))

(defn mount-root []
  (r/render [board-component game-state] (.getElementById js/document "app")))

(defn init! []
  (mount-root))

(go-loop []
         (<! (timeout 500))
         (let [next-state (down @game-state)]
           (if (= next-state @game-state)
             (swap! game-state assoc :active-piece I-piece)
             (reset! game-state next-state)))
         (recur))
