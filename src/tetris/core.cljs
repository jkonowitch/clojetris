(ns tetris.core
    (:use-macros [cljs.core.async.macros :only [go-loop go]])
    (:require
      [clojure.browser.event :as events]
      [reagent.core :as r]
      [cljs.core.async :refer [alts! <! timeout chan put! mult tap sliding-buffer]]))

;; ------------------------
;; Logic

(enable-console-print!)

(defn in-bounds? [[y x]]
  (and (<= 0 x 9) (<= 0 y 19)))

(def vec-repeat (comp vec repeat))

(defn no-conflicts?
  [board proposed-segments]
  (every? nil? (map #(get-in board %) proposed-segments)))

(defn piece->board
  [{{val :name keys :segments} :piece board :board}]
  (reduce #(assoc-in %1 %2 val) board keys))

(def piece<-board
  (comp piece->board #(assoc-in % [:piece :name] nil)))

(defn maybe-next-board
  [current-state next-piece]
  (let [clean-board (piece<-board current-state)]
    (when (no-conflicts? clean-board (:segments next-piece))
      (piece->board {:piece next-piece :board clean-board}))))

(defn next-state
  "Takes a segment transformation fn and the current state. Returns the next state.
   Simply returns the current state if the segment transformation is illegal."
  [f {piece :piece :as state}]
  (let [next-piece (update piece :segments f)]
    (if-let [next-board (and (every? in-bounds? (:segments next-piece))
                             (maybe-next-board state next-piece))]
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
  (let [blank-row (vec-repeat 10 nil)
        board-without-line (concat (subvec board 0 i)
                                   (subvec board (inc i)))
        new-board (into [blank-row] board-without-line)]
    (if (empty? idxs) new-board (recur idxs new-board))))

(defn on-deck-board [state]
  (let [board (->> (vec-repeat 4 nil)
                   (vec-repeat 3))]
    (piece->board (assoc state :board board :piece (first (:upcoming state))))))

;; -------------------------
;; Views

(defn cell-component [n i cell]
  (let [element (str "div.cell" (when cell (str ".piece." cell)))]
    ^{:key (str n i)} [(keyword element)]))

(defn row-component [n row]
  ^{:key n} [:div.row (map-indexed (partial cell-component n) row)])

(defn board-component [game-state]
  (let [board (:board @game-state)
        elt (if (:paused @game-state) :div.board.paused :div.board)]
    [:div.game
      [elt (map-indexed row-component board)]
      [:div.side
        [:div.on-deck (map-indexed row-component (on-deck-board @game-state))]
        [:div.score [:h3 [:span.title "Score"] [:br] 100]]
        [:div.instructions
          [:h4.title "Instructions"]
          [:p "Use the arrow keys" [:br] [:b "[P]"] "ause"]]]]))

;; ------------------------
;; Game State

(def tetronimos [{:name "I", :segments [[0 1] [0 0] [0 2] [0 3]]}
                 {:name "O", :segments [[0 0] [0 1] [1 0] [1 1]]}
                 {:name "T", :segments [[0 1] [0 0] [0 2] [1 1]]}
                 {:name "S", :segments [[0 1] [0 2] [1 0] [1 1]]}
                 {:name "Z", :segments [[0 1] [0 0] [1 1] [1 2]]}
                 {:name "J", :segments [[1 1] [0 0] [1 0] [1 2]]}
                 {:name "L", :segments [[1 1] [0 2] [1 0] [1 2]]}])

(def blank-board (->> (vec-repeat 10 nil)
                      (vec-repeat 20)))

(def starting-deck (shuffle tetronimos))

(def game-state (r/atom {:board blank-board
                         :upcoming (rest starting-deck)
                         :piece (first starting-deck)
                         :paused false}))

;; ------------------------
;; Stateful Operations

(defn next-piece! []
  (let [nxt (first (:upcoming @game-state))
        upcoming (or (not-empty (drop 1 (:upcoming @game-state)))
                     (shuffle tetronimos))]
    (swap! game-state assoc :upcoming upcoming :piece nxt)))

(defn clear-lines! []
  (when-let [lines (-> (lines-to-clear (:board @game-state))
                       (not-empty))]
    (swap! game-state update :board #(clear-lines lines %))))

;; ------------------------
;; Game/Input Loops

(def command-map {"ArrowUp" up
                  "ArrowDown" down
                  "ArrowLeft" left
                  "ArrowRight" right})

(defn piece-commands-loop [[_ pause-ch :as chs]]
  (go-loop []
    (let [[key ch] (alts! chs)
          command (get command-map key)]
      (when-not (= pause-ch ch)
        (swap! game-state merge (command @game-state))
        (recur)))))

(defn game-loop [pause-ch]
  (go-loop []
     (let [[_ ch] (alts! [pause-ch (timeout 500)])
           next-state (down @game-state)]
       (when-not (= pause-ch ch)
         (if (= next-state @game-state)
           (do (next-piece!) (clear-lines!))
           (swap! game-state merge next-state))
         (recur)))))

;; ------------------------
;; Initialize app

(defn mount-root []
 (r/render [board-component game-state] (.getElementById js/document "app")))

(defn init! []
 (mount-root))

;; User Input Channels

(def user-input-src (chan 1 (map #(.-key %))))
(events/listen (.querySelector js/document "body") "keydown" #(put! user-input-src %))

(def user-m (partial tap (mult user-input-src)))
(def piece-ch (user-m (chan (sliding-buffer 1) (filter (set (keys command-map))))))
(def interrupt-ch (user-m (chan 1 (filter #{"p"}))))

(let [interrupt-m (partial tap (mult interrupt-ch))
      unpause-ch (interrupt-m (chan 1 (keep-indexed #(if (odd? %1) %2))))
      pause-xf (keep-indexed #(if (even? %1) %2))
      pause-chs (repeatedly 3 #(interrupt-m (chan 1 pause-xf)))]

  (go-loop []
    (<! (first pause-chs))
    (swap! game-state assoc :paused true)
    (recur))

  (go-loop []
    (println "(Re)starting...")
    (game-loop (second pause-chs))
    (piece-commands-loop [piece-ch (last pause-chs)])
    (<! unpause-ch)
    (swap! game-state assoc :paused false)
    (recur)))
