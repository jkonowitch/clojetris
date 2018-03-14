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
  (and (<= 0 x 9) (<= 0 y 21)))

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
   Returns nil if the segment transformation is illegal."
  [f {piece :piece :as state}]
  (let [next-piece (update piece :segments f)]
    (when-let [next-board (and (every? in-bounds? (:segments next-piece))
                               (maybe-next-board state next-piece))]
      {:board next-board :piece next-piece})))

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

(defn update-x [f] #(update % 1 f))

(defn update-y [f] #(update % 0 f))

(defn translation [f] (fn [state] (next-state (partial map f) state)))

(def down (translation (update-y inc)))

(def right (translation (update-x inc)))

(def left (translation (update-x dec)))

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

(def dec-3 #(- % 3))

(defn on-deck-board [piece]
  (let [board (->> (vec-repeat 4 nil)
                   (vec-repeat 3))
        translate-left (update piece :segments (partial map (update-x dec-3)))]
    (piece->board {:board board :piece translate-left})))

(def score-map [0 100 300 500 800])

(defn score-value [n-of-lines level]
  (* (nth score-map n-of-lines) level))

(defn level [total-lines]
  (-> (/ total-lines 10)
      (int)
      (inc)))
;; -------------------------
;; Views

(defn cell-component [n i cell]
  (let [element (str "div.cell" (when cell (str ".piece." cell)))]
    ^{:key (str n i)} [(keyword element)]))

(defn row-component [n row]
  ^{:key n} [:div.row (map-indexed (partial cell-component n) row)])

(defn side-component [state]
  [:div.side
    [:div.on-deck (map-indexed row-component (on-deck-board (first (:upcoming state))))]
    [:div.score [:h3 [:span.title "Score"] [:br] (:score state)]]
    [:div.level [:h3 [:span.title "Level"] [:br] (level (:total-lines state))]]
    [:div.instructions
      [:h4.title "Instructions"]
      [:p "Use the arrow keys" [:br] [:b "[P]"] "ause"]]])

(defn board-component [state]
  (let [board (:board @state)
        elt (cond
              (:game-over @state) :div.board.overlay.game-over
              (:paused @state) :div.board.overlay.paused
              :else :div.board)]
    [:div.game
      [elt (map-indexed row-component board)]
      (side-component @state)]))

;; ------------------------
;; Game State

(def tetronimos [{:name "I", :segments [[0 4] [0 3] [0 5] [0 6]]}
                 {:name "O", :segments [[0 3] [0 4] [1 3] [1 4]]}
                 {:name "T", :segments [[0 4] [0 3] [0 5] [1 4]]}
                 {:name "S", :segments [[0 4] [0 5] [1 3] [1 4]]}
                 {:name "Z", :segments [[0 4] [0 3] [1 4] [1 5]]}
                 {:name "J", :segments [[1 4] [0 3] [1 3] [1 5]]}
                 {:name "L", :segments [[1 4] [0 5] [1 3] [1 5]]}])

(def blank-board (->> (vec-repeat 10 nil)
                      (vec-repeat 22)))

(def starting-deck (shuffle tetronimos))

(def game-state (r/atom {:board blank-board
                         :upcoming (rest starting-deck)
                         :piece (first starting-deck)
                         :paused false
                         :game-over false
                         :total-lines 0
                         :score 0}))
;; ------------------------
;; Stateful Operations

(def game-over (chan))

(defn next-piece! []
  (let [nxt (first (:upcoming @game-state))
        upcoming (or (not-empty (drop 1 (:upcoming @game-state)))
                     (shuffle tetronimos))]
    (swap! game-state assoc :upcoming upcoming :piece nxt)
    (when-not (down @game-state) (go (put! game-over "")))))

(defn clear-lines! []
  (when-let [lines (-> (lines-to-clear (:board @game-state))
                       (not-empty))]
    (swap! game-state update :board #(clear-lines lines %))
    (swap! game-state update :score + (score-value (count lines) (level (:total-lines @game-state))))
    (swap! game-state update :total-lines + (count lines))))
;; ------------------------
;; Game/Input Loops

(def command-map {"ArrowUp" up
                  "ArrowDown" down
                  "ArrowLeft" left
                  "ArrowRight" right})

(defn piece-commands-loop [[piece-ch :as chs]]
  (go-loop []
    (let [[key ch] (alts! chs)
          command (get command-map key)]
      (when (= piece-ch ch)
        (swap! game-state merge (command @game-state))
        (recur)))))

; TODO - equation for a concave timeout curve
; timeout-ms = -1800 + (125000 / (1 + (level / (9.132303 * 10^-28)) ^ 0.06378964))

(defn tick-length [] (->> (level (:total-lines @game-state))
                          (* 25)
                          (- 525)))

(defn game-loop [chs]
  (go-loop []
     (let [t (timeout (tick-length))
           [_ ch] (alts! (conj chs t))]
       (when (= t ch)
         (if-let [next-state (down @game-state)]
           (swap! game-state merge next-state)
           (do (next-piece!) (clear-lines!)))
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
      pause-chs (repeatedly 3 #(interrupt-m (chan 1 pause-xf)))
      game-over-m (partial tap (mult game-over))
      game-over-chs (repeatedly 3 #(game-over-m (chan)))]

  (go-loop []
    (<! (first game-over-chs))
    (swap! game-state assoc :game-over true)
    (recur))

  (go-loop []
    (<! (first pause-chs))
    (swap! game-state assoc :paused true)
    (recur))

  (go-loop []
    (println "(Re)starting...")
    (game-loop [(second game-over-chs)(second pause-chs)])
    (piece-commands-loop [piece-ch (last game-over-chs) (last pause-chs)])
    (<! unpause-ch)
    (swap! game-state assoc :paused false)
    (recur)))
