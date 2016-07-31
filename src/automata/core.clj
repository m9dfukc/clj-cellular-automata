(ns automata.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def cell-size 5)
(def width 500)
(def height 500)
(def cols (int (/ width cell-size)))
(def rows (int (/ height cell-size)))

(defn fill-rand [col prob]
  (into-array
      Byte/TYPE
      (doall
        (map
          (fn [_]
            (if (> (rand 1) prob) 1 0))
          col))))

(defn neighbors [coll idx stride]
  (let [t (- idx stride)
        b (+ idx stride)
        left (aget coll (- idx 1))
        center (aget coll idx)
        right (aget coll (+ idx 1))
        top-left (aget coll (- t 1))
        top (aget coll t)
        top-right (aget coll (+ t 1))
        bottom-left (aget coll (- b 1))
        bottom (aget coll b)
        bottom-right (aget coll (+ b 1))]
    {:left left
     :center center
     :right right
     :top-left top-left
     :top top
     :top-right top-right
     :bottom-left bottom-left
     :bottom bottom
     :bottom-right bottom-right
     :sum (+ left right top-left top top-right bottom-left bottom bottom-right)}))

(defn sum-neighbors [coll idx stride]
   (let [t (- idx stride)
        b (+ idx stride)]
     (+
       (aget coll (- idx 1))
       (aget coll idx)
       (aget coll (+ idx 1))
       (aget coll (- t 1))
       (aget coll t)
       (aget coll (+ t 1))
       (aget coll (- b 1))
       (aget coll b)
       (aget coll (+ b 1)))))

(defn evolve-cell [coll idx stride]
  (let [n (neighbors coll idx stride)
        sum (:sum n)
        current (:center n)]
    (if (pos? current)
      (if (or (= sum 2) (= sum 3)) 1 0)
      (if (= sum 3) 1 0))))

(defn compute-grid [cols rows grid]
  (let [w (- cols 1)
        h (- rows 2)
        len (* cols rows)
        buff (byte-array len)
        ]
    (loop [idx (+ cols 1), x 1, y 1]
        (if (< x w)
          (do
            (aset-byte buff idx (evolve-cell grid idx cols))
            (recur (inc idx) (inc x) y))
          (if (< y h)
            (recur (+ idx 2) 1 (inc y))
            buff)))))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (q/hint :disable-texture-mipmaps)
  (let [len (* cols rows)
        data (byte-array len)]
    {:cols cols
     :rows rows
     :grid (fill-rand data 0.8)
     :img  (q/create-image cols rows :rgb)}))

(defn update-state [state]
  (let [then (System/currentTimeMillis)
        cols (:cols state)
        rows (:rows state)
        new-state (update-in state
                             [:grid]
                             (fn [grid] (compute-grid cols rows grid)))]
    (println (str "compute grid: " (- (System/currentTimeMillis) then) "ms"))
    new-state))

(defn draw-state [state]
  (q/background 255)
  (let [then (System/currentTimeMillis)
        cols (:cols state)
        rows (:rows state)
        grid (:grid state)
        img  (:img state)
        len  (* cols rows)
        pixels (q/pixels img)]
    (loop [i 0, idx 0]
        (if (< i len)
          (do
            (aset pixels idx (if (pos? (aget grid i)) 0xffffff 0x0000ff))
            (recur (inc i) (+ idx 1)))
          state))
    (q/update-pixels img)
    (q/image img 0 0 width height)
    (println (str "draw grid: " (- (System/currentTimeMillis) then) "ms"))
    state))

(q/defsketch automata
  :title "2d cellular automata"
  :size [width height]
  :settings #(q/smooth 0)
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
