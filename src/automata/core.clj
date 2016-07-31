(ns automata.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def cell-size 4)
(def width 500)
(def height 500)
(def cols (int (/ width cell-size)))
(def rows (int (/ height cell-size)))

(defn set-every-nth [coll p n]
  (into-array
    Short/TYPE
    (map-indexed
      #(if (zero? (mod (inc %1) n)) (short p) %2)
      coll)))

(defn fill-rand [col prob]
  (into-array
    Short/TYPE
    (doall
      (map
        (fn [el]
          (short (if (> (rand 1) prob) 1 0)))
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

(defn evolve-cell [coll idx stride]
  (let [n (neighbors coll idx stride)
        sum (:sum n)
        current (:center n)]
    (short
      (if (pos? current)
        (if (or (== sum 2) (== sum 3)) 1 0)
        (if (== sum 3) 1 0)))))

(defn compute-grid [cols rows grid]
  (let [w (- cols 1)
        h (- rows 2)
        len (* cols rows)
        buff (short-array len)]
    (loop [idx (+ cols 1), x 1, y 1]
        (if (< x w)
          (do
            (aset-short buff idx (evolve-cell grid idx cols))
            (recur (inc idx) (inc x) y))
          (if (< y h)
            (recur (+ idx 2) 1 (inc y))
            buff)))))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (q/hint :disable-texture-mipmaps)
  (let [len (* cols rows)
        data (short-array len)]
    {:cols cols
     :rows rows
     :grid (fill-rand data 0.8)
     :img  (q/create-image cols rows :rgb)
     }))

(defn update-state [state]
  (let [cols (:cols state)
        rows (:rows state)]
    (update-in state
               [:grid]
               (fn [grid] (compute-grid cols rows grid)))))

(defn bypass-update [state]
  (compute-grid 600 600 (:grid state))
  state)

(defn draw-state [state]
  (q/background 255)
  (let [cols (:cols state)
        rows (:rows state)
        grid (:grid state)
        img  (:img state)
        len  (* cols rows)
        pixels (q/pixels img)]
    (loop [i 0, idx 0]
        (if (< i len)
          (do (aset pixels idx (if (pos? (aget grid i)) 0xffffff 0x0000ff))
              (recur (inc i) (+ idx 1)))
          (do (q/update-pixels img)
              state)))
      (q/image img 0 0 width height))
  (println (str "framerate: " (q/current-frame-rate)))
  (println (str "timestamp: " (System/currentTimeMillis))))

(q/defsketch automata
  :title "2d cellular automata"
  :size [width height]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
