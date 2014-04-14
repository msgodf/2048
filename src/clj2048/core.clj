(ns clj2048.core)

;; Okay, so what is the minimum required for the game?

;; A grid
;;   some cells (an array of arrays - the elements are null at the start)
;;   insertTile,

(defn within-bounds
  [grid x y]
  (and (>= x 0)
       (< x (count (first grid)))
       (>= y 0)
       (< y (count grid))))

(defn get-from-grid
  [grid x y]
  (when (within-bounds grid x y)
    (-> grid
        (nth y)
        (nth x))))

(defn set-in-grid
  [grid x y value]
  (when (and (< x (count (first grid)))
             (< y (count grid)))
    (let [[before after] (split-at y
                                   grid)]
      (concat before
              (vector (let [[before after] (split-at x
                                                     (first after))]
                        (concat before
                                (vector value)
                                (rest after))))
              (rest after)))))

(defn make-grid
  [n]
  (repeat n
          (repeat n
                  {:previously-merged false
                   :value nil})))

(defn direction-to-vector
  [v]
  (condp = v
    :up {:x 0 :y -1}
    :right {:x 1 :y 0}
    :down {:x 0 :y 1}
    :left {:x -1 :y 0}))

(defn build-traversals
  [grid v]
  (let [xs (range (count (first grid)))
        ys (range (count grid))]
    {:x (if (= (:x v) 1)
          (reverse xs)
          xs)
     :y (if (= (:y v) 1)
          (reverse ys)
          ys)}))



(defn cell-available
  "Check whether the map at [x y] in the grid has a nil :value"
  [grid x y]
  (nil? (:value (get-from-grid grid x y))))


(defn find-farthest-position
  "Find the last cell either before the edge of the grid, or another tile, from the specified position in the direction specified by the vector"
  [grid ox oy {dx :x dy :y}]
  (let [[f b] (split-with (fn [[x y]] (and (within-bounds grid x y)
                                          (cell-available grid x y)))
                          (for [v (map inc (range))]
                            [(+ ox (* dx v))
                             (+ oy (* dy v))]))]
    (cond (= (count f) 0) {:farthest [ox oy] :next (first b)}
          :otherwise {:farthest (last f) :next (first b)})))

(defn can-merge
  [grid positions tile]
  (when (:next positions)
    (let [next-tile (get-from-grid grid
                                   (first (:next positions))
                                   (second (:next positions)))]
      (when next-tile
        (when (and next-tile
                   (= (:value tile)
                      (:value next-tile))
                   (not (:previously-merged next-tile)))
          next-tile)))))

(defn move-single
  [grid x y v]
  (let [tile (get-from-grid grid x y)]
    (if (not (nil? (:value tile)))
      (let [positions (find-farthest-position grid x y v)]
        (if positions
          (if-let [next-tile (can-merge grid positions tile)]
            (let [[nx ny] (:next positions)]
              ;; set the tile to be previously merged
              ;; set the tile position to be nil
              ;; update the next tile value and set it's previously merged to true
              (-> grid
                  (set-in-grid nx ny {:previously-merged true
                                      :value (+ (:value tile)
                                                (:value next-tile))})
                  (set-in-grid x y {:previously-merged false
                                    :value nil})))
            (let [[nx ny] (:farthest positions)]
              (-> grid
                  (set-in-grid nx ny {:previously-merged false
                                      :value (:value tile)})
                  (set-in-grid x y {:previously-merged false
                                    :value nil}))))
          grid))
      grid)))

(defn move
  [grid direction]
  (let [v (direction-to-vector direction)
        {xs :x ys :y} (build-traversals grid v)]
    (:grid (first
            (drop-while
             #(not-empty (:coordinates %))
             (iterate (fn [{:keys [grid coordinates] :as current}]
                        (if (empty? coordinates)
                          current
                          (let [[[x y] & rest] coordinates]
                            {:grid (move-single grid x y v)
                             :coordinates rest})))
                      {:grid grid
                       :coordinates (for [x xs y ys] [x y])}))))

    ;; actually, don't feed in the [x y] from the top, but from the bottom, along with the grid
    ;; because we're not generating an sequence of independent elements, but ones that are modifications of themselves
    ;; this should be broken out into a function that takes a grid, an [x y] pair, and a vector and produces a new
    ;; grid that is the result of a single move from that position
    ;; then we can call iterate on this sequence of coordinates (not reduce, as I still feel that the result should
    ;; look like the thing passed in, more than superficially)
    ))

;; Tiles on the grid
;; What does a move look like?
;; GameManager.prototype.move is the JS function that does this
;; takes a direction
;; gets a vector from the direction (0: up, 1: right, 2:down, 3:left)
;;  for 0 x=0,y=-1, 1 x=1, y=0 etc.
;; builds the 'traversals' from the vector - a list of positions to traverse in the right order
;;   - all the x values and all the y values in the grid x = [0,1,2,3] y=[0,1,2,3]
;;   - reverse the x and/or y values depending on the chosen direction (always traverse from the farthest cell)

;; saves the current tile positions, without merger information

;; traverses the grid in the 'right' direction, and move tiles
;; takes each of the x and y coordinates in the traversals as a pair [x,y] called a cell
;;   for each of these
;;     get the tile in the cell
;;     if there something there
;;       find the farthest position from the cell in the given direction
;;         -- 'progress toward the vector direction until an obstacle is found'
;;         -- returns a map {farthest: the last cell before an obstacle, next: the obstacle}
;;         -- the definition of 'obstacle' is that it stops when the cell is no longer in the bounds of the grid or available (doesn't contain any content - i.e. is null)
;;       gets the content of the 'next' cell in the returned map
;;       deals with merging - if there is something in the next cell and it has the same value AND next doesn't have
;;                            a 'mergedFrom' i.e. it isn't the result of a merge
;;         creates a new Tile object, in the position of the 'next' tile, and with double the value of the current tile
;;         it sets the mergedFrom value of this tile to be a pair of the current and 'next' tile
;;         it inserts the merged tile in the grid
;;         it removes the 'current' tile from the grid
;;         it updates the current tile's position to that of the 'next' tile
;;         it updates the score, adding the value of the merged tile
;;         it checks whether we've won (whether the merged tile has value 2048)
;;       if we're not merging, then it just moves the tile to the farthest position
;;         this involves setting the cell at the old position to null
;;         and setting the cell at the new position to contain the tile
;;         and calls 'updatePosition(cell)' on the tile
;;       finally, it checks whether the tile has moved by checking whether it is still in the cell, and sets a flag
;; if the moved flag is set
;;   a random tile is added
;;   it checks whether there are any available moves, and if not sets a 'game over' flag
;;      -- this is calculated as there are any empty cells
;;         or if not, whether there are any available matches between tiles
;;           -- this goes over all [x,y] pairs in the grid, and for each one if there is a tile in the cell
;;                (there must be if there are no available cells!)
;;                it checks all four directions to see whether there is an adjacent tile that has the same value
;;                  if it does, then there is an available move, and it exits immediately!
;;   calls actuate() - which redraws the board


(defn -main
  [& args]
  (println args))
