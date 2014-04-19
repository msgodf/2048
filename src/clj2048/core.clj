(ns clj2048.core)

(defn within-bounds
  [grid [x y]]
  (and (>= x 0)
       (< x (count (first grid)))
       (>= y 0)
       (< y (count grid))))

(defn get-from-grid
  [grid [x y]]
  {:pre [(within-bounds grid [x y])]}
  (-> grid
      (nth y)
      (nth x)))

(defn set-in-grid
  [grid [x y] value]
  {:pre [ (within-bounds grid [x y])]}
  (let [[before after] (split-at y
                                 grid)]
    (concat before
            (vector (let [[before after] (split-at x
                                                   (first after))]
                      (concat before
                              (vector value)
                              (next after))))
            (next after))))

(defn make-grid
  [n]
  (repeat n
          (repeat n
                  nil)))

(defn direction-to-vector
  [v]
  (condp = v
    :up {:x 0 :y -1}
    :right {:x 1 :y 0}
    :down {:x 0 :y 1}
    :left {:x -1 :y 0}))

(defn build-traversals
  [grid direction]
  (let [{:keys [x y]} (direction-to-vector direction)
        xs (range (count (first grid)))
        ys (range (count grid))]
    {:x (if (= x 1)
          (reverse xs)
          xs)
     :y (if (= y 1)
          (reverse ys)
          ys)}))

(defn cell-available
  "Check whether the map at [x y] in the grid has a nil :value"
  [grid pos]
  (nil? (get-from-grid grid pos)))

(defn available-cells
  [grid]
  (filter (fn [pos] (cell-available grid pos))
          (for [x (range (count (first grid)))
                y (range (count grid))]
            [x y])))

;; from data.generators, but with a java.util.Random as the first argument
(defn uniform
  "Uniform distribution from lo (inclusive) to high (exclusive)."
  (^long [rnd lo hi] {:pre [(< lo hi)]}
        (clojure.core/long (Math/floor (+ lo (* (.nextDouble rnd) (- hi lo)))))))

(defn random-item
  [generator coll]
  (nth coll (uniform generator 0 (count coll))))

(defn random-available-cell
  [generator grid]
  (random-item generator
               (available-cells grid)))

(defn find-farthest-position
  "Find the last cell either before the edge of the grid, or another tile, from the specified position in the direction specified by the vector"
  [grid [ox oy] direction]
  (let [{dx :x dy :y} (direction-to-vector direction)
        [f b] (split-with (fn [pos] (and (within-bounds grid pos)
                                        (cell-available grid pos)))
                          (for [v (map inc (range))]
                            [(+ ox (* dx v))
                             (+ oy (* dy v))]))]
    {:farthest (or (last f)
                   [ox oy])
     :next (first b)}))

(defn can-merge
  [grid merged-tiles positions tile]
  {:pre [(:next positions)]}
  (let [pos (:next positions)]
    (when (within-bounds grid pos)
      (let [next-tile (get-from-grid grid pos)]
        (when (and next-tile
                   (= tile next-tile)
                   (not (some #{pos}
                              merged-tiles)))
          next-tile)))))

(defn print-grid
  [grid]
  (println (clojure.string/join "\n"
                                (map #(clojure.string/join (map (fn [v] (if v
                                                                         (format "%5d     "
                                                                                 v)
                                                                         "    -     "))
                                                                %))
                                     grid))))

(defn spawn
  [grid generator]
  (let [pos (random-available-cell generator grid)]
    (set-in-grid grid
                 pos
                 (random-item generator
                              (conj (repeat 9 2)
                                    4)))))

(defn move-single
  [grid merged-tiles pos direction]
  (if-let [tile (get-from-grid grid pos)]
    (let [positions (find-farthest-position grid pos direction)]
      (if-let [next-tile (can-merge grid merged-tiles positions tile)]
        {:grid (-> grid
                   (set-in-grid pos nil)
                   (set-in-grid (:next positions)
                                (+ tile
                                   next-tile)))
         :merged-tiles (conj merged-tiles
                             (:next positions))}
        {:grid (-> grid
                   (set-in-grid pos nil)
                   (set-in-grid (:farthest positions)
                                tile))
         :merged-tiles merged-tiles}))
    {:grid grid
     :merged-tiles merged-tiles}))

(defn move
  [grid direction]
  (:grid (first
          (drop-while
           #(not-empty (:coordinates %))
           (iterate (fn [{:keys [grid coordinates merged-tiles] :as current}]
                      (if (empty? coordinates)
                        current
                        (let [[pos & rest] coordinates
                              {:keys [grid merged-tiles]} (move-single grid
                                                                       merged-tiles
                                                                       pos
                                                                       direction)]
                          {:grid grid
                           :merged-tiles merged-tiles
                           :coordinates rest})))
                    {:grid grid
                     :merged-tiles #{}
                     :coordinates (let [{xs :x ys :y} (build-traversals grid
                                                                        direction)]
                                    (for [x xs y ys] [x y]))})))))

(defn move-and-spawn
  [grid direction rng]
  (-> grid
      (move direction)
      (spawn rng)))

(defn apply-moves
  [grid rng moves]
  (reduce (fn [grid i] (move-and-spawn grid i rng))
          grid
          moves))

(defn game
  [seed moves]
  (print-grid
   (let [rng (java.util.Random. seed)]
     (-> (make-grid 4)
         (spawn rng)
         (spawn rng)
         (apply-moves rng
                      moves)))))

(defn example-game
  []
  (game 1 [:right :up :left :down :left]))

(defn -main
  [& args]
  (if (empty? args)
    (example-game)
    (game (Integer/parseInt (first args))
          (map keyword (rest args)))))
