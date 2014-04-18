(ns clj2048.core)

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
                  {:value nil})))

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

(defn available-cells
  [grid]
  (remove nil?
          (for [x (range (count (first grid)))
                y (range (count grid))]
            (when (cell-available grid x y)
              [x y]))))

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
  (random-item generator (available-cells grid)))

(defn find-farthest-position
  "Find the last cell either before the edge of the grid, or another tile, from the specified position in the direction specified by the vector"
  [grid ox oy {dx :x dy :y}]
  (let [[f b] (split-with (fn [[x y]] (and (within-bounds grid x y)
                                          (cell-available grid x y)))
                          (for [v (map inc (range))]
                            [(+ ox (* dx v))
                             (+ oy (* dy v))]))]
    (cond (= (count f) 0) {:farthest [ox oy]
                           :next (first b)}
          :otherwise {:farthest (last f)
                      :next (first b)})))

(defn can-merge
  [grid positions tile]
  (when (:next positions)
    (let [[x y] (:next positions)
          next-tile (get-from-grid grid  x y)]
      (when (and next-tile
                 (= (:value tile)
                    (:value next-tile))
                 (nil? (:previously-merged next-tile)))
        next-tile))))

(defn move-single
  [grid x y v]
  (let [tile (get-from-grid grid x y)]
    (if (:value tile)
      (let [positions (find-farthest-position grid x y v)]
        (if-let [next-tile (can-merge grid positions tile)]
          (let [[nx ny] (:next positions)]
            (-> grid
                (set-in-grid x y {:value nil})
                (set-in-grid nx ny {:previously-merged true
                                    :value (+ (:value tile)
                                              (:value next-tile))})))
          (let [[nx ny] (:farthest positions)]
            (-> grid
                (set-in-grid x y  {:value nil})
                (set-in-grid nx ny {:value (:value tile)})))))
      grid)))

(defn print-grid
  [grid]
  (println (clojure.string/join "\n"
                                (map #(clojure.string/join (map (fn [v] (if (:value v)
                                                                         (format "%5d     "
                                                                                 (:value v))
                                                                         "    -     "))
                                                                %))
                                     grid))))

(defn spawn
  [grid generator]
  (let [[x y] (random-available-cell generator grid)]
    (set-in-grid grid x y {:value (random-item generator
                                               (conj (repeat 9 2)
                                                     4))})))

(defn remove-merge-information
  [grid]
  (map #(map (fn [element] (dissoc element :previously-merged))
             %)
       grid))

(defn move
  [grid direction]
  (let [v (direction-to-vector direction)
        {xs :x ys :y} (build-traversals grid v)]
    (remove-merge-information (:grid (first
                                      (drop-while
                                       #(not-empty (:coordinates %))
                                       (iterate (fn [{:keys [grid coordinates] :as current}]
                                                  (if (empty? coordinates)
                                                    current
                                                    (let [[[x y] & rest] coordinates]
                                                      {:grid (move-single grid x y v)
                                                       :coordinates rest})))
                                                {:grid grid
                                                 :coordinates (for [x xs y ys] [x y])})))))))

(defn move-and-spawn
  [grid direction rng]
  (-> grid
      (move direction)
      (spawn rng)))

(defn example-game
  []
  (print-grid
   (let [rng (java.util.Random. 1)
         left #(move-and-spawn %1 :left rng)
         right #(move-and-spawn %1 :right rng)
         up #(move-and-spawn %1 :up rng)
         down #(move-and-spawn %1 :down rng)]
     (-> (make-grid 4)
         (spawn rng)
         (spawn rng)
         (right)
         (up)
         (left)
         (down)))))

(defn -main
  [& args]
  (println args))
