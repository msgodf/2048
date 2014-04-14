# 2048

A Clojure implementation of the game 2048

The original JavaScript version of the game is by Gabriele Cirulli (and others) and can be found at https://github.com/gabrielecirulli/2048

# Example

```
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
```
