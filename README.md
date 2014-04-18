# 2048

A Clojure implementation of the game 2048

The original JavaScript version of the game is by Gabriele Cirulli (and others) and can be found at https://github.com/gabrielecirulli/2048

# Example

```clj
(defn example-game
  []
  (game 1 [:right :up :left :down :left]))
```

The game can also be played (albeit slowly, as this will start a new JVM each time) by using `lein run`

```bash
lein run 1 right up left down left
```
