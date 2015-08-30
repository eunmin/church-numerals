(ns church-numerals.core)

(defn church [n]
  (loop [n n
         result (fn [f x] x)]
    (if (zero? n)
      result
      (recur (dec n)
        (fn [f x]
          (f (result f x)))))))

(defn unchurch [n]
  (n inc 0))

(defn plus [m n]
  (fn [f x]
    (m f (n f x))))

(defn pred [n]
  (fn [f x]
    ((n (fn [g]
          (fn [h]
            (h (g f)))) (fn [u] x))
     (fn [u] u))))

(defn sub [m n]
  (n pred m))

(defn Y [m]
  ((fn [future]
     (future future))
   (fn [future]
     (m (fn [arg]
          ((future future) arg))))))

(defn dec+ [n]
  (sub n (church 1)))

(defn fib [f]
  (fn [n]
    (let [n (unchurch n)]
      (condp = n
        0 (church 0)
        1 (church 1)
        (plus (f (dec+ (church n)))
          (f (dec+ (dec+ (church n)))))))))

(defn -main [& args]
  (try
    (if-let [n (first args)]
      (println (unchurch ((Y fib) (church (read-string n)))))
      (throw (IllegalArgumentException.)))
    (catch Exception _
      (binding [*out* *err*]
        (println "Invalid argument")))))
