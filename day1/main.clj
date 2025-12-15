(require '[clojure.string :as str])


(defn map-lines [f input-str]
  (->> input-str
       str/split-lines
       (map f)))

(defn parse [line]
  (* (if (= \L (first line)) -1 1)
       (parse-long (subs line 1))))

(defn parse-input [input] 
  (map-lines parse input)) 

(def teste "L12
L20
R40
R5
L60")

(def teste-testado (parse-input teste))

(def data (->> (slurp "inputs/day1.txt") (parse-input)))

(def tam 100)

(def start 50)

(defn parte-1 [data]
  (-> (reduce (fn [[zeros start] value] 
                (let [end (mod (+ start value ) tam)]
                  [(if (= 0 end) (inc zeros) zeros)
                   end]))
              [0 start]
              data)
      first))

(println (parte-1 data))
