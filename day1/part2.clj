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

;; Se ficar negativo soma 1 e dps eh so somar o floor da divisão pelo tamanho max (100)
(defn parte-2 [data]
  (-> (reduce (fn [[zeros start] value] 
                (let [end (+ start value )]
                  [(+ zeros
                      (abs (quot end tam))
                           (if (>= start 1 0 end) 1 0))
                      (mod end tam)]))
                [0 start]
                data)
      first))

(println (parte-2 data))
