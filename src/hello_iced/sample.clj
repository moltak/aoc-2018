(ns hello-iced.sample)

(defn toggle-grow [direction]
	(if (= direction :small) :big :small))

(toggle-grow :big)
(toggle-grow :small)

(defn oh-my [direction] 
	(str "Oh my! You are growing " direction))

(oh-my :big)

(oh-my (toggle-grow :small))

(defn surprise [direction] 
  ((comp oh-my toggle-grow) direction))

(surprise :small)


(def adjs ["normal" "too small" "too big" "is swimming"])
(defn alice-is [in out]
    (if (empty? in) ;
      out ;
      (alice-is ;
        (rest in) ;
        (conj out ;
          (str "Alice is " (first in))))))

(alice-is adjs [])

(defn alice-is2 [input]
  (loop [in input out []]
    (if (empty? in)
      out
      (recur (rest in) ;
             (conj out (str "Alice is " (first in)))))))

(alice-is2 adjs)

(count (take 10 (range)))

(let [[color size] ["blue" "small"]]
	(str "The " color " door is " size))

(let [[color [size]] ["blue" ["very small"]]]
  (str "The " color " door is " size))

(let [{flower1 :flower1 flower2 :flower2 :or {flower2 "missing"}} {:flower1 "red"}]
  (str "The flowers are " flower1 " and " flower2))

(defn flower-colors [colors]
  (str "The flowers are "
       (:flower1 colors)
       " and "
       (:flower2 colors)))

(flower-colors {:flower1 "red"})
