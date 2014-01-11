(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base n]
                 (cond
                   (= n 0)
                     1
                   (= n 1)
                     acc
                   :else
                     (recur (* acc base) base (dec n))))]
    (helper base base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc cur-seq]
                 (if (empty? cur-seq)
                   acc
                   (recur (first cur-seq) (rest cur-seq))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc cur-seq1 cur-seq2]
                 (cond
                   (or (not acc) (and (empty? cur-seq1) (not (empty? cur-seq2))) (and (not (empty? cur-seq1)) (empty? cur-seq2)))
                     false
                   (and (empty? cur-seq1) (empty? cur-seq2))
                     true
                   :else
                     (recur (= (first cur-seq1) (first cur-seq2)) (rest cur-seq1) (rest cur-seq2))))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         cur-seq a-seq
         pred pred]
    (cond
      (empty? cur-seq)
        nil
      (pred (first cur-seq))
        index
      :else
        (recur (inc index) (rest cur-seq) pred))))

;; throws for empty sequences
(defn avg [a-seq]
  (loop [sum 0
         length 0
         cur-seq a-seq]
    (if (empty? cur-seq)
      (/ sum length)
      (recur (+ sum (first cur-seq)) (inc length) (rest cur-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         cur-seq a-seq]
    (if (empty? cur-seq)
      acc
      (recur (toggle acc (first cur-seq)) (rest cur-seq)))))

(defn fast-fibo [n]
  (loop [x1 1
         x 1
         index 3]
    (cond
      (= n 0)
        0
      (= n 1)
        1
      (= n 2)
        1
      (= index n)
        (+ x1 x)
      :else
        (recur x (+ x1 x) (inc index)))))

(defn cut-at-repetition [a-seq]
  (loop [result []
         check-set #{}
         cur-seq a-seq]
    (let [check-set-length (count check-set)
          head (first cur-seq)
          check-set-with-head (conj check-set head)
          check-set-with-head-length (count check-set-with-head)
          tail (rest cur-seq)]
    (if (or (= check-set-length check-set-with-head-length) (empty? cur-seq))
      result
    (recur (conj result head) check-set-with-head tail)))))

