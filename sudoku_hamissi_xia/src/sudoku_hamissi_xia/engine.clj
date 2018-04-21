(ns sudoku-hamissi-xia.engine
  (:use midje.sweet)
  (:require [sudoku-hamissi-xia.grid :as g]))

(def ^:private sudoku-grid (var-get #'g/sudoku-grid))

(defn values
  "Return the set of values of a vector or grid `cells`."
  [cells]

  (loop [cells cells, res #{}]
    (if (seq cells)
      (let [valeur (g/cell-value (first cells))]
        (if valeur
          (recur (rest cells) (conj res valeur))
          (recur (rest cells) res)))
      res)))

;  (reduce (fn [res x]
 ;           (let [valeur (g/cell-value x)]
  ;            (printf valeur)
   ;           (if valeur
    ;            (conj res valeur)))) #{} cells))

(fact
 (values (g/block sudoku-grid 1)) => #{5 3 6 9 8})

(fact
 (values (g/row sudoku-grid 1)) => #{5 3 7})

(fact
 (values (g/col sudoku-grid 1)) => #{5 6 8 4 7})

(fact
 (values (g/block sudoku-grid 8)) => #{4 1 9 8})

(fact
 (values (g/row sudoku-grid 8)) => #{4 1 9 5})

(fact
 (values (g/col sudoku-grid 8)) => #{6 8 7})


;;Pas tres tres optimisé dit donc
(defn values-except
  "Return the set of values of a vector of cells, except the `except`-th."
  [cells except]
  {:pre [(<= 1 except (count cells))]}
  (let [new (values cells)
        exept (get cells (dec except))
        valeur (g/cell-value exept)]
    (disj new valeur)))

(fact
 (values-except (g/block sudoku-grid 1) 1) => #{3 9 6 8})

(fact
 (values-except (g/block sudoku-grid 1) 4) => #{3 9 5 8})

(defn mk-conflict [kind value]
  {:status :conflict
   :kind kind
   :value value})

(defn merge-conflict-kind
  [kind1 kind2]
  (cond
    (and (set? kind1) (set? kind2)) (clojure.set/union kind1 kind2)
    (set? kind1) (conj kind1 kind2)
    (set? kind2) (conj kind2 kind1)
    (= kind1 kind2) kind1
    :else (hash-set kind1 kind2)))

;;condition 4
(fact
 (merge-conflict-kind :row :row) => :row)
;; condition 5
(fact
 (merge-conflict-kind :row :block) => #{:row :block})
;;condition 3
(fact
 (merge-conflict-kind :row #{:row :block}) => #{:row, :block})
;;condition 2
(fact
 (merge-conflict-kind #{:row :block} :block) => #{:row, :block})
;;condition 1
(fact
 (merge-conflict-kind #{:row :block} #{:block :col}) => #{:row :block :col})


(defn merge-conflict [conflict1 conflict2]
  (assoc conflict1 :kind (merge-conflict-kind (:kind conflict1) (:kind conflict2))))

(defn merge-conflicts [& conflicts]
  (apply (partial merge-with merge-conflict) conflicts))

(defn update-conflicts
  [conflict-kind cx cy value conflicts]
  (if-let [conflict (get conflicts [cx, cy])]
    (assoc conflicts [cx, cy] (mk-conflict (merge-conflict-kind conflict-kind (:kind conflict))
                                           cx cy value))
    (assoc conflicts [cx, cy] (mk-conflict conflict-kind cx cy value))))

(defn conflict-value [values except cell]
  (when-let [value (g/cell-value cell)]
    (when (and (not= (:status cell) :init)
               (contains? (values-except values except) value))
      value)))

;;Count est à utiliser avec les collections
;;First ainsi que second sont aussi pour les collections
(defn conflict-row-aux [valeur row cx cy]
  (loop [acomp (g/cell-value valeur)
    conflit 0
    row row
    cxt (inc cx)
    res {}]
    (if (seq row)
      ;;On saute les cases qui sont empty
      (if (= (compare (get (first row) :status) :empty) 0)
        (recur acomp conflit (rest row) (inc cxt) res)
        ;;Comparaison de valeur avec le premier map dans row
        (if (= (compare acomp (g/cell-value (first row))) 0)
          ;;Si la valeur est identique, il y a conflit on vérifie si ce sont des init ou des set, il faut donc récupérer les status
          ;;Si c'est un set
          (if (= (compare (get (first row) :status) :set) 0)
            (recur acomp (inc conflit) (rest row) (inc cxt) (conj res {[cxt cy] (mk-conflict :row acomp)}))
            (recur acomp (inc conflit) (rest row) (inc cxt) res))
          (recur acomp conflit (rest row) (inc cxt) res)))
      (if (and (not= conflit 0) (= (compare (get valeur :status) :set) 0))
        (conj res {[cx cy] (mk-conflict :row acomp)})
        res))))

(defn row-conflicts
  "Returns a map of conflicts in a `row`."
  [row cy]
  (loop [row row
         cx 1
         res {}]
    (if (seq row)
      (recur (rest row) (inc cx) (conj res (conflict-row-aux (first row) (rest row) cx cy)))
      res)))



(fact
 (row-conflicts (map #(g/mk-cell :set %) [1 2 3 4]) 1) => {})

(fact
 (row-conflicts (map #(g/mk-cell :set %) [1 2 3 1]) 1)
 => {[1 1] {:status :conflict, :kind :row, :value 1},
     [4 1] {:status :conflict, :kind :row, :value 1}})

(fact
 (row-conflicts [{:status :init, :value 8} {:status :empty} {:status :empty} {:status :empty} {:status :init, :value 6} {:status :set, :value 6} {:status :empty} {:status :empty} {:status :init, :value 3}] 4)
 => {[6 4] {:status :conflict, :kind :row, :value 6}})

(fact
 (row-conflicts [{:status :init, :value 8} {:status :empty} {:status :empty} {:status :empty} {:status :set, :value 6} {:status :init, :value 6} {:status :empty} {:status :empty} {:status :init, :value 3}] 4)
 => {[5 4] {:status :conflict, :kind :row, :value 6}})

(defn rows-conflicts [grid]
  (reduce merge-conflicts {}
          (map (fn [r] (row-conflicts (g/row grid r) r)) (range 1 10))))



;;Count est à utiliser avec les collections
;;First ainsi que second sont aussi pour les collections
(defn conflict-col-aux [valeur col cx cy]
  (loop [acomp (g/cell-value valeur)
    conflit 0
    col col
    cyt (inc cy)
    res {}]
    (if (seq col)
      ;;On saute les cases qui sont empty
      (if (= (compare (get (first col) :status) :empty) 0)
        (recur acomp conflit (rest col) (inc cyt) res)
        ;;Comparaison de valeur avec le premier map dans row
        (if (= (compare acomp (g/cell-value (first col))) 0)
          ;;Si la valeur est identique, il y a conflit on vérifie si ce sont des init ou des set, il faut donc récupérer les status
          ;;Si c'est un set
          (if (= (compare (get (first col) :status) :set) 0)
            (recur acomp (inc conflit) (rest col) (inc cyt) (conj res {[cx cyt] (mk-conflict :col acomp)}))
            (recur acomp (inc conflit) (rest col) (inc cyt) res))
          (recur acomp conflit (rest col) (inc cyt) res)))
      (if (and (not= conflit 0) (= (compare (get valeur :status) :set) 0))
        (conj res {[cx cy] (mk-conflict :col acomp)})
        res))))


(defn col-conflicts
  "Returns a map of conflicts in a `col`."
  [col cx]
  (loop [col col
         cy 1
         res {}]
    (if (seq col)
      (recur (rest col) (inc cy) (conj res (conflict-col-aux (first col) (rest col) cx cy)))
      res)))

;;; Ecrire les 'fact'  nécessaires...

(defn cols-conflicts
  [grid] (reduce merge-conflicts {}
                 (map (fn [c] (col-conflicts (g/col grid c) c)) (range 1 10))))


(defn block-conflicts-aux [c v valeurs block indices]
  (loop [valeurs valeurs
    conflit 0
    cinc c
    res {}]
    (if (seq valeurs)
      ;;Si les valeurs sont identiques, il y a un conflit
      (if (= v (first valeurs))
        (if (= (get (nth block cinc) :status) :set)
          (recur (rest valeurs) (inc conflit) (inc cinc) (conj res {(second (nth indices cinc)) (mk-conflict :block v)}))
          (recur (rest valeurs) (inc conflit) (inc cinc) res))
        (recur (rest valeurs) conflit (inc cinc) res))
      (if (and (not= conflit 0) (= (get (nth block c) :status) :set))
        (conj res {(second (nth indices c)) (mk-conflict :block v)})
        res))))

(defn block-conflicts
  [block b]

  ;;valeurs contient les valeurs dans un block
  ;;indices contient les indices dans le block
  (let [valeurs (g/reduce-block (fn [acc index cx cy cell]
                 (conj acc (g/cell-value cell))) [] block b)
        indices (g/reduce-block (fn [acc index cx cy cell]
                 (conj acc [index, [cx,cy]])) [] block b)]
    (loop [v valeurs
      coord 0
      res {}]
      (if (seq v)
        (recur (rest v) (inc coord) (conj res (block-conflicts-aux coord (first v) (rest v) block indices)))
        (do (println "La valeur de res c'est     " res)
          res)))))

;;; Ecrire les 'fact' nécessaires...

(defn blocks-conflicts
  [grid]
  (reduce merge-conflicts {}
          (map (fn [b] (block-conflicts (g/block grid b) b)) (range 1 10))))

(defn grid-conflicts
  "Compute all conflicts in the Sudoku grid."
  [grid]
  (merge-conflicts (rows-conflicts grid)
                   (cols-conflicts grid)
                   (blocks-conflicts grid)))
