(ns mrsudoku.sat.encode
  (:require [midje.sweet :refer [fact]]
            [mrsudoku.grid :as g]
            ))


(def ex-grille @#'g/sudoku-grid)

(fact
  (g/cell ex-grille 1 1) => {:status :init, :value 5}
  (g/cell ex-grille 4 2) => {:status :init, :value 1}
  (g/cell ex-grille 9 9) => {:status :init, :value 9}
  (g/cell ex-grille 4 5) => {:status :init, :value 8}
  (g/cell ex-grille 4 6) => {:status :empty})

;; <<A DEFINIR>>
(declare log-binary)

(defn log-binary-1[nb]
  (if ( = nb 0 )
    '(0)
    (if ( = nb 1)
      '(1)
        (cons (int(mod nb 2)) (log-binary-1 (int (/ nb 2)))))))

(defn log-binary[nb]
  (reverse (log-binary-1 nb)))


(fact
  (log-binary 5) => '(1 0 1)
  (log-binary 9) => '(1 0 0 1)
  (log-binary 42) => '(1 0 1 0 1 0))


;; <<A DEFINIR>>
(declare pad-seq)

(defn list-to-vect[lis]
  (loop [a lis, res []]
    (if (seq a)
      (recur (rest a) (conj res (first a)))
      res)))

(list-to-vect (log-binary 1))

(defn pad-seq[pas ntr nbr]
  (if( < (count (list-to-vect pas)) nbr)
    (loop [res pas]
      (if (= 0 ( - nbr (count (list-to-vect res))))
        res
        (recur (cons ntr res))))
    pas))

(fact
  (pad-seq (log-binary 1) 0 4) => '(0 0 0 1)
  (pad-seq (log-binary 5) 0 4) => '(0 1 0 1)
  (pad-seq (log-binary 9) 0 4) => '(1 0 0 1)
  (pad-seq '(:a :b :c) :x 7) => '(:x :x :x :x :a :b :c))

(defn encode-value
  "Encode une valeur de cellule `n` (entre 1 et 9) en une séquence de 4 bits."
  [n]
  (pad-seq (log-binary n) 0 4))

(defn mkcellbit
  "Créer la variable du `bit` spécifié (entre 0 poids faible
   et 3 poids fort) pour la cellule située en `cx` (colonne)
  et `cy` (ligne)."
  [cx cy bit]
  (symbol (str "x" cx "y" cy "b" bit)))

(fact
  (mkcellbit 6 2 2) => 'x6y2b2
  (mkcellbit 4 3 0) => 'x4y3b0)


(defn mkcell [cx cy]
  (map #(mkcellbit cx cy %) (range 4)))


;; <<A DEFINIR>>
(declare encode-num)

(defn num-of-set [row] ;;fonciton qui associe les indexes
  (loop [a row , cpt (dec (count row)) , res []]
    (if (seq a)
      (recur (rest a) (dec cpt) (conj res [cpt (first a)]))
      res)))

(defn encode-num [cx cy v]
  (let [lis (list-to-vect (encode-value v))]
    (reduce (fn [res x]
              (if (= (second x) 0)
                (list 'and (list 'not (mkcellbit cx cy (first x))) res)
                 (list 'and (mkcellbit cx cy (first x)) res))) 'true (num-of-set lis))))

(num-of-set (list-to-vect (encode-value 5)))

(fact
  ;; bits '(0 1 0 1) donne x6y2b0 /\ (not x6y2b1) /\ x6y2b2 /\ (not x6y2 b3)
  (encode-num 6 2 5) => '(and x6y2b0 (and (not x6y2b1) (and x6y2b2
                                                            (and (not x6y2b3) true))))
  ;; bits '(0 0 0 1) donne x6y2b0 /\ (not x6y2b1) /\ (not x6y2b2) /\ (not x6y2 b3)
  (encode-num 2 3 1) => '(and x2y3b0 (and (not x2y3b1) (and (not x2y3b2)
                                                            (and (not x2y3b3) true))))
  ;; bits '(1 0 0 1) donne x6y2b0 /\ (not x6y2b1) /\ (not x6y2b2) /\ x6y2 b3)
  (encode-num 2 3 9) => '(and x2y3b0 (and (not x2y3b1) (and (not x2y3b2)
                                                            (and x2y3b3 true)))))
(defn encode-inits
  "Formule d'encodage des cellules déjà remplies dans la `grille`"
  [grille]
  (g/reduce-grid
   (fn [acc cx cy cell]
     (if (= (:status cell) :empty)
       acc
       (list 'and (encode-num cx cy (:value cell)) acc))) true grille))

;; <<A DEFINIR>>
(declare encode-vide)

(defn encode-vide [cx cy]
  (reduce (fn [res x]
           (list 'and (list 'not (encode-num cx cy x)) res)) 'true (conj (range 10 16) 0)))

(encode-vide 3 3)

(fact
 (encode-vide 7 3) ;; encodage d'une cellule vide en cx=7 et cy=3
 => '(or (and
          x7y3b0
          (and (not x7y3b1) (and (not x7y3b2) (and x7y3b3 true))))
         (or (and (not x7y3b0) (and (not x7y3b1) (and (not x7y3b2) (and x7y3b3 true))))
             (or (and x7y3b0 (and x7y3b1 (and x7y3b2 (and (not x7y3b3) true))))
                 (or (and (not x7y3b0) (and x7y3b1 (and x7y3b2 (and (not x7y3b3) true))))
                     (or (and x7y3b0 (and (not x7y3b1)
                                          (and x7y3b2 (and (not x7y3b3) true))))
                         (or (and (not x7y3b0)
                                  (and (not x7y3b1)
                                       (and x7y3b2 (and (not x7y3b3) true))))
                             (or (and x7y3b0 (and x7y3b1
                                                  (and (not x7y3b2)
                                                       (and (not x7y3b3) true))))
                                 (or (and (not x7y3b0) (and x7y3b1
                                                            (and (not x7y3b2)
                                                                 (and (not x7y3b3) true))))
                                     (or (and x7y3b0 (and (not x7y3b1)
                                                          (and (not x7y3b2)
                                                               (and (not x7y3b3) true))))
                                         false))))))))))

(defn encode-vides
  "Formule d'encodage des cellules vides de la `grille`."
  [grille]
  (g/reduce-grid
    (fn [acc cx cy cell]
      (if (= (:status cell) :empty)
        (list 'and (encode-vide cx cy) acc)
        acc)) true grille))

;; <<A DEFINIR>>
(declare distinct-empty-empty)

(defn mkcellbit2
  "Créer la variable du `bit` spécifié (entre 0 poids faible
   et 3 poids fort) pour la cellule située en `cx` (colonne)
  et `cy` (ligne)."
  [cx cy bit]
  (symbol (str "l" cy "c" cx "b" bit)))


(defn distinct-empty-empty [cx1 cy1 cx2 cy2]
  (loop [res 'false, cpt 0]
    (if (= cpt 4)
     res
     (recur (list 'or (list '<=> (mkcellbit2 cx1 cy1 cpt) (list 'not (mkcellbit2 cx2 cy2 cpt))) res) (inc cpt))
     )))

(fact
 ;; les cellules entre cx1=2,cy1=3 et cx2=2,cy=5 doivent être distinctes
 (distinct-empty-empty 2 3 2 5)
 => '(or (<=> l3c2b0 (not l5c2b0)) ; bits b0 distincts
          (or (<=> l3c2b1 (not l5c2b1)) ; b1
               (or (<=> l3c2b2 (not l5c2b2)) ; b2
                    (or (<=> l3c2b3 (not l5c2b3))  ; b3
                         false)))))

;; <<A DEFINIR>>
(declare distinct-filled-empty)

(defn distinct-filled-empty[cval cx2 cy2]
  (let [lis (list-to-vect (encode-value cval))]
    (reduce (fn [res x]
              (if (= (second x) 0)
                (list 'or (mkcellbit cx2 cy2 (first x)) res)
                (list 'or (list 'not (mkcellbit cx2 cy2 (first x))) res))) 'false (num-of-set lis))))

(fact
 ;; cval1=5  et cx2=3,cy2=6
 (distinct-filled-empty 5 3 6)
 => '(or (not x3y6b0) (or x3y6b1 (or (not x3y6b2) (or x3y6b3 false)))))

(defn distinct-pair [cx1 cy1 cell1 cx2 cy2 cell2]
  (cond
    ;; cas 1 : deux cellules vides
    (and (= (:status cell1) :empty) (= (:status cell2) :empty))
    (distinct-empty-empty cx1 cy1 cx2 cy2)
    ;; cas 2a : la première est vide
    (= (:status cell1) :empty)
    (distinct-filled-empty (:value cell2) cx1 cy1)
    ;; cas 2b : la seconde est vide
    (= (:status cell2) :empty)
    (distinct-filled-empty (:value cell1) cx2 cy2)
    ;; cas 3 : la formule n'est pas satisfiable
    (= (:value cell1) (:value cell2))
    false
    ;; cas par défaut : contrainte satisfaite
    :else true))

(fact
 (distinct-pair 2 3 {:status :empty} 5 6 {:status :empty})
 => '(and (<=> l3c2b0 (not l6c5b0))
          (and (<=> l3c2b1 (not l6c5b1))
               (and (<=> l3c2b2 (not l6c5b2))
                    (and (<=> l3c2b3 (not l6c5b3)) true))))
 (distinct-pair 2 3 {:status :init :value 5} 5 6 {:status :empty})
 => '(or (not x5y6b0) (or x5y6b1 (or (not x5y6b2) (or x5y6b3 false))))
 (distinct-pair 5 6  {:status :empty} 2 3 {:status :init :value 5})
 => '(or (not x5y6b0) (or x5y6b1 (or (not x5y6b2) (or x5y6b3 false))))
 (distinct-pair 2 3 {:status :init :value 5} 5 6 {:status :init :value 6})
 => true
 (distinct-pair 2 3 {:status :init :value 5} 5 6 {:status :init :value 5})
 => false)

;; <<A DEFINIR>>
(declare distinc-cells)

(defn distinct-cells [seqi]
  (reduce (fn[res x x2]
            (list 'and (distinct-pair (first x) (second x) (second (rest x)) (first x2) (second x2) (second (rest x2))) res)) '() seqi))

(defn distinct-celles1 [seqi elem]
  (loop [a seqi res '()]
    (if (seq a)
      (recur (rest a) (list (distinct-pair (first (first a)) (second (first a)) (second (rest (first a))) (first elem) (second elem) (second (rest elem))) res))
      res)))


(defn distinct-celles1 [seqi elem]
  (reduce (fn[res x]
           (list 'and (distinct-pair (first x) (second x) (second (rest x)) (first elem) (second elem) (second (rest elem))) res )) 'true seqi))


(distinct-celles1 '([1 1 {:status :empty}]
                   [2 1 {:status :init :value 5}]
                   [2 2 {:status :empty}]) [1 1 {:status :empty}])

(defn distinct-cells [seqi]
  (loop [a seqi res 'true]
    (if (seq a)
      ;;(if (contains? ([(first (first a)) (second (first a))] contains))
        (recur (rest a) (list 'and (distinct-celles1 (rest a) (first a)) res))
        res)))

(fact
 (distinct-cells '([1 1 {:status :empty}]
                   [2 1 {:status :init :value 5}]
                   [2 2 {:status :empty}]))
 => '(and true
          (and (and (or (not x2y2b0)
                        (or x2y2b1 (or (not x2y2b2) (or x2y2b3 false)))) true)
               (and (and (and (<=> l1c1b0 (not l2c2b0))
                              (and (<=> l1c1b1 (not l2c2b1))
                                   (and (<=> l1c1b2 (not l2c2b2))
                                        (and (<=> l1c1b3 (not l2c2b3)) true))))
                         (and (or (not x1y1b0) (or x1y1b1
                                                   (or (not x1y1b2) (or x1y1b3 false))))
                              true))
                    true))))


(defn fetch-row
  "Récupère les cellules de la ligne `cy` de la `grille`."
  [grille cy]
  (map (fn [cx cell]
         [cx cy cell]) (range 1 10) (g/row grille cy)))

(defn encode-rows
  "Formule d'encodage des contraintes de ligne dans la grille."
  [grille]
  (loop [cy 1, phi true]
    (if (<= cy 9)
      (recur (inc cy) (list 'and (distinct-cells (fetch-row grille cy))
                            phi))
      phi)))

(defn fetch-col
  "Récupère les cellules de la colonne `cx` de la `grille`."
  [grille cx]
  (map (fn [cy cell]
         [cx cy cell]) (range 1 10) (g/col grille cx)))

(defn encode-cols
  "Formule d'encodage des contraintes de colonne dans la grille."
  [grille]
  (loop [cx 1, phi true]
    (if (<= cx 9)
      (recur (inc cx) (list 'and (distinct-cells (fetch-col grille cx))
                            phi))
      phi)))

(defn block-rows
  "Séquence des coordonnées de ligne des cellules du block `b`"
  [b]
  (map #(+ (* (mod (dec b) 3) 3)
           1 (mod % 3)) (range 0 9)))

(defn block-cols
  [b]
  (map #(+ (quot % 3) 1
           (* (quot (dec b) 3) 3)) (range 0 9)))

(defn fetch-block
  "Récupère les cellules du block `b` de la `grille`."
  [grille b]
   (map vector (block-rows b) (block-cols b) (g/block grille b)))

(defn encode-blocks
  "Formule d'encodage des contraintes de bloc dans la grille."
  [grille]
  (loop [b 1, phi true]
    (if (<= b 9)
      (recur (inc b) (list 'and (distinct-cells (fetch-block grille b))
                           phi))
      phi)))

(defn encode-sudoku
  "Formule d'encodage de la grille du Sudoku."
  [grille]
  (list 'and
        (encode-inits grille)
        (list 'and (encode-vides grille)
              (list 'and (encode-rows grille)
                    (list 'and (encode-cols grille)
                          (encode-blocks grille))))))

;(count (encode-sudoku ex-grille))
;; => .... attention : formule énorme ! ....



;; Fonction qui permet de décoder la valeur d'une cellule à partir du dpll
(defn decode-value [cx cy m]
  (reduce
   +
   0
   (map
    #(if (get m %1) %2 0)
    (mkcell cx cy)
    (iterate #(* 2 %) 1))))
;; iterate permet d'obtenir les chiffres binaires
;; mkcell renvoit donc la liste des 4 bits pour la cellule cx et cy

(defn bin-to-normal[x]
   (loop [a x, m (take 4 (iterate #(* 2 %) 1)) , res 0]
     (if (seq a)
       (recur (rest a) (rest m) (+ ( * (first a)(first m)) res))
       res)))



;;(get a 'x1y1b1)

(take 4 (iterate #(* 2 %) 1))

(defn decode-block [m b]
  (let [cxb (inc (* (rem b 3) 3))
        cyb (inc (* 3 (quot b 3)))]
    (reduce
     #(let [cx (+ cxb (rem %2 3))
            cy (+ cyb (quot %2 3))
            val-cell (decode-value cx cy m)]
            ;; Il faut faire une vérification supplémentaire si c'est un solved ou un init

            (if (= (:status (g/cell ex-grille cx cy)) :empty)
              (conj %1 (g/mk-cell :solved val-cell))
              (conj %1 (g/mk-cell val-cell))))
     []
     (range 9))))

;;(decode-block a 4)

(defn decode-grid [m]
  (partition 3
             (for [x (range 9)]
               (decode-block m x))))




;(decode-grid (dpll/dcnf (encode-sudoku ex-grille)))
