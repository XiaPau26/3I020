(ns sudoku-hamissi-xia.core
  (:gen-class)
  (:use [clojure.core.match :only [match]])
  (:require [clojure.set :as set]
            [sudoku-hamissi-xia.grid :as g]
            [midje.sweet :refer [fact]]))


(declare xor)
(declare filter-trivial)
(not true)
(not false)
(and true false)
(and true true)
(or true false)
(or false false)
(declare resolution)
(declare distinct-cells)

(defn -main [& args]
  (distinct-cells '([1 1 {:status :empty}]
                   [2 1 {:status :init :value 5}]
                   [2 2 {:status :empty}]))
)

;; a et b sont des variables propositionnelles
(defn xor [a b]
  (and (or a b)
       (not (and a b))))
(xor true false)
(xor true true)

(defn venum-aux [bools]
  (list (conj bools false) (conj bools true)))

(venum-aux [false, false, false])
(venum-aux [])

(defn venum
  "Retourne la partie gauche de la table de vérité pour `nb` variables."
  [nb]
  (cond
    (zero? nb) ()
    (= nb 1) (venum-aux [])
    :else
    (let [tbl (venum (dec nb))]
      (reduce concat (map venum-aux tbl)))))

(venum 1)
(venum 5)
(count (venum 10))

(defn partie-gauche [variables]
  (let [values (venum (count variables))
        vvalues (map (fn [enum]
                       (apply array-map
                              (interleave variables enum))) values)]
    vvalues))

(partie-gauche [:a :b :c])

(defn table-verite-aux [variables prop]
  (let [gauche (partie-gauche variables)]
    (map (fn [vmap]
           [vmap (apply prop (vals vmap))]) gauche)))

(table-verite-aux '[a b] xor)
(table-verite-aux '[a b] #(or %1 %2))

(defn table-verite [prop-var]
  (if-let [op-meta (meta prop-var)]
    (table-verite-aux (first (:arglists op-meta)) prop-var)
    (throw (ex-info "Impossible de construire la table: pas de meta" prop-var))))

(table-verite #'xor)

(defn nand [a b]
  (not (and a b)))

(table-verite #'nand)

(defn my-or [a b]
  (or a b))

(table-verite #'my-or)

(defn tiers-exclu [a]
  (or a (not a)))

(table-verite #'tiers-exclu)

(defn ==> [a b]
  (or (not a) b))

(defn <==> [a b]
  (and (or (not a) b)
       (or (not b) a)))

(<==> true true)
(<==> false true)
(<==> true false)
(<==> false false)

(defn modus-ponens [a b]
  (==> (and (==> a b) a)
       b))

(table-verite #'modus-ponens)

(defn tautologie-aux? [tbl]
  (every? true? (map second tbl)))

(tautologie-aux? (table-verite #'modus-ponens))

(defn tautologie? [prop]
  (tautologie-aux? (table-verite prop)))

(tautologie? #'tiers-exclu)
(tautologie? #'modus-ponens)
(tautologie? #'==>)

(defn neant [a]
  (and a (not a)))

(table-verite #'neant)

(defn satisfiable-aux? [tbl]
  (or (some true? (map second tbl))
      false))

(satisfiable-aux? (table-verite #'neant))
(satisfiable-aux? (table-verite #'modus-ponens))
(satisfiable-aux? (table-verite #'==>))

(defn satisfiable? [prop]
  (satisfiable-aux? (table-verite prop)))

(satisfiable? #'modus-ponens)



;;; # SAT Partie 2 : manipulations de formules propositionnelles

;;; Ne pas oublier la dépendance suivante dans project.clj
;;; [org.clojure/core.match "0.3.0-alpha5"]


;; EXERCICE : ajouter  l'implication et l'équivalence dans tout ce qui suit

'(==> a b)
'(<=> a b)

;;; ## Simplifications des formules

(defn simplify-one [f]
  (match f
         ;; *** simplification du not ***
         ;; (not true) -> false
         (['not true] :seq) false
         ;; (not false) -> true
         (['not false] :seq) true
         ;; (not (not a)) -> a
         (['not (['not a] :seq)] :seq) a
         ;; *** simplification du or ***
         ;; (or true a) -> true
         (['or true a] :seq) true
         ;; (or a true) -> true
         (['or a true] :seq) true
         ;; (or false a) -> a
         (['or false a] :seq) a
         ;; (or a false) -> a
         (['or a false] :seq) a
         ;; *** simplification du and ***
         ;; (and true a) -> a
         (['and true a] :seq) a
         ;; (and a true) -> a
         (['and a true] :seq) a
         ;; (and false a) -> false
         (['and false a] :seq) false
         ;; (and a false) -> false
         (['and a false] :seq) false

         ;; *** Simplification de l'implication ***
         (['==> false a] :seq) true
         (['==> true a] :seq) a

         ;; *** Simplification de l'equivalence ***
         (['<=> true true] :seq) true
         (['<=> false false] :seq) true
         (['<=> true false] :seq) false
         (['<=> false true] :seq) false



         :else f))

(simplify-one '(not true))
(simplify-one '(not false))
(simplify-one '(not (not true)))
(simplify-one '(not (or true (not false))))
(simplify-one '(==> true true))
(simplify-one '(==> false true))
(simplify-one '(==> false false))
(simplify-one '(==> true false))
(simplify-one '(<=> true true))
(simplify-one '(<=> false true))


;;Permet de retourner une formule simplifiée
(defn simplify [f]
  (match f
         ([op a] :seq) (simplify-one (list op (simplify a)))
         ([op a b] :seq) (simplify-one (list op (simplify a)
                                             (simplify b)))
         :else f))

(simplify '(or (not (or (not true)
                        (and (or (not x) false)
                             (or (not false) x))))
               (not (or y (and false z)))))

(simplify '(not (or true (not false))))
(simplify '(==> (not true) true))
(simplify '(<=> (not true) (not true)))
(simplify '(<=> (or false a) (and false true)))
(simplify '(and a (not a)))

;;; ## Forme normale nenf
;; ###Cette nouvelle forme permet de remplacer la forme nnf où la formule peut exploser, le principe consiste donc à faire descendre les not à la racine de l'arbre

(defn nenf' [f]
  (match f
         ;; not .. and
         (['not (['and a b] :seq)] :seq)
         (list 'or (nenf' (list 'not  a)) (nenf' (list 'not b)))
         ;; not .. or
         (['not (['or a b] :seq)] :seq)
         (list 'and (nenf' (list 'not  a)) (nenf' (list 'not b)))
         ;: not .. not
         (['not (['not a] :seq)] :seq) (nenf' a)
         ;; and ..
         (['and a b] :seq) (list 'and (nenf' a) (nenf' b))
         ;; or ..
         (['or a b] :seq) (list 'or (nenf' a) (nenf' b))
         ;; TODO ==>
         (['==> a b] :seq) (list '==> (nenf' a) (nenf' b))
         ;;et <=>
         (['<=> a b] :seq) (list '<=> (nenf' a) (nenf' b))
         ;; <=>
         (['not (['<=> a b] :seq)] :seq) ('<=> (nenf' a) (nenf' (list 'not b)))
         :else f))

(nenf' '(==> (or true (not false))))
(nenf' '(or (not (or (not true)
                     (and (or (not x) false)
                          (or (not false) x))))
            (not (or y (and false z)))))

(simplify '(or (<=> (or (not true)
                        (and (or (not x) false)
                             (or (not false) x))))
               (not (or y (and false z)))))

(simplify (nenf' '(or (not (or (not true)
                               (and (or (not x) false)
                                    (or (not false) x))))
                      (not (or y (and false z))))))

(defn nenf [f]
  (nenf' (simplify f)))

(nenf '(or (not (or (not true)
                    (and (or (not x) false)
                         (or (not false) x))))
           (not (or y (and false z)))))

;;; ## Forme normale disjonctive DNF

;;Distributivité pour le and ==> ((a+b) . c) = (a.c) + (b.c)
(defn distrib [f]
  (match f
         (['and (['or a b] :seq) c] :seq)
         (list 'or (distrib (list 'and a c))
               (distrib (list 'and b c)))
         (['and a (['or b c] :seq)] :seq)
         (list 'or (distrib (list 'and a b))
               (distrib (list 'and a c)))
         (['or (['and a b] :seq) c] :seq)
         (list 'and (distrib (list 'or a c))
               (distrib (list 'or b c)))
         (['or a (['and b c] :seq)] :seq)
         (list 'and (distrib (list 'or a b))
               (distrib (list 'or a c)))
         :else f))


(distrib '(or a (and b c)))
(distrib '(or (and b c) a))

;;il faut donner une priorité dans l'ordre de la distributivité ?
(distrib '(or (and (or a b) c) d))


;; Remarque : f doit être en nenf
(defn dnf' [f]
  (match f
         (['and a b] :seq) (distrib (list 'and (dnf' a)
                                          (dnf' b)))
         (['or a b] :seq) (list 'or (dnf' a)
                                (dnf' b))
         :else f))


(declare filter-trivial)

;;faire un filter-trivial récursif ==> pas besoin de le rendre récursif, il suffit de l'appliquer à chaque nouvel appel de la fonction "list"

(defn dnf [f]
  (dnf' (nenf f)))


;;Il faut représenter le dnf avec des ensemble
(dnf '(and (or a (and b c)) (or (not a) (not c))))
(dnf '(and a (and a (and (not b ) (not b)))))


'(or (or (and a (not a)) (and a (not c))) (or (and (and b c) (not a)) (and (and b c) (not c))))

;;; Problème : c'est pas lisible et c'est simplifiable
;;; Solution : représentation sous forme d'ensemble (conjonctif) de clauses (disjonctives)


;;On transforme chaque clause de la forme and dans un enesemble
(defn setify-and [f]
  (match f
         (['and a b] :seq)
         (set/union (setify-and a) (setify-and b))
         :else #{f}))

(setify-and '(and a (and a (and (not b ) (not b)))))


;;On affiche la forme dnf ou chaque clause est affichée sous forme d'un ensemble
(defn setify-dnf [f]
  (match f
         (['and a b] :seq) #{(setify-and f)}
         (['or a b] :seq) (set/union (setify-dnf a) (setify-dnf b))
         ([==> a b] :seq) (set/union (setify-dnf a) (setify-dnf b))
         ([<=> a b] :seq) (set/union (setify-dnf a) (setify-dnf b))
         :else #{#{f}}))

(setify-dnf
  '(==> (or (<=> a (not a)) (and a (not c))) (or (and (and b c) (not a)) (and (and b c) (not c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EXO 1


;;Si on ne fait pas les trois premières du match, il suffit de transformer a et b en string en faisant (str a)
(defn filter-trivial [f]
  (if (> (count f) 3)
    f
    (match f
           (['and a false] :seq) false

           (['or a false] :seq) a
           (['or a true] :seq) true
           (['and a (['not b] :seq)] :seq) (if (= (compare a b) 0)
                                             false
                                             f)
           (['or a (['not b] :seq)] :seq) (if (= (compare a b) 0)
                                            true
                                            f)
           (['and a b] :seq) (if (= (compare a b) 0)
                               a
                               f)
           (['or a b] :seq)  (if (= (compare a b) 0)
                               a
                               f)
           :else f)))

(defn filter-aux [f]
  ;;recursion sur f pour simplifier
  (match f
         (['or a b] :seq) (filter-trivial (list 'or (filter-aux a) (filter-aux b)))
         (['and a b] :seq) (filter-trivial (list 'and (filter-aux a) (filter-aux b)))
         :else f))


(filter-aux '(and a (and a (not a))))
(filter-aux '(or a (and a a)))
(filter-aux '(or a (not a)))
(filter-aux '(or a (not b)))
(filter-aux '(and a (not b)))
(filter-aux '(and a a))
(filter-aux '(or a a))
(filter-aux '(and a b))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EXO 2
;;(defn filter-subsume[f]
;;  (filter (f[x])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EXO 3
;;Transformer un formule avec des and et des or par des ensembles d'ensembles

;;On transforme chaque clause de la forme and dans un enesemble
(defn setify-or [f]
  (match f
         (['or a b] :seq)
         (set/union (setify-or a) (setify-or b))
         :else #{f}))

(setify-or '(or a (or a (or (not b ) (not b)))))

;;On affiche la forme cnf ou chaque clause est affichée sous forme d'un ensemble
(defn setify-cnf [f]
  (match f
         (['or a b] :seq) #{(setify-or f)}
         (['and a b] :seq) (set/union (setify-cnf a) (setify-cnf b))


         ;;([==> a b] :seq) (set/union (setify-cnf a) (setify-cnf b))
         ;;([<=> a b] :seq) (set/union (setify-cnf a) (setify-cnf b))
         :else #{#{f}}))


(setify-cnf '(<=> y (or a b)))


;; Remarque : f doit être en nenf
(defn cnf' [f]
  (match f
         (['or a b] :seq) (distrib (list 'or (cnf' a)(cnf' b)))
         (['and a b] :seq) (list 'and (cnf' a)(cnf' b))
         :else f))

(defn cnf [f]
  (cnf' (nenf f)))


(cnf '(or a (and b c)))
(cnf '(and (or a (and b c)) (or (not a) (not c))))
(cnf '(or a (or a (and (not b ) (not b)))))

(cnf '(or (and a b) (or c e)))



;;Pour réaliser la cnf à partir de la dnf, il faut réaliser réaliser un not sur le dnf
(defn cnf-aux' [f]
  (nenf (list 'not (dnf (list 'not f)))))

(cnf-aux' '(or a (and b c)))
(cnf-aux' '(and (or a (and b c)) (or (not a) (not c))))
(cnf-aux' '(and a (or a (and (not b) (not b)))))



(defn cnf-dnf [f]
  (let [dnf (setify-dnf f)]
    dnf))





(defn dcnf-aux [f equivs]
  (match f
         ([op a b] :seq)
         (let [[a', equivs1] (if (symbol? a) [a equivs] (dcnf-aux a equivs))
               [b', equivs2] (if (symbol? b) [b equivs] (dcnf-aux b equivs1))
               f' (list op a' b')]
           (if-let [eq (get equivs2 f')]
             [eq equivs2]
             ;;si on a pas trouvé la formule
             (let [v (symbol (str "$"(inc (count equivs2))))]
               [v (assoc equivs2 f' v)])))))



(defn dcfn [f]
  (dcnf-aux f))

(dcnf-aux '(and a b) {})



(defn make-true1 [clause x]
  (let [not-x (list 'not x)]
    (cond
      (contains? clause x) true
      (contains? clause not-x) (let [clause' (disj clause not-x)]
                                 (if (empty? clause)
                                   nil
                                   clause'))
      :else
      clause)))


(defn make-true [phi x]
  (reduce (fn[phi' clause']
            (case clause'
              nil (reduced nil) ;;Pour sortir d'un reduce
              true phi'
              ;;else
              (conj phi' clause'))) #{} (map #(make-true1 % x) phi)))



(make-true '#{#{z (not g)} #{(not z) (not x)} #{y s}} 'z)
(make-true '#{#{z (not g)} #{(not z)} #{y s}} 'z)



(defn make-false1 [clause x]
  (let [not-x (list 'not x)]
    (cond
      (contains? clause not-x) true
      (contains? clause x) (let [clause' (disj clause not-x)]
                             (if (empty? clause)
                               nil
                               clause'))
      :else
      clause)))


(defn make-false [phi x]
  (reduce (fn[phi' clause']
            (case clause'
              nil (reduced nil) ;;Pour sortir d'un reduce
              true phi'
              ;;else
              (conj phi' clause'))) #{} (map #(make-false1 % x) phi)))


(make-false '#{#{z (not g)} #{(not z) (not x)} #{y s}} 'z)
;;#{#{(not g)} #{y x}}
(make-false '#{#{z (not g)} #{(not z)} #{y s}} 'z)
;;nil



(defn find-1-literal [f]
    ;;When renvoie first % si la condition devient vrai sinon when revoit nil
    (some #(when (= (count %) 1) (first %)) f))


(find-1-literal '#{#{x (not g)} #{(not z)} #{z (not x)}})


(defn rule-1-literal [phi]
  (if-let [litt (find-1-literal phi)]
    (if-let [phi' (if (symbol? litt)
                    (make-true phi litt)
                    (make-false phi (second litt)))]
      [phi', (if (symbol? litt) litt (second litt)), (symbol? litt)]
      ;;else formule insatisfiable
      [nil, nil, nil]) ;;insatisfiable
    ;;else on n'a pas trouvé
    nil)) ;;pas de 1 literal


(defn find-np1 [m clause]
    (loop [clause clause, m m]
        (if (seq clause)
           (let [[x, xsigne] (if (symbol? (first clause))
                               [(first clause) "positive"]
                               [(second (first clause)) "negative"])
               signe (get m x)]
               (case signe
                   nil (recur (rest clause) (assoc m x xsigne))
                   "positive" (recur (rest clause) (if (= xsigne :positive)
                                                       m
                                                       (assoc m x :supp)))
                   "negative" (recur (rest clause) (if (= xsigne :negative)
                                                       m
                                                       (assoc m x :supp)))
                   :supp (recur (rest clause) m)))
               ;;fin du let
               m)))


(defn find-neg-pos [phi]
    (some (fn [[x signe]]
            (if (not= signe :supp)
                [x signe]
                false)))   (reduce find-np1 {} phi))

;;Exemple de fonction pour la commande some
(defn my-some [f s]
    (reduce (fn [res e]
        (let [eval (f e)]
            (if eval
                ;On sort du reduce
                (reduced eval)
                nil))) nil s))

(defn rule-aff-neg [phi]
    (if-let [[x signe] (find-neg-pos phi)]
        (case signe
            :positive (make-true phi x)
            :negative (make-false phi x))
            ;;Pas de pos ou de neg
        nil))

(defn trivial-splitter [phi]
    (let [litt (first (first phi))]
        (if (symbol? litt)
            litt
            (second litt))))

(defn get-var [litt]
    (if (symbol? litt)
        litt
        (second litt)))
;;prendre la variable la plus fréquente dans la formule
;;#{#{x (not y) w} #{(not x) y (not w) z} #{x y}}
; {x 3, y 3, w 2, z 1}
;max-val retourne la clé ayant la valeur maximale
(defn varfreq1 [m clause]
    (reduce (fn [m litt]
        (let [x (get-var litt)]
            (if-let [xnb (get m x)]
                (assoc m x (inc xnb))
                (assoc m x 1)))) m clause))

            ;remplace le if-let

(defn varfreqs [phi]
    (reduce varfreq1 {} phi))


(defn max-val [m]
    (loop [m m, xval nil, maxval 0]
        (if (seq m)
            (let [[y yval] (first m)]
                (if (> yval xval)
                    (recur (rest m) y yval)
                    (recur (rest m) xval maxval)))
            ;;à la fin du let
            maxval)))

(defn max-splitter [phi]
    (max-val (varfreqs phi)))


(defn dpll
  "prend une formule phi et retourne la map des variables/valeurs (instanciation) ou no si non satisfiable"
  ([phi] (dpll phi {} max-splitter))
  ([phi splitter] (dpll phi {} splitter))
  ([phi sat splitter]
   (loop [phi phi, sat sat]
     (if (empty? phi)
       sat ;;=> si le joueur a gagné
       (if-let [[phi', x, xval] (rule-1-literal phi)] ;cherche dans une formule les clauses avec un seul littéral
         (recur phi' (assoc sat x xval))
         ;Pour l'affirmative, la variable est tout le temps faux (resp. vrai)
         ;On en cherche une, puisqu'on ne peut pas tout appliquer d'un coup
         (if-let [[phi', x, xval] (rule-aff-neg phi)]
           (recur phi' (assoc sat x xval))
           (let [x (splitter phi)]
             (or (let [phi-true (make-true phi x)]
                   (and phi-true (dpll phi-true (assoc sat x true) splitter)))
                 (let [phi-false (make-false phi x)]
                   (and phi-false (dpll phi-false (assoc sat x false) splitter)))
                 nil))))))))









(def ex-grille @#'g/sudoku-grid)

(fact
  (g/cell ex-grille 1 1) => {:status :init, :value 5}
  (g/cell ex-grille 4 2) => {:status :init, :value 1}
  (g/cell ex-grille 9 9) => {:status :init, :value 9}
  (g/cell ex-grille 4 5) => {:status :init, :value 8}
  (g/cell ex-grille 4 6) => {:status :empty})

;; Permet d'encoder un nombre décimal en chiffre binaire
(declare log-binary)

(defn log-binary [n]
  (loop [n n
    res '()]
    (if (not= n 0)
      (recur (/(- n (mod n 2)) 2) (conj res (mod n 2)))
      res)))



(fact
  (log-binary 5) => '(1 0 1)
  (log-binary 9) => '(1 0 0 1)
  (log-binary 42) => '(1 0 1 0 1 0))

;; Permet de prendre uniquement les n premiers chiffres de la liste passée, si il y a moins de chiffres, on les remplace par x
(declare pad-seq)

(defn pad-seq [liste x n]
  (let [liste (reverse liste)
    res (take n liste)
    taille (count res)]
    (loop [res (reverse res)
      taille taille]
      (if (not= taille n)
        (recur (conj res x) (inc taille))
        res))))


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

(declare encode-num)


(defn encode-aux [x y valeur bit]
  (if (= valeur 0)
    (list 'not (mkcellbit x y bit))
    (mkcellbit x y bit)))

(defn encode-num
  ([x y n]
    (let [bin (reverse (encode-value n))]
      (list 'and (encode-aux x y (first bin) 0) (encode-num x y (rest bin) 1))))
  ([x y bin nbit]
    (if (= nbit 3)
      (list 'and (encode-aux x y (first bin) nbit) true)
      (list 'and (encode-aux x y (first bin) nbit) (encode-num x y (rest bin) (inc nbit))))))




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

;; Renvoie toutes les valeurs possibles pour une cellule vide
(declare encode-vide)

(defn encode-vide
  ([x y]
   (list 'or (encode-num x y 9) (encode-vide x y 8)))
  ([x y n]
   (if (= n 1)
     (list 'or (encode-num x y n) false)
     (list 'or (encode-num x y n) (encode-vide x y (dec n))))))



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


(declare distinct-empty-empty)

(defn mkdistinct [c l bit]
  (symbol (str "l" l "c" c "b" bit)))

(defn empty-empty [c1 l1 c2 l2 bit]
  (list '<=> (mkdistinct c1 l1 bit) (list 'not (mkdistinct c2 l2 bit))))


(defn distinct-empty-empty
  ([c1 l1 c2 l2]
    (list 'and (empty-empty c1 l1 c2 l2 0) (distinct-empty-empty c1 l1 c2 l2 1)))
  ([c1 l1 c2 l2 bit]
   (if (= bit 3)
     (list 'and (empty-empty c1 l1 c2 l2 bit) true)
     (list 'and (empty-empty c1 l1 c2 l2 bit) (distinct-empty-empty c1 l1 c2 l2 (inc bit))))))


(fact
 ;; les cellules entre cx1=2,cy1=3 et cx2=2,cy=5 doivent être distinctes
 (distinct-empty-empty 2 3 2 5)
 => '(and (<=> l3c2b0 (not l5c2b0)) ; bits b0 distincts
          (and (<=> l3c2b1 (not l5c2b1)) ; b1
               (and (<=> l3c2b2 (not l5c2b2)) ; b2
                    (and (<=> l3c2b3 (not l5c2b3))  ; b3
                         true)))))



(declare distinct-filled-empty)

(defn distinct-filled-empty
  ([val x y]
    (let [bin (encode-value val)]
      (list 'or (encode-aux x y (first bin) 0) (distinct-filled-empty x y (rest bin) 1))))
  ([x y bin nbit]
    (if (= nbit 3)
      (list 'or (encode-aux x y (first bin) nbit) false)
      (list 'or (encode-aux x y (first bin) nbit) (distinct-filled-empty x y (rest bin) (inc nbit))))))



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


(declare distinct-cells)

(defn distinct-cells
  ([liste]
   (list 'and true (distinct-cells (first liste) (rest liste) (rest liste))))
  ([pre verif backup]
    (if (seq backup)
      (if (seq verif)
        (let [[x1 y1 status1] pre
          [x2 y2 status2] (first verif)]
          (list 'and (distinct-pair x1 y1 status1 x2 y2 status2) (distinct-cells pre (rest verif) backup)) true)
        (recur (first backup) (rest backup) (rest backup))))))



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
                          (list 'and (encode-blocks grille))))))),

;; (encode-sudoku ex-grille)
;; => .... attention : formule énorme ! ....

(defn resolution [grille]
  (dpll (d/dcnf grille)))


;; EXERCICE : retirer les clauses qui contiennent un littéral et sa négation
;; (a ou non a) => vrai
;; (a et non a) => faux
;; fonction :  filter-trivial FAIT

;; EXERCICE : si on a une clause C1 incluse dans une clause C2
;; (par exemple: #{a (not b)}   et  #{a (not b) c})
;; alors on retire la plus grande C2 ..
;; fonction :  filter-subsume

;; EXERCICE : en déduire une fonction dnfs qui prend une
;; formule quelconque et retourne la formule DNF simplifiée représentée par des ensembles

;; EXERCICE :  comment passer d'une DNF sous forme d'ensemble d'ensembles à une CNF ?
;;             (indice : la CNF d'une formule f  est liée à la DNF de (not f) )

;; En déduire une fonction :  cnfs prend une
;; formule quelconque et retourne la formule CNF simplifiée représentée
