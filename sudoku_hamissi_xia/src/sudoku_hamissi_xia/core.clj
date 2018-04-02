(ns sudoku-hamissi-xia.core
  (:gen-class)
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.set :as set]))

(declare xor)
(declare filter-trivial)
(not true)
(not false)
(and true false)
(and true true)
(or true false)
(or false false)

;;(defn -main [& args]
  ;;(print (filter-trivial '(and a (not a))))
 ;;)

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

    ;; *** Simplification de l'equivalence
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

( nenf '(or (not (or (not true)
                              (and (or (not x) false)
                                   (or (not false) x))))
                     (not (or y (and false z)))))

;;; ## Forme normale disjonctive DNF

(defn distrib [f]
  (match f
    (['and (['or a b] :seq) c] :seq)
    (list 'or (distrib (list 'and a c))
              (distrib (list 'and b c)))
    (['and a (['or b c] :seq)] :seq)
    (list 'or (distrib (list 'and a b))
              (distrib (list 'and a c)))
    :else f))

;; Remarque : f doit être en nenf
(defn dnf' [f]
  (match f
    (['and a b] :seq) (distrib (list 'and (dnf' a)
                                          (dnf' b)))
    (['or a b] :seq) (list 'or (dnf' a)
                               (dnf' b))
    :else f))

(defn dnf [f]
  (dnf' (nenf f)))

(dnf '(and (or a (and b c)) (or (not a) (not c))))
(dnf '(and a (and a (and (not b ) (not b)))))
;;; Problème : c'est pas lisible et c'est simplifiable
;;; Solution : représentation sous forme d'ensemble (conjonctif) de clauses (disjonctives)

(defn setify-and [f] ;;On transforme chaque caluse de la forme and dans un enesemble
  (match f
    (['and a b] :seq)
    (set/union (setify-and a) (setify-and b))
    :else #{f}))

(setify-and '(and a (and a (and (not b ) (not b)))))

(defn setify-dnf [f];;On affiche la forme dnf ou chaque clause est affichée sous forme d'un ensemble
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
(defn filter-trivial [f]
  (match f
    (setify-dnf f
    (['and a (['not b] :seq)] :seq) (if (= (compare a b) 0)
                                      false
                                      f)
    (['or a (['not b] :seq)] :seq) (if (= (compare a b) 0)
                                     true
                                     f)
    :else f))

(filter-trivial '(and a (not a)))
(filter-trivial '(or a (not a)))
(filter-trivial '(or a (not b)))
(filter-trivial '(and a (not b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EXO 2
(defn filter-subsume[f]
 (filter (f[x]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EXO 3
;;CNF qui correpond pas à l'instruction de 'exo :

(defn distrib [f]
  (match f
    (['or (['and a b] :seq) c] :seq)
    (list 'and (distrib (list 'or a c))(distrib (list 'or b c)))

    (['or a (['and b c] :seq)] :seq)
    (list 'and (distrib (list 'or a b))(distrib (list 'or a c)))

    (['==> a (b :seq)] :seq)
    (list 'or (distrib 'not a)(distrib b))

    (['<=> a b] :seq)
    (list 'and (distrib '==> a b)(distrib '==> b a))

    :else f))

;; Remarque : f doit être en nenf
(defn cnf' [f]
  (match f
    (['or a b] :seq) (distrib (list 'or (cnf' a)(cnf' b)))
    (['and a b] :seq) (list 'and (cnf' a)(cnf' b))
    (['<=> a b] :seq) (distrib (list '<=> (cnf' a)(cnf' b)))
    (['==> a b] :seq)(distrib(list  '==> (cnf' b)(cnf' a)))
    :else f))

(defn cnf [f]
  (cnf' (nenf f)))

(cnf '(and (or a (and b c)) (or (not a) (not c))))
(cnf '(and a (or a (and (not b ) (not b)))))
(cnf '(<=> y (or a b)))

(cnf '(==> y (or a b)))


(defn setify-or [f] ;;On transforme chaque caluse de la forme and dans un enesemble
  (match f
    (['or a b] :seq)
    (set/union (setify-or a) (setify-or b))
    :else #{f}))

(setify-or '(or a (or a (or (not b ) (not b)))))

(defn setify-cnf [f];;On affiche la forme dnf ou chaque clause est affichée sous forme d'un ensemble
  (match f
   (['or a b] :seq) #{(setify-or f)}
   (['and a b] :seq) (set/union (setify-cnf a) (setify-cnf b))
   ([==> a b] :seq) (set/union (setify-cnf a) (setify-cnf b))
   ([<=> a b] :seq) (set/union (setify-cnf a) (setify-cnf b))
   :else #{#{f}}))


(setify-cnf '(<=> y (or a b)))


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
;; formule quelconque et retourne la formule CNF simplifiée représentée par des ensembles
;; (en passant par la représentation DNF)


