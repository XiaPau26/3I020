;;; # SAT Partie 2 : manipulations de formules propositionnelles

;;; Ne pas oublier la dépendance suivante dans project.clj
;;; [org.clojure/core.match "0.3.0-alpha5"]

(ns mrsudoku.dpll
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.set :as set])
  (:require [mrsudoku.sat.encode :as encode])
  (:require [mrsudoku.grid :as g]))


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
         (['==> a false] :seq) (list 'not a)
         (['==> false a] :seq) true

         ;; *** Simplification de l'equivalence ***
         (['<=> a true] :seq) a
         (['<=> true a] :seq) a
         (['<=> a false] :seq) (list 'not a)
         (['<=> false a] :seq) (list 'not a)



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


(defn nnf' [f]
  (match f
    ;; not .. and
    (['not (['and a b] :seq)] :seq)
    (list 'or (nnf' (list 'not  a)) (nnf' (list 'not b)))
    ;; not .. or
    (['not (['or a b] :seq)] :seq)
    (list 'and (nnf' (list 'not  a)) (nnf' (list 'not b)))

    ;;not .. <=>
    (['not (['<=> a b] :seq)] :seq)
    (nnf' (list '<=> (list 'not a) b))

    ;;not.. ==>
    (['not (['=> a b] :seq)] :seq)
    (nnf' (list 'and a (list 'not b)))

    ;: not .. not
    (['not (['not a] :seq)] :seq) (nnf' a)
    ;; and ..
    (['and a b] :seq) (list 'and (nnf' a) (nnf' b))
    ;; or ..
    (['or a b] :seq) (list 'or (nnf' a) (nnf' b))
    ;; TODO ==> et <=>
    (['==> a b] :seq) (list 'or (nnf' (list 'not a)) (nnf' b))
    (['<=> a b] :seq) (list 'and (nnf' (list '==> a b)) (nnf' (list '==> b a)))
    :else f))

(defn nnf [f]
  (nnf' (simplify f)))

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
;(defn filter-subsume[f]
 ;; (filter (f[x])))

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


(defn cnf2 [f]
  (match f
    (['or a b] :seq) (distrib (list 'or (cnf' a)
                                          (cnf' b)))
    (['and a b] :seq) (list 'and (cnf' a)
                               (cnf' b))
    :else f))

(defn cnf3 [f]
  (cnf2 (nnf f)))

(cnf3 '(<=> a (and b c)))

(defn dcnf-aux [f equivs]
  (match f
         ([op a b] :seq)
         (let [[a', equivs1] (dcnf-aux a equivs)
               [b', equivs2] (dcnf-aux b equivs1)
               f' (list op a' b')]
           (if-let [eq (get equivs2 f')]
             [eq equivs2]
             ;;si on a pas trouvé la formule
             (let [v (symbol (str "$" (inc (count equivs2))))]
               [v (assoc equivs2 f' v)])))
         :else [f equivs]))




(defn dcnf [f]
  (let [[var phi] (dcnf-aux f {})]
    (set (mapcat
          (fn [[a b]]
            (let [b (if (= var b) true b)]
              (setify-cnf (cnf3 (list '<=> a b))))
              )
          phi))))

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

(defn make-clause [clause x not-x]
  (cond
   (contains? clause x) true
   (contains? clause not-x) (disj clause not-x)
   :else clause))

(defn make-true-false [phi x not-x]
  (reduce
   (fn [phi' clause']
     (case clause'
       #{} (reduced nil)
       true phi'
       ;;else
       (conj phi' clause')
       ))
   #{}
   (map #(make-clause % x not-x) phi)))

(defn make-true [phi x]
  (make-true-false phi x (list 'not x)))

(defn make-false [phi x]
  (make-true-false phi (list 'not x) x))


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
                               [(first clause) :positive]
                               [(second (first clause)) :negative])
               signe (get m x)]
               (case signe
                   nil (recur (rest clause) (assoc m x xsigne))
                   :positive (recur (rest clause) (if (= xsigne :positive)
                                                       m
                                                       (assoc m x :supp)))
                   :negative (recur (rest clause) (if (= xsigne :negative)
                                                       m
                                                       (assoc m x :supp)))
                   :supp (recur (rest clause) m)))
               ;;fin du let
               m)))



;(defn find-neg-pos [phi]
 ;   (some (fn [[x signe]]
  ;          (if (not= signe :supp)
   ;             [x signe]
    ;            false))   (reduce find-np1 {} phi)))

(defn find-neg-pos [phi]
 (loop [phi phi nm {}]
    (if-let [clause (first phi)]
      (let [nm (reduce
                #(let [[var kw] (cond
                                 (symbol? %2) [%2 :positive]
                                 :else [(second %2) :negative])]
                   (if (contains? %1 var)
                     (cond (= kw (get %1 var)) %1
                           :else (assoc %1 var :supp))
                     (assoc %1 var kw)))
                nm
                clause)]
        (recur (rest phi) nm))
      (some #(when (not (= :supp (second %))) %) nm))))


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
            :positive [(make-true phi x) x true]
            :negative [(make-false phi x)x false])
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


;(defn max-val [m]
 ;   (loop [m m, xval nil, maxval 0]
  ;      (if (seq m)
   ;         (let [[y yval] (first m)]
    ;            (if (> yval xval)
     ;               (recur (rest m) y yval)
      ;              (recur (rest m) xval maxval)))
            ;;à la fin du let
       ;     maxval)))

;(defn max-splitter [phi]
 ;   (max-val (varfreqs phi)))

(defn max-val [m]
  (loop [m m, xmax nil, max 0]
    (if (seq m)
      (let [[y yval] (first m)]
        (if (> yval max)
          (recur (rest m) y yval)
          (recur (rest m) xmax max)))
      ;;à la fin du let
      xmax)))

(defn max-splitter [phi]
  (max-val (varfreqs phi)))



;(defn dpll
 ; "prend une formule phi et retourne la map des variables/valeurs (instanciation) ou no si non satisfiable"
  ;([phi] (dpll phi {} max-splitter))
  ;([phi splitter] (dpll phi {} splitter))
  ;([phi sat splitter]
   ;(loop [phi phi, sat sat]
    ; (if (empty? phi)
     ;  sat ;;=> si le joueur a gagné
      ; (if-let [[phi', x, xval] (rule-1-literal phi)] ;cherche dans une formule les clauses avec un seul littéral
       ;  (recur phi' (assoc sat x xval))
        ; ;Pour l'affirmative, la variable est tout le temps faux (resp. vrai)
         ;On en cherche une, puisqu'on ne peut pas tout appliquer d'un coup
 ;        (if-let [[phi', x, xval] (rule-aff-neg phi)]
  ;         (recur phi' (assoc sat x xval))
   ;        (let [x (splitter phi)]
    ;         (or (let [phi-true (make-true phi x)]
     ;              (and phi-true (dpll phi-true (assoc sat x true) splitter)))
      ;           (let [phi-false (make-false phi x)]
       ;            (and phi-false (dpll phi-false (assoc sat x false) splitter)))
        ;         nil))))))))


(defn dpll
  "prend une formule ph: et retourne la map des variables/valeurs (instanciation) ou no si non satisfiable"
  ([phi] (dpll phi {} max-splitter))
  ([phi splitter] (dpll phi {} splitter))
  ([phi sat splitter]
   (loop [phi phi, sat sat]
     ;; le joueur a gagné
     (if (empty? phi)
       sat
       (if-let [[phi', x, xval] (rule-1-literal phi)]
         ;;=> cherche dans une formule les clauses avec un seul littéral
         (if (nil? phi')
           nil
           (recur phi' (assoc sat x xval)))
         ;;Pour l'affirmative, la variable est tout le temps faux (resp. vrai)
         ;;On en cherche une, puisqu'on ne peut pas tout appliquer d'un coup
         (if-let [[phi', x, xval] (rule-aff-neg phi)]
           (recur phi' (assoc sat x xval))
           (let [x (splitter phi)]
             (or (let [phi-true (make-true phi x)]
                   (and phi-true (dpll phi-true (assoc sat x true) splitter)))
                 (let [phi-false (make-false phi x)]
                   (and phi-false (dpll phi-false (assoc sat x false) splitter)))
                 nil))))))))






;(count (flatten (encode/encode-sudoku encode/ex-grille)))

(def s (dcnf (encode/encode-sudoku encode/ex-grille)))


;(time(dpll s))

(defn s2 []
  (println "laaaaaa")
  (println (count s))
  (encode/decode-grid (dpll s)))

;(do (println "voici le résultat obtenu ")
 ; (print (g/grid->str s2)))
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
