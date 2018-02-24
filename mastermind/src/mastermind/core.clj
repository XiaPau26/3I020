(ns mastermind.core
  (:gen-class))

(declare vs-device)
(declare code-secret)
(declare comparer)
(declare indications)
(declare lireLine)
(declare filtre-indications)
(declare freqs-dispo)
(declare frequences)
(declare compter)
(declare filtrer)
(declare convertirKey)
(declare verifColor)
(declare restart)
(declare valeur-tenta)
(declare lancer-jeu-choix)
(declare vs-player)
(declare lireCode)
(declare rightColor)
(declare deviner)
(declare buildCode)
(declare convertirString)
(declare bad)
(declare good)
(declare comparerIndic)
(declare tabColor)
(declare removeColor)
(declare choixColor)

(def pionsC [:red :blue :green :yellow :black :white])




(defn -main [& args]
  (println "************************************* Welcome to the MatserMind game ! Do you want to start a new game ? *************************************")
  ;;(println (first (clojure.string/split (read-line) #" "))))
  (println " 1 : yes,why not?")
  (println " 2 : WTH! not at all")
  ;;(println (bad [:red :white :red :black] [:bad :bad :bad :bad] [:red :blue :green :yellow :black :white])))
  (let [response (read-line)] ;;read-line récupère un string sur la ligne de commande
    (if (= (compare response "1") 0)
      ;;On lance le jeu si la reponse est 1
      (lancer-jeu-choix)
      (println "Too bad ! see you later ..."))))



;;Cette fonction permet de lancer le jeu en fonction des choix du joueur, c'est-à-dire soit joueurVSordinareur soit ordinateurVSjoueur
(defn lancer-jeu-choix []
	(println " Perfect !")
	(println " Shall I defeat you ?")
	(println " 1 : Yes, try it if you dare")
	(println " 2 : No, I want to defeat you !")
	(println " 3 : After a thought, I don't want to play")
	(let [choix (read-line)]
		(cond 
			(= (compare choix "1") 0) (vs-player)
			(= (compare choix "2") 0) (vs-device)
			:else 
			(println " Soo sad, see you late !"))))

;;****************************************************************************************



;;Cette fonction permet de générer une partie contre le joueur
;;Le joueur entre au début son code secret (pour éviter les triches)
;;Elle va récupérer un code générer aléatoirement puis faire les comparaisons afin de décider si l'ordinateur a pu déviner le code secret ou non
(defn vs-player []
	(println " Nice, let's begin the game !")
	(println " So, let's start by entering you code, please choose 4 colors from red, blue, green, yellow, black and white")
	(let [codeSecret (convertirKey (lireCode))] ;;Conversion en clé du code secret lu et qui est correct (vérifier par le code rightColor)

		;;On initialise le nombre d'essais donné par le joueur 
		(println " How many tries do you give me ?")
		(println " 1 : 12")
		(println " 2 : 10")
		(println " 3 : 8")
		(println " 4 : 6 (Hmm, easy for me)")
		;;On créait une boucle sur le nombre de tentatives 
	   	(loop [tentative (valeur-tenta (read-line))
	   		tp pionsC
	   		indic []
	   		code []]
			(if (> tentative 0)
				(do
					(println " I have" tentative "tries left")
					;;On construit le code avec tp qui contient les couleurs que le device peut choisir
					(let [codeDevice (buildCode (count codeSecret) code indic tp)]
						(println " Is this the good answer ? If it is at the right place tap good, 
							if there is a token which color is in your code but not at the right place in mine please tap color, 
							else tap bad")
						(println " Yours : " codeSecret)
						(println " Mine : " codeDevice)
						(let [indiquer (comparerIndic (convertirKey (clojure.string/split (read-line) #" ")) (filtre-indications codeSecret codeDevice (indications codeSecret codeDevice)))] 
							;;compIndic vérifie si l'indication donnée par le joueur correspond à la vraie indication ou non
							;;Puis on appelle la fonction good qui va permettre de savoir si l'ordinateur à gagner la partie ou non 
							(if (good indiquer)
								;;Le device a gagné
								(println " Yes ! I did it, it was fun, thanks a lot !")
								;;Sinon il refait son code à partir des choix qu'il peut prendre 
								(do (println " Oh too bad, let's retry !")
									(recur (dec tentative) (bad codeDevice indiquer tp) indiquer codeDevice))))))
				(println " Oh, I don't have any tries *sad*")))))
			


;;Cette fonction permet de retirer les couleurs qui ne peuvent pas entrer dans le code Secret 
;;Ce sont les couleurs qui ont été marquées bad dans l'indication donnée par le joueur (et vérifier par les fonctions ci-dessous)
(defn bad [cS indic tp]
	(loop [cpt 0
		tpR tp]
		(if (< cpt (count cS))
			(if (= (compare (get indic cpt) :bad) 0)
				;;Si ça correspond a un bad, on peut le retirer de la liste
				;;Laisse passer que les mots clés qui vérifie le filtre
				(recur (inc cpt) (filter #(not= (get cS cpt) %) tpR))
				;;Sinon je ne fais rien d'autre
				(recur (inc cpt) tpR))
			tpR)))




;;Cette fonction permet de comparer l'indication donné par le joueur et celui trouvé par l'ordinateur
;;Pour éviter les triches 
;;v1 correspond aux indications tapées par le joueur
(defn comparerIndic [v1 v2]
	;;On compare si les deux vecteurs ont la même taille, si ils n'ont pas la même taille, cela signifie que le joueur à mal entré ses indications
	(if (= (count v1) (count v2))
		;;sinon on compare si c'est les même
		(loop [cpt 0
			v v1]
			;;Si on est pas arrivé au bout du vecteur 
			(if (< cpt (count v1))
				;;si c'est la même indication donnée par le joueur et l'ordinateur, on continue la vérification
				(if (= (compare (get v cpt) (get v2 cpt)) 0)
					;;si ce sont les mêmes valeurs, on passe à la valeur suivante
					(recur (inc cpt) v)
					;;Sinon on demande de retaper le code d'indication
					(do (println " Be careful, you did a mistake, please enter again")
						(recur 0 (convertirKey (clojure.string/split (read-line) #" ")))))
				v))
		;;Si ce n'est pas la même taille, on demande à l'utilisateur de retaper le code 
		(do (println " Warning, there are lesser indications than the Secret code")
			(comparerIndic (convertirKey (clojure.string/split (read-line) #" ")) v2))))



;;Cette fonction permet d'indiquer si le vecteur d'indication contient que des good ou non 
(defn good [v]
	(loop [v v]
		(if (seq v)
			(if (= (compare (first v) :good) 0)
				(recur (rest v))
				false)
			true)))






;;Cette fonction permet de lire en input le code tapé par l'utilisateur, on vérifie si le code est correct ou non
(defn lireCode []
		(let [codeSecret (clojure.string/split (read-line) #" ")] 
			(if (not= (count codeSecret) 4)
				(do (println " AARGHH, Please enter the right number of arguments, there are 4 tokens in a Secret Code") (lireCode))
				(if (rightColor codeSecret)
					codeSecret
					(lireCode)))))

;;Cette fonction permet de vérifier si les couleurs contenues dans le codeSecret appartiennent au vecteur de couleurs pionsC
(defn rightColor [v]
	(loop [i 0]
		(if (< i (count v))
			(if (some #(= (compare (keyword (get v i)) %) 0) pionsC)
				(recur (inc i))
				(do (println " You did a mistake, please be careful with the color name") false))
			true)))


;;Cette fonction permet à l'ordinateur de construire un code 
;;tabc est le tableau contenant les valeurs possible que le device peut choisir
;;indic est le tableau des indications
;;cs correspond au code trouvé par l'ordinateur précédemment
(defn buildCode [taille cs indic tab]
	(loop [i 0
		color (tabColor cs indic)
		res []]
		;;Si c'est inferieur au code donné par le player, on continue a remplir le vecteur réponse
		(if (< i taille)
			;;si cs et indic sont non vides, cela signifie qu'on n'est pas à la premiere combinaison de code qu'on tape  
			(if (and (seq cs) (seq indic))
				(if (= (compare (get indic i) :good) 0)
					(recur (inc i) color (conj res (get cs i)))
					(if (seq color)
						;;Sinon l'indication est un bad, on vérifie donc si on peut récupérer des valeurs provenant du tableau des couleurs mal placés
						(do 
							(let [c (choixColor i cs tab color)] 
								(if (= (compare (second c) 5) 0)
									;;Si ce n'est pas -1 cela signifie que la couleur retournée est une couleur choisie dans color
									(recur (inc i) color (conj res (first c)))
									(recur (inc i) (removeColor color c) (conj res (first c))))))
						(recur (inc i) color (conj res (rand-nth tab)))))
					;;Sinon c'est la premiere combinaison et on remplit le vecteur code avec les valeurs de tab
				(recur (inc i) color (conj res (rand-nth tab))))
			res)))


;;Cette fonction permet de choisir une couleur parmi c
;;Stratégie => faire une rotation 
;;Renvoie un map 
(defn choixColor [i code tabC mapC]
	(if (>= (count mapC) 2)
		;;Si j'ai plus de 2 éléments dans le map, on est sur de ne pas mettre la même couleur puisqu'on réalise une rotation des couleurs mals placées
		(loop [cpt 1
			clefV (first mapC) ;;On récupère la première valeur pour pouvoir les comparer et permettre de réaliser la rotation
			value (second (first mapC))
			mc (rest mapC)]
			(if (< cpt (count mapC))
				(let [clefV2 (first mc)
					value2 (second (first mc))]
					(if (< value value2) ;;On cherche celui qui a la position maximale
						(recur (inc cpt) clefV2 value2 (rest mc))
						(recur (inc cpt) clefV value (rest mc))))
				clefV))
		(loop [cpt 0
			mc mapC]
			(if (< cpt (count mapC))
				(if (= (compare (get code i) (first (first mc))) 0)
				;;si c'est la même couleur 
					(recur (inc cpt) mc)
					(first mc))
				(first (assoc {} (rand-nth tabC) 5))))))


;;Cette fonction construit le vecteur contenant les couleurs qui ne sont pas à la bonne place, renvoie un map avec les valeurs de leur position dans le vecteur 
(defn tabColor [code indic]
	(loop [i 0
		res {}]
		(if (< i (count code))
			(if (= (compare (get indic i) :color) 0)
				(recur (inc i) (assoc res (get code i) i))
				(recur (inc i) res))
			res)))



;;Cette fonction permet de retirer la couleur qui a été utilisé dans le code et qui ne doit pas être choisi dans la prochaine pool
(defn removeColor [m c]
	(loop [i 0
		mn m
		res {}
		c (first c)]
		(if (seq mn)
			(if (not= (compare (first (first mn)) c) 0)
				;;Si les couleurs ne correspondent pas à c
				(recur (inc i) (rest mn) (assoc res (first (first mn)) (second (first mn))) c)
				(recur (inc i) (rest mn) res c))
			res)))




;;Cette fonction permet de convertir les mots clés en String (optionnelles)
;(defn convertirString [v]
;	(loop [tmp v
;		res []]
;		(if (seq tmp)
;			(recur (rest tmp) (conj res (name (first tmp))))
;			res)))
;;****************************************************************************************
;;Fonctions permettant de faire une partie contre l'ordinateur


;;Fonction permettant de gerer le jeu : lancement et arrêt du jeu
(defn vs-device []
  (println " Perfect! Will you be the next mastermind?")
  ;;On initialise le code secret ainsi que le nombre de tentatives

  (println " How many tries do you want ?")
  (println " 1 : 12 (hmmm still a newbie ?)")
  (println " 2 : 10 (A good start hmm)")
  (println " 3 : 8 (Oh you are very confident )")
  (println " 4 : 6 (Wow , You really like challenges !)")

  ;;Lit le nombre de tentative voulu
  (let [tentative (valeur-tenta (read-line))]
    (let [codeSecret (code-secret 4)]
      (println " The Boss has chosen 4 colors from red, blue, green, yellow, black and white so let's start !")
      (loop [m 0]
      ;;m correspond au nombre de tentatives réalisées
        (println "You have" (- tentative m) "tries left")
        (if (< m tentative)
          (if (= (verifColor (lireLine) codeSecret) true) ;;Permet de vérifier si le vecteur tapé correspond au code secret
            (do (println " OMG ! Congratulations! I can't believe it ! You won ! Another game ? ")
              (println " 1 : it was fun, so yes")
              (println " 2 : I need a break, so no thanks")
              (restart))
            (do (println "Too bad ! let's retry") (recur (+ m 1)))) ;;refait une tentative si mauvais vecteur tapé
          (do (println "Too bad, you used up all your tries ! Try again ? ") (println "1 : Yes, do not want to give up !") (println "2 : No, need a break") (restart)))))))



;;****************************************************************************************


;;Fonction permettant de savoir combien de tentatives le joueur souhaite
(defn valeur-tenta [n]
  (cond
    (= (compare n "1") 0) 12
    (= (compare n "2") 0) 10
    (= (compare n "3") 0) 8
    (= (compare n "4") 0) 6
    :else (do (println "Please enter an appropriate number..") (valeur-tenta (read-line)))))


;;fonction qui permet de traiter la relance du jeu
(defn restart []
  (let [res (read-line)]
    (if (= (compare res "1") 0)
      (vs-device)
      (println "Too bad, it was fun ! See you later !"))))



;;Fonction qui permet de lire les valeurs taper par l'utilisateur sur le terminal
(defn lireLine []
  (loop [tentative 0
          res []]
          (if (< tentative 4)
          	(let [lire (read-line)]
          		;;We need to check if there is only one argument written in the input
          		(if (some #(= (compare (keyword lire) %) 0) pionsC) ;;TODO on ne peut pas compter le nombre d'argumets, il faudra donc vérifier l'argument est dans pionsC
          			;;If there is only one input, can ask the next argument
          			(recur (inc tentative) (conj res lire))
          			(do (println "Wrong arguments ! Please enter only one argument with the correct format : example red (do not add a space after)") (recur tentative res))))
          	(do (println res) (convertirKey res)))))



;;Cette fonction va permettre de transformer les elements dans le vecteur tapé par le joueur et le transforme en mot clé, puisque les éléments
;;tapés sont des strings
(defn convertirKey [v]
	(loop [cpt 0
		res []]
		(if (< cpt (count v))
			(recur (inc cpt) (conj res (keyword (get v cpt))))
			res)))


;;Cette fonction retournve vrai si le joueur a deviné la bonne combinaison, sinon faux
(defn verifColor [codePlayer codesecret]
  (if (= (count codePlayer) (count codesecret))
    (let [resp (filtre-indications codesecret codePlayer (indications codesecret codePlayer))]
      (println resp)
      (if (some #(not= (compare :good %) 0) resp)
        false
        true))))





;;****************************************************************************************


;;Cette fonction permet de construire un code secret à n tokens
(defn code-secret [n]
  (loop [m 0
         res []]
    (if (< m n)
      (recur (inc m) (conj res (rand-nth pionsC)))
      res)))


;;****************************************************************************************


;;These two methods return an indication about the current game

;;Cette fonction permet de déterminer le contenu de l'indication à rendre
(defn comparer [cs ts v]
  (if (= (compare cs ts) 0)
    :good
    (if (some #(= (compare ts %) 0) v)
      :color
      :bad)))

;;Cette fonction permet de donner des indications sur l'avancement du jeu
;;v1 est le vecteur code secret, tandis que v2 est le vecteur tapé par l'utilisateur
(defn indications [v1 v2]
  (loop [i 0
        res []]
    (if (< i (count v1)) ;;On parcourt le vecteur
      (recur (inc i) (conj res (comparer (get v1 i) (get v2 i) v1)))
      res)))


;;****************************************************************************************
;;On supprime tous les va dans le vecteur v puisqu'on a déjà calculer son occurence
(defn filtrer [va v]
  (filter #(not= va %) v))

;;Compter le nombre de va dans le vecteur v
(defn compter [va v]
  (loop [cpt 0
         tv v
         res {}]
    (if (seq tv)
      (if (= (compare va (first tv)) 0)
        (recur (inc cpt) (rest tv) (assoc res va (inc cpt)))
        (recur cpt (rest tv) res))
      res)))

;;Cette fonctions permet de calculer l'occurence d'une clé dans un vecteur v
(defn frequences [v]
  (loop [tv v
         res {}] ;;Parcourt le vector
    (if (seq tv) ;;vérification si c'est vide
      (recur (filtrer (first tv) tv) (conj res (compter (first tv) tv)))
      res)))


;;****************************************************************************************
;;Cette fonction permet de déterminer combien de pions de couleur il reste à deviner
(defn freqs-dispo [v1 v2]
  (loop [m 0
         res (frequences v1)]
    (if (< m (count v1))
      (if (= (compare (get v2 m) :good) 0)
        (recur (inc m) (assoc res (get v1 m) (dec (get res (get v1 m)))))
        (recur (inc m) res))
      res)))



;;****************************************************************************************
;;Le deuxième paramètre ne contient pas des mots clés, il faut donc les congreenir avec la fonction name pour transformer un mot clé en string
(defn filtre-indications [v1 v2 v3]
  (loop [i 0
         res []
         cpt (freqs-dispo v1 v3)]
    (if (< i (count v1))
      (if (= (compare (get v3 i) :color) 0) ;;Si c'est un color
        (if (= 0 (get cpt (get v2 i))) ;;On vérifie si la cardinalité est bonne
          (recur (inc i) (assoc res i :bad) cpt) ;;Si pas bonne cardinalité, on change à :bad
          (recur (inc i) (conj res (get v3 i)) (assoc cpt (get v2 i) (dec (get cpt (get v2 i))))))
        (recur (inc i) (conj res (get v3 i)) cpt))
      res)))

