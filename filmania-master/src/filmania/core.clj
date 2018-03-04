(ns filmania.core
  (require [clojure.data.csv :as csv]
           [clojure.java.io :as io]
           [clojure.set]))


(declare cptBase)
(declare all-genres)
(declare films-by-genre)
(declare contenir)
(declare card-genres)
(declare movie-avg-rating)
(declare moy)
(declare ratings)
(declare csv-seq)
(declare moyTot)
(declare good-or-not)
(declare goodUser)
(declare badUser)
(declare goodMovies)
(declare badMovies)
(declare search-By-genre)
(declare reponse)
(declare mostRe)
(declare lessRe)
(declare bestRate)
(declare worstRate)
(declare cptSF)

(defn -main [& args]
	(println " Welcome to the filmania project !")
	(reponse))


	;;(println (cptBase))
	;;(let [card (card-genres)]
	;;	(println "Le maximum est" (key (apply max-key val card)))
	;;	(println "Le minimum est" (key (apply min-key val card))))

	;;(println (val (filter #(= (compare 1 (first %)) 0) ratings)))
	;;(let [note (movie-avg-rating)]
	;;	(println "Les films les mieux notés sont : ")
	;;	(println (filter #(> (second %) 2.5) note))
	;;	(println "Les films les moins biens notés sont : ")
	;;	(println (filter #(< (second %) 2.5) note))))
	;;(println "La note moyenne de la base de film est" (moyTot)))
	;;(println (goodMovies)))
	;;(println (badMovies)))
	
(defn reponse []
	(println " How can I help you ? ")
	(println " 1 - How many movies are there in the database ? ")
	(println " 2 - How many movies for a specific genre ?")
	(println " 3 - The more representative genre ")
	(println " 4 - The less representative genre ")
	(println " 5 - List the bestest movie ")
	(println " 6 - List the worstest movie ")
	(println " 7 - List the bestest and worstest movie by a genre ")
	(println " 8 - List of users who are nice")
	(println " 9 - List of users who are critics")
	(println " 10 - Exit ?")
	(let [rep (read-line)
		genres (all-genres)
		rate (movie-avg-rating)]
		(cond
			(= (compare rep "1") 0) (println (cptBase)) ;;retourne le nombre de film dans la base de données
			(= (compare rep "2") 0) (do (println " Please enter a genre, here is the list of genre : " genres) (search-By-genre (read-line) genres 1))
			(= (compare rep "3") 0) (mostRe)
			(= (compare rep "4") 0) (lessRe)
			(= (compare rep "5") 0) (bestRate rate)
			(= (compare rep "6") 0) (worstRate rate)
			(= (compare rep "7") 0) (do (println " Please enter a genre, here is the list of genre : " genres) (search-By-genre (read-line) genres 2))
			(= (compare rep "8") 0) (println (goodUser))
			(= (compare rep "9") 0) (println (badUser))
			(= (compare rep "10") 0) (println "Ok, bye, see you soon ")
			:else
			(do (println " Ooops, you enter a wrong number, I did not recongnize it, please enter again") 
				(reponse)))))



(defn search-By-genre [line genres choix]
	(if (contains? genres line)
		(cond 
			(= choix 1) (let [filmG (films-by-genre line)] 
				(println "There are" filmG line "movies that to say" (count filmG) "overall"))
			(= choix 2) (do (println "The bestest" line "movie is " (goodMovies line))
				(println "The worstest" line "movie is" (badMovies line))))
		(do (println "I did not rocognize the genre, please enter again ") (search-By-genre (read-line) genres choix))))
		


;;************************************************************************************************

(defn csv-seq
  "Retourne une séquence à partir d'un fichier CSV."
  [filename]
  (with-open [in-file (io/reader filename)]
    (doall
     (csv/read-csv in-file))))



(defn parse-movie
  "Construit un enregistrement de film depuis un entrée lue depuis CSV."
  [title-year genres]
  (let [r (re-seq #"(.+) \((\d\d\d\d)\)$" title-year)
        title (get (first r) 1)]
    (try
      (let [year (Integer/parseInt (get (first r) 2))]
        {:title title
         :year year
         :genres (set (filter #(not= % "(no genres listed)") (clojure.string/split genres #"\|")))})
      (catch Exception _ nil))))

(defn movie-map
  "Construit une map de films à partir d'une base en CSV."
  [csv]
  (reduce (fn [m [movie-id title-year genres]]
            (if-let [movie (parse-movie title-year genres)]
              (assoc m (Integer/parseInt movie-id) movie)
              m))
          {} csv))

;; Attention: gros fichier
(def movie-filename "resources/ml-latest-small/movies.csv")

(def movies (movie-map (rest (csv-seq movie-filename))))

(defn pt "Prend les 10 premiers éléments de movies"
	[]
	(take 10 movies))

(defn cptBase "Retourne le nombre de films de base"
	[]
	(print "Il y a " (count movies) "Films de base"))



;;************************************************************************************************
(defn compteurSF "vérifie si l'ensemble ens passé en paramètre contient le genre car"
	[ens car]
	(loop [e ens 
		cpt 0]
		(if (seq e)
			(if (not= (compare (first e) car) 0)
				(recur (rest e) cpt)
				1)
			cpt)))


(defn cptSF "compte le nombre de films ayant comme genre Sci-Fi"
	[]
	(loop [m movies
		cpt 0]
		(if (seq m)
			(let [genre (get (second (first m)) :genres)]
				(recur (rest m) (+ cpt (compteurSF genre "Sci-Fi"))))
			cpt)))




;;************************************************************************************************

(defn all-genres "retourne l'ensemble des genres de film de la base"
	[]
	(loop [m movies
		res #{}]
		(if (seq m)
			(let [genre (get (second (first m)) :genres)]
				(recur (rest m) (clojure.set/union res genre)))
				res)))


(defn contenir [genre ens]
	(loop [e ens]
		(if (seq e)
			(if (= (compare (first e) genre) 0)
				true
				(recur (rest e)))
			false)))


(defn films-by-genre "permet d'obtenir la base composée uniquement des films dont le genre est spécifié en paramètre"
	[genre]
	(loop [m movies
		res #{}]
		(if (seq m)
			(let [lgenre (get (second (first m)) :genres)]
				(recur (rest m) (if (contenir genre lgenre)
					(conj res (first m))
					res)))
			res)))




;;************************************************************************************************


(defn card-genres "construit une table de cardinalité par genre où chaque entrée est de la forme genre card"
	[]
	(loop [tab (all-genres)
		res {}]
		(if (seq tab)
			(recur (rest tab) (assoc res (first tab) (count (films-by-genre (first tab)))))
			res)))


(defn mostRe []
	(println "The genre the most representative is" (key (apply max-key val (card-genres)))))

(defn lessRe []
	(println "The less representative genre is" (key (apply min-key val (card-genres)))))





;;************************************************************************************************
;;PART 2


(defn parse-rating
  "Construit la map contenant les rates pour un utilisateur depuis une entrée lue depuis CSV."
  [userId movieId rate res]
  	(if (contains? res (Integer/parseInt userId))
  		(let [valU (get res (Integer/parseInt userId))]
  			(assoc valU (Integer/parseInt movieId) (Double/valueOf rate)))
    	{(Integer/parseInt movieId) (Double/valueOf rate)}))

(defn rating-map
  "Construit une map de rating à partir d'une base en CSV."
  [csv]
  (reduce (fn [r [userId movieId rating times]] ;;Prend le map et décompose csv en 3 parties
            (if-let [rate (parse-rating userId movieId rating r)] ;;Création de la map {:1 5.0}
              (assoc r (Integer/parseInt userId) rate) ;;Construction de la map de retour {:1 {:1 5.0}}
              r))
          {} csv))


(def ratings (rating-map (rest (csv-seq "resources/ml-latest-small/ratings.csv"))))

(defn tr "prend les 10 premiers éléments de ratings"
	[]
	(take 10 ratings))


;;************************************************************************************************
(defn moy "Calcul la moyenne d'un film id à partir de la map rate"
	[id]
	(loop [r ratings
		somme 0
		cpt 0]
		(if (seq r)
			(if (contains? (second (first r)) id)
				(recur (rest r) (+ somme (get (second (first r)) id)) (inc cpt)) 
				(recur (rest r) somme cpt))
			(if (= cpt 0)
				{id 0}
				{id (/ somme cpt)}))))


(defn movie-avg-rating "retourne une map associant à chaque film de la base movies sa note moyenne dans la base ratings"
	[]
	(reduce (fn [r mvs]
		(if-let [moyenne (moy (first mvs))] ;;(first mvs) permet de récupérer l'id du film et on va calculer la moyenne grace à la fonction moy
				(assoc r (first (first moyenne)) (second (first moyenne)))
				r))
	{} movies))


;;************************************************************************************************


;;Question 2 : Quelle est la note moyenne de la base de films 
(defn moyTot []
	(loop [rate (movie-avg-rating)
		somme 0
		cpt 0]
		(if (seq rate)
			(recur (rest rate) (+ somme (second (first rate))) (inc cpt))
			(/ somme cpt))))



(defn bestRate [note]
	(println "Les films les mieux notés sont : ")
	(println (filter #(> (second %) 2.5) note)))

(defn worstRate [note]
	(println "Les films les moins biens notés sont : ")
	(println (filter #(< (second %) 2.5) note)))	


;;Question 3 
(defn good-or-not "Retourne vrai si la note moyenne des notes de l'utilisateur sont supérieures à 2.5, faux sinon"
	[notes]
	(loop [rate notes
		somme 0
		cpt 0]
		(if (seq rate)
			(recur (rest rate) (+ somme (second (first rate))) (inc cpt))
			(if (>= (/ somme cpt) 2.5)
				true
				false))))

;;Quels sont les utilisateurs les plus "sympatiques" 
(defn goodUser "renvoie la liste d'id contenant les ids des personnes ayant une note moyenne supérieure à 2.5"
	[]
	(let [rate ratings]
		(reduce (fn [r x]
			(if-let [good (good-or-not (second x))]
				(conj r (first x))
				r))
		[] rate)))



;;Quels sont les utilisateurs les plus "critiques"
(defn badUser "renvoie une liste contenant les ids des personnes ayant une note moyenne inférieure à 2.5"
	[]
	(let [rate ratings]
		(reduce (fn [r x]
			(if-let [good (good-or-not (second x))]
				r
				(conj r (first x))))
		[] rate)))

;;Quel est le film de science-fiction le mieux noté...le moins bien noté

(defn goodMoviesSF "renvoie l'id du film le mieux noté parmi tous les films de science-fiction"
	[]
	(let [rate (movie-avg-rating)]
		(loop [films (films-by-genre "Sci-Fi")
			res {}]
			(if (seq films)
				(recur (rest films) (conj res (filter #(= (compare (first (first films)) (first %)) 0) rate)))
				(key (apply max-key val res))))))




(defn badMoviesSF "renvoie l'id du film le moins bien noté parmi tous les films de science-fiction"
	[]
	(let [rate (movie-avg-rating)]
	(loop [films (films-by-genre "Sci-Fi")
		res {}]
		(if (seq films)
			(recur (rest films) (conj res (filter #(= (compare (first (first films)) (first %)) 0) rate)))
			(key (apply min-key val res))))))


(defn goodMovies "renvoie l'id du film le mieux noté parmi tous les films de genre"
	[genre]
	(let [rate (movie-avg-rating)]
		(loop [films (films-by-genre genre)
			res {}]
			(if (seq films)
				(recur (rest films) (conj res (filter #(= (compare (first (first films)) (first %)) 0) rate)))
				(key (apply max-key val res))))))


(defn badMovies "renvoie l'id du film le moins bien noté parmi tous les films de genre"
	[genre]
	(let [rate (movie-avg-rating)]
	(loop [films (films-by-genre genre)
		res {}]
		(if (seq films)
			(recur (rest films) (conj res (filter #(= (compare (first (first films)) (first %)) 0) rate)))
			(key (apply min-key val res))))))

