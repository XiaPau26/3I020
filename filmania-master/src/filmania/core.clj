(ns filmania.core
  (require [clojure.data.csv :as csv]
           [clojure.java.io :as io]
           [clojure.set]))


(declare cptBase)
(declare cptSF)
(declare compteurSF)
(declare all-genres)
(declare films-by-genre)
(declare contenir)
(declare card-genres)
(declare tr)
(declare pt)
(declare movie-avg-rating)
(declare moy)
(declare ratings)
(declare csv-seq)

(defn -main [& args]
	;;(println (card-genres))
	(println (cptBase))
	;;(println "Le maximum est" (key (apply max-key val (card-genres))))
	;;(println "Le minimum est" (key (apply min-key val (card-genres))))
	;;(println (pt))
	;;(println (tr))
	;;(println (movie-avg-rating))
	;;(println (type (first (movie-avg-rating)))))
	(println ratings)
	(println (count (rest (csv-seq "resources/ml-latest-small/movies.csv")))))



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

(defn cptBase []
	(print "Il y a " (count movies) "Films de base"))


(defn cptSF []
	(loop [m movies
		cpt 0]
		(if (seq m)
			(let [genre (get (second (first m)) :genres)]
				(recur (rest m) (+ cpt (compteurSF genre "Sci-Fi"))))
			cpt)))


(defn compteurSF [ens car]
	(loop [e ens 
		cpt 0]
		(if (seq e)
			(if (not= (compare (first e) car) 0)
				(recur (rest e) cpt)
				1)
			cpt)))


(defn all-genres []
	(loop [m movies
		res #{}]
		(if (seq m)
			(let [genre (get (second (first m)) :genres)]
				(recur (rest m) (clojure.set/union res genre)))
				res)))


(defn films-by-genre [genre]
	(loop [m movies
		res #{}]
		(if (seq m)
			(let [lgenre (get (second (first m)) :genres)]
				(recur (rest m) (if (contenir genre lgenre)
					(conj res (first m))
					res)))
			res)))

(defn contenir [genre ens]
	(loop [e ens]
		(if (seq e)
			(if (= (compare (first e) genre) 0)
				true
				(recur (rest e)))
			false)))


(defn card-genres []
	(loop [tab (all-genres)
		res {}]
		(if (seq tab)
			(recur (rest tab) (assoc res (first tab) (count (films-by-genre (first tab)))))
			res)))



(defn parse-rating
  "Construit un enregistrement de film depuis un entrée lue depuis CSV."
  [movieId rate]
    {(Integer/parseInt movieId) (Double/valueOf rate)})

(defn rating-map
  "Construit une map de rating à partir d'une base en CSV."
  [csv]
  (reduce (fn [r [userId movieId rating times]] ;;Prend le map et décompose csv en 3 parties
            (if-let [rate (parse-rating movieId rating)] ;;Création de la map {:1 5.0}
              (assoc r (Integer/parseInt userId) rate) ;;Construction de la map de retour {:1 {:1 5.0}}
              r))
          {} csv))


(def ratings (rating-map (rest (csv-seq "resources/ml-latest-small/ratings.csv"))))

(defn tr "prend les 10 premiers éléments de ratings"
	[]
	(take 10 ratings))


(defn moy "Calcul la moyenne d'un film id à partir de la map rate"
	[id]
	(loop [r ratings
		somme 0
		cpt 0]
		(if (seq r)
			(if (= (compare (first (first (second (first r)))) id) 0) 
				(recur (rest r) (+ somme (second (first (second (first r))))) (inc cpt))
				(recur (rest r) somme cpt))
			(if (= cpt 0)
				{id 0}
				{id (/ somme cpt)}))))


(defn movie-avg-rating []
	(reduce (fn [r mvs]
		(if-let [moyenne (moy (first mvs))] ;;(first mvs) permet de récupérer l'id du film 
				(conj r moyenne)
				r))
	{} movies))










