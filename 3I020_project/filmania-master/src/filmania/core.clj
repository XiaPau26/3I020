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
(declare transfo)
(declare card)

(defn -main [& args]
	(println (card-genres)))



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
  "Construit une map de films à partir d'un base en CSV."
  [csv]
  (reduce (fn [m [movie-id title-year genres]]
            (if-let [movie (parse-movie title-year genres)]
              (assoc m (Integer/parseInt movie-id) movie)
              m))
          {} csv))

;; Attention: gros fichier
(def movie-filename "resources/ml-latest-small/movies.csv")

(def movies (movie-map (rest (csv-seq movie-filename))))

(defn cptBase []
	(print "Il y a ")
	(print (count movies))
	(println " Films de base"))


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
	(loop [m movies
		tab (all-genres)
		res {}]
		(if (seq m)
			(recur (rest m) (rest tab) (assoc res (keyword (first tab)) (count (films-by-genre (first tab)))))
			res)))


(defn transfo [tab]
	(loop [t tab
		res {}]
		(if (seq t)
			(recur (rest t) (assoc res (keyword (first t))))
			res)))


(defn card [res genre]
	(println genre)
	(loop [g genre
		r res]
		(if (seq g)
			(recur (rest g) (assoc r (first g) (inc (get r (keyword (first g))))))
			r)))




