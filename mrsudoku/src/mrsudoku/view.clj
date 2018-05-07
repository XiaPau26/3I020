
(ns mrsudoku.view
  (:require
   [mrsudoku.grid :as g]
   [mrsudoku.dpll :as dpll]
   [seesaw.core :refer [frame label text config! grid-panel
                        horizontal-panel vertical-panel button separator alert replace!
                        invoke-later pack! show!]]
   [seesaw.border :refer [line-border]]
   ))

(def default-color "white")
(def conflict-color "red")
(def set-color "blue")
(def solved-color "gray")
(def decode (dpll/s2))
(declare show-solved)

(defn mk-cell-view
  [cell cx cy ctrl]
  (case (:status cell)
    (:init :solved) (label :text (str (:value cell))
                 :h-text-position :center
                 :v-text-position :center
                 :halign :center
                 :valign :center
                 :background default-color)
    :empty (let [cell-widget (text :columns 1
                                   :halign :center
                                   :id (keyword (str "cell-" cx "-" cy))
                                   :foreground set-color
                                   :background default-color)]
             (config! cell-widget
                      :listen [:document
                               ;; XXX: normally, we should not depend from the controller
                               ;;      but it's an emblamatic counter-example
                               ((resolve 'mrsudoku.control/cell-input-handler) ctrl cell-widget cx cy)])
             cell-widget)
    (throw (ex-info "Can only build widget for :init or :empty cells." {:cell cell,
                                                                        :cx cx,
                                                                        :cy cy}))))

(defn mk-block-view
  [block bref ctrl]
  (let [cell-widgets (g/reduce-block
                      (fn [widgets _ cx cy cell]
                        (conj widgets (mk-cell-view cell cx cy ctrl))) [] block bref)]
    (grid-panel :rows 3
                :columns 3
                :hgap 3
                :vgap 3
                :border (line-border :thickness 2 :color "black")
                :items cell-widgets
                :id (keyword (str "block-" bref)))))

(defn mk-grid-view [grid ctrl]
  (let [block-widgets (for [i (range 1 10)]
                        (mk-block-view (g/block grid i) i ctrl))]
    (grid-panel :rows 3
                :columns 3
                :border 6
                :hgap 6
                :vgap 6
                :items (into [] block-widgets))))




;; The event parameter is an event object => if we indicate the event, the button will become the parent of the alert, the alert will
;; be place over the frame otherwise it will appear whatever it likes
(defn uneAlerte [event]
  (println "la grillllle résolue")
  (println (first decode)))




;; Seesaw creates label when it sees string
(defn mk-main-frame [grid ctrl]
  (let [grid-widget (mk-grid-view grid ctrl)
        main-frame (frame :title "MrSudoku"
                          :content (horizontal-panel
                                    :items [grid-widget
                                            [:fill-h 32]
                                            (vertical-panel
                                             :items [:fill-v
                                                     (grid-panel
                                                      :columns 1
                                                      :vgap 20
                                                      :items [(button :text "Load"
                                                                      :listen [:action (fn [event] (uneAlerte event))])
                                                              (button :text "Solve"
                                                                      :listen [:action (fn [event] (show-solved))])
                                                              (button :text "Quit")])
                                                     :fill-v])
                                            [:fill-h 32]])
                          :minimum-size [540 :by 380]
                          :on-close :exit)]
    (swap! ctrl #(assoc % :grid-widget grid-widget :main-frame main-frame))
    main-frame))

(defn update-cell-view!
  [cell cell-widget]
  (println "je suis dans update-cell-view")
  (case (:status cell)
    :conflict (config! cell-widget :background conflict-color) 
    (:set :init :empty) (config! cell-widget :background default-color)
    :solved (config! cell-widget :backround solved-color :editable? false)
    (throw (ex-info "Cannot update cell widget." {:cell cell :cell-widget cell-widget}))))


; controller maker
(defn mk-controller
  [grid] (atom {:grid grid}))



(defn show-solved []
  ;;méthode qui marche seulement à l'affichage
  ;(let [cell-widget (text :columns 1
   ;                                :halign :center
     ;                              :id (keyword (str "cell-" 3 "-" 1))
    ;                               :foreground set-color
      ;                             :background default-color)]
    ;((resolve 'mrsudoku.control/cell-validate-input!) ctrl cell-widget 3 1 4));
  
    ;; Méthode qui marche mais ouvre une deuxième fenêtre lorsqu'on clique sur le bouton load
    (let [ctrl (mk-controller decode)
      main-frame (mk-main-frame decode ctrl)]
    (invoke-later
     (-> main-frame
         pack!
         show!))
    ctrl))
