(require '[clojure.string :as string])

(def bowl-state (ref {}))
(def bowl-ingredients (ref {}))
(def pan-state (ref {}))
(def hand-state (ref nil))
(def cooling-rack (ref {}))

(def ingredients #{:egg :flour :milk :sugar :butter})
(def grabbables #{:egg :cup :butter})
(def scoopables #{:flour :milk :sugar})
(def squeezables #{:egg})

;; just to get a clean state again

(defn start-over
  "Clean up the bakery, empty hands, clean out the bowl, get a new pan. Good when you mess up and need a fresh start."
  []
  (dosync
   (ref-set bowl-state {})
   (ref-set bowl-ingredients {})
   (ref-set pan-state {})
   (ref-set hand-state nil)
   (ref-set cooling-rack {}))
  :ok)



(defn grab
  "Grab an item from the bakery (:egg, :butter, or :cup)."
  [ingredient]
  (cond
   (not (grabbables ingredient))
   (do
     (println "I cannot grab" ingredient)
     :error)
   @hand-state
   (do
     (println "I already have" (name @hand-state) "in my hand.")
     :error)
   :else
   (dosync
    (ref-set hand-state ingredient)
    :ok)))

(defn scoop
  "If you are holding the cup, scoop up something into your measuring cup
   (milk, flour, or sugar)."
  [ingredient]
  (cond
   (not= @hand-state :cup)
   (do
     (println "I need to have the cup in my hand to scoop.")
     :error)

   (not (scoopables ingredient))
   (do
     (println ingredient "is not scoopable.")
     :error)

   :everything-is-ok
   (dosync
    (ref-set hand-state (keyword (str "cup-of-" (name ingredient))))
    :ok)))

(defn squeeze
  "Squeeze the egg you are holding to crack it."
  []
  (cond
   (nil? @hand-state)
   (do
     (println "I am not holding anything.")
     :error)

   (not (squeezables @hand-state))
   (do
     (println "I am holding" @hand-state ", which is not squeezable.")
     :error)

   :its-squeezable
   (dosync
    (alter hand-state #(keyword (str "squeezed-" (name %))))
    :ok)))

(defn release
  "Release whatever you are holding in your hand."
  []
  (cond
   (nil? @hand-state)
   (do
     (println "I am not holding anything.")
     :error)

   :else
   (dosync
    (ref-set hand-state nil)
    :ok)))

(defn from-cup [x]
  (and
   (= (seq "cup-of-") (take 7 (name x)))
   (keyword (.substring (name x) 7))))

(defn from-squeezed [x]
  (and
   (= (seq "squeezed-") (take (count "squeezed-") (name x)))
   (keyword (.substring (name x) (count "squeezed-")))))

(defn add-to-bowl
  "Add what is in your hand to the bowl."
  []
  (cond
   (nil? @hand-state)
   (do
     (println "I am not holding anything.")
     :error)

   (= :butter @hand-state)
   (dosync
    (alter bowl-ingredients update-in [:butter] (fnil inc 0))
    (ref-set hand-state nil)
    (alter bowl-state assoc :mixed false)
    :ok)

   (= :egg @hand-state)
   (do
     (println "Shouldn't I break the egg I am holding first?")
     :error)

   (= :cup @hand-state)
   (do
     (println "My cup is empty.")
     :error)

   (from-cup @hand-state)
   (dosync
    (alter bowl-ingredients update-in [(from-cup @hand-state)] (fnil inc 0))
    (ref-set hand-state :cup)
    (alter bowl-state assoc :mixed false)
    :ok)

   (from-squeezed @hand-state)
   (dosync
    (alter bowl-ingredients update-in [(from-squeezed @hand-state)] (fnil inc 0))
    (ref-set hand-state nil)
    (alter bowl-state assoc :mixed false)
    :ok)

   :otherwise
   (do
     (println "I'm lost.")
     :error)))

(defn mix
  "Mix the contents of the bowl."
  []
  (cond
   (empty? @bowl-state)
   (do
     (println "The bowl is empty.")
     :error))
  :else
  (dosync
   (alter bowl-state assoc :mixed true)
   :ok))

(defn add-contents [a b]
  (if (or (nil? a) (map? a))
    (reduce #(update-in %1 [(%2 0)] (fn [x y] (if x (+ x y) y)) (%2 1))
            a b)
    :randomness))

(defn pour-into-pan
  "Pour the contents of the bowl into the baking pan."
  []
  (cond
   (not (:mixed @bowl-state))
   (do
     (println "The bowl is unmixed.")
     :error)

   :else
   (dosync
    (alter pan-state update-in [:contents] add-contents @bowl-ingredients)
    (ref-set bowl-ingredients {})
    (ref-set bowl-state {})
    :ok)))

(defn bake [pan-contents minutes]
  (cond
   (= pan-contents
      {:flour 1
       :egg 1
       :sugar 1
       :butter 1})
   (cond
    (< minutes 30)
    :mushy-mess
    (> minutes 30)
    :burned-mess
    (= minutes 30)
    :cookies)

   (= pan-contents
      {:flour 2
       :egg 2
       :milk 1
       :sugar 1})
   (cond
    (< minutes 25)
    :mushy-mess
    (> minutes 25)
    :burned-mess
    (= minutes 25)
    :cake)

   (nil? pan-contents)
   (do
     (println "Baking an empty pan.")
     nil)

   :else
   :randomness))

(defn bake-pan
  "Put the pan in the oven and bake it for so many minutes."
  [minutes]
  (cond
    (nil? (:contents @pan-state))
    (do
      (println "There is nothing to bake: the pan is empty!")
      :error)
    (number? minutes)
    (do
      (println "Baking" minutes "minutes. . .")
      (dosync
        (alter pan-state update-in [:contents] bake minutes)
        (alter pan-state assoc :baking true))
      (println "Done!")
      (println "The result is" (name (:contents @pan-state)))
      :ok)
    :else
    (do
      (println "I need a number of minutes to bake. You gave me a" (type minutes))
      :error)))

(defn cool-pan
  "Remove the pan from the oven and put it in the cooling rack. Also grab a new pan to make something new! Returns the name of the cooling rack where the pan was placed."
  []
  (let [r (gensym "cooling-rack-")]
   (dosync
    (alter cooling-rack assoc r (:contents @pan-state))
    (ref-set pan-state {}))
   (println "Cooling pan!")
   (println "I now have a fresh pan.")
   r))

(defn status
  "Look around and report the status of the bakery."
  []
  (dosync
   (print "Available ingredients: ")
   (if (seq ingredients)
     (println (string/join ", " (map name ingredients)))
     (println "none"))

   (print "In hand: ")
   (if @hand-state
     (println (string/replace (name @hand-state) "-" " "))
     (println "nothing"))

   (print "In bowl (")
   (if (:mixed @bowl-state)
     (print "mixed")
     (print "unmixed"))
   (print "): ")
   (if (seq @bowl-ingredients)
     (println (string/join ", " (for [[k v] @bowl-ingredients]
                                  (if (scoopables k)
                                    (if (= 1 v)
                                      (str v " cup of " (name k))
                                      (str v " cups of " (name k)))
                                    (if (= 1 v)
                                      (str v " " (name k))
                                      (str v " " (name k) "s"))))))
     (println "nothing"))

   (print "In pan")
   (if (:baking @pan-state)
     (print " (in oven)")
     (print ""))
   (print ": ")
   (cond
    (nil? (:contents @pan-state))
    (println "nothing")

    (keyword? (:contents @pan-state))
    (println (name (:contents @pan-state)))

    :else
    (do
     (println (string/join ", " (for [[k v] (:contents @pan-state)]
                                  (if (scoopables k)
                                    (str v " cups of " (name k))
                                    (str v " " (name k) "s")))))
     (println "nothing")))
   (print "On the cooling rack: ")
   (if (seq @cooling-rack)
     (println (string/join ", " (for [[k v] @cooling-rack]
                                  (str (name v) " in " (string/replace (name k) "-" " ")))))
     (println "nothing")))
  :ok)

(defn bakery-help
  "Print out some helpful text to remember the available commands."
  []
  (println "Welcome to the bakery!")
  (println)
  (println "Available commands are: ")
  (println "grab \t\t Pick something up.")
  (println "squeeze \t Squeeze whatever you are holding.")
  (println "release \t Release whatever you are holding.")
  (println "scoop \t\t If you are holding the cup, fill it with an ingredient.")
  (println "add-to-bowl \t Add the ingredient you are holding to the mixing bowl.")
  (println "mix \t\t Mix the ingredients in the bowl.")
  (println "pour-into-pan \t Pour the contents of the bowl into the pan.")
  (println "bake-pan \t Put the pan in the oven for a certain number of minutes.")
  (println "cool-pan \t After baking, put the pan on the cooling racks to cool.")
  (println "\t\t Returns the id of the cooling rack.")
  (println "status \t\t Print out the status of the bakery.")
  (println "start-over \t If you get lost, this command will reset the bakery.")
  (println)
  (println "bakery-help \t Print out this message.")

  'welcome)

(bakery-help)
