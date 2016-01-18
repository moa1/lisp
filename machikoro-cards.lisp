(let* ((placeholder (make-normal-card :dice '(1) :cost 0 :activation :any))
       (c (make-array 38 :element-type 'normal-card :initial-element placeholder)))
  (setf (aref c 0)
(make-normal-card :name "Weinberg"
                  :symbol :grain
                  :dice (list 7)
                  :cost 3
                  :activation :any
                  :color :blau
                  :description "Erhalte 3 Münzen aus der Bank."
                  :edition "?"))
  (setf (aref c 1)
(make-normal-card :name "Küstenfischerboot"
                  :symbol :boat
                  :dice (list 8)
                  :cost 2
                  :activation :any
                  :color :blau
                  :description "Hast du einen \"Hafen\" gebaut, erhältst du 3 Münzen aus der Bank."
                  :edition "?"))
  (setf (aref c 2)
(make-normal-card :name "Bergwerk"
                  :symbol :gear
                  :dice (list 9)
                  :cost 6
                  :activation :any
                  :color :blau
                  :description "Erhalte 5 Münzen aus der Bank."
                  :edition "base"))
  (setf (aref c 3)
(make-normal-card :name "Apfelplantage"
                  :symbol :grain
                  :dice (list 10)
                  :cost 3
                  :activation :any
                  :color :blau
                  :description "Erhalte 3 Münzen aus der Bank."
                  :edition "base"))
  (setf (aref c 4)
(make-normal-card :name "Hochseefischerboot"
                  :symbol :boat
                  :dice (list 12 13 14)
                  :cost 5
                  :activation :any
                  :color :blau
                  :description "Hast du einen \"Hafen\" gebaut, würfelt der Spieler, der an der Reihe ist, mit 2 Würfeln. Erhalte so viele Münzen aus der Bank wie er gewürfelt hat."
                  :edition "?"))
  (setf (aref c 5)
(make-normal-card :name "IT-Unternehmen"
                  :symbol :tower
                  :dice (list 10)
                  :cost 1
                  :activation :self
                  :color :pink
                  :description "Am Ende deines Zuges darfst du 1 deiner Münzen auf diese Karte legen. Würfelst du eine \"10\", erhältst du von jedem Mitspieler so viele Münzen, wie auf der Karte liegen."
                  :edition "?"))
  (setf (aref c 6)
(make-normal-card :name "Park"
                  :symbol :tower
                  :dice (list 11 12 13)
                  :cost 3
                  :activation :self
                  :color :pink
                  :description "Verteile die Münzen aller Spieler gleichmäßig unter allen Spielern neu. Sind nicht für alle gleich viel da, nimm den Rest aus der Bank."
                  :edition "?"))
  (setf (aref c 7)
(make-normal-card :name "Lebensmittelladen"
                  :symbol :bread
                  :dice (list 2)
                  :cost 0
                  :activation :self
                  :color :grün
                  :description "Hast du höchstens 1 Großprojekt gebaut, erhältst du 2 Münzen aus der Bank."
                  :edition "?"))
  (setf (aref c 8)
(make-normal-card :name "Bäckerei"
                  :symbol :bread
                  :dice (list 2 3)
                  :cost 0
                  :activation :self
                  :color :grün
                  :description "Erhalte 1 Münze aus der Bank."
                  :edition "base"))
  (setf (aref c 9)
(make-normal-card :name "Baufirma"
                  :symbol :bag
                  :dice (list 4)
                  :cost 2
                  :activation :self
                  :color :grün
                  :description "Du musst 1 deiner bereits gebauten Großprojekt wieder auf die \"im Bau\"-Seite drehen. Dafür erhältst du 8 Münzen aus der Bank."
                  :edition "?"))
  (setf (aref c 10)
(make-normal-card :name "Mini-Markt"
                  :symbol :bread
                  :dice (list 4)
                  :cost 2
                  :activation :self
                  :color :grün
                  :description "Erhalte 3 Münzen aus der Bank."
                  :edition "base"))
  (setf (aref c 11)
(make-normal-card :name "Molkerei"
                  :symbol :factory
                  :dice (list 7)
                  :cost 5
                  :activation :self
                  :color :grün
                  :description "Erhalte 3 Münzen aus der Bank für jedes deiner \"cow\"-Unternehmen."
                  :edition "base"))
  (setf (aref c 12)
(make-normal-card :name "Möbelfabrik"
                  :symbol :factory
                  :dice (list 8)
                  :cost 3
                  :activation :self
                  :color :grün
                  :description "Erhalte 3 Münzen aus der Bank für jedes deiner \"gear\" Unternehmen."
                  :edition "base"))
  (setf (aref c 13)
(make-normal-card :name "Weingut"
                  :symbol :factory
                  :dice (list 9)
                  :cost 3
                  :activation :self
                  :color :grün
                  :description "Erhalte 6 Münzen aus der Bank für jeden deiner Weinberge. Drehe danach die Karte auf \"im Urlaub\"."
                  :edition "?"))
  (setf (aref c 14)
(make-normal-card :name "Spedition"
                  :symbol :bag
                  :dice (list 9 10)
                  :cost 2
                  :activation :self
                  :color :grün
                  :description "Du musst 1 deiner Karten einem Mitspieler deiner Wahl geben. Kein \"tower\" Unternehmen. Dafür erhältst du 4 Münzen aus der Bank."
                  :edition "?"))
  (setf (aref c 15)
(make-normal-card :name "Getränkehersteller"
                  :symbol :factory
                  :dice (list 11)
                  :cost 5
                  :activation :self
                  :color :grün
                  :description "Erhalte 1 Münze aus der Bank für jede \"cup\" auf allen Unternehmen aller Spieler zusammen."
                  :edition "?"))
  (setf (aref c 16)
(make-normal-card :name "Markthalle"
                  :symbol :circle
                  :dice (list 11 12)
                  :cost 2
                  :activation :self
                  :color :grün
                  :description "Erhalte 2 Münzen aus der Bank für jedes deiner \"grain\" Unternehmen."
                  :edition "base"))
  (setf (aref c 17)
(make-normal-card :name "Lebensmittelgrossmarkt"
                  :symbol :factory
                  :dice (list 12 13)
                  :cost 2
                  :activation :self
                  :color :grün
                  :description "Erhalte 2 Münzen aus der Bank für jedes deiner \"cup\" Unternehmen."
                  :edition "?"))
  (setf (aref c 18)
(make-normal-card :name "Weizenfeld"
                  :symbol :grain
                  :dice (list 1)
                  :cost 0
                  :activation :any
                  :color :blau
                  :description "Erhalte 1 Münze aus der Bank."
                  :edition "base"))
  (setf (aref c 19)
(make-normal-card :name "Bauernhof"
                  :symbol :cow
                  :dice (list 2)
                  :cost 1
                  :activation :any
                  :color :blau
                  :description "Erhalte 1 Münze aus der Bank."
                  :edition "base"))
  (setf (aref c 20)
(make-normal-card :name "Maisfeld"
                  :symbol :grain
                  :dice (list 3 4)
                  :cost 2
                  :activation :any
                  :color :blau
                  :description "Hast du höchstens 1 Großprojekt gebaut, erhältst du 1 Münze aus der Bank."
                  :edition "?"))
  (setf (aref c 21)
(make-normal-card :name "Blumenfeld"
                  :symbol :grain
                  :dice (list 4)
                  :cost 2
                  :activation :any
                  :color :blau
                  :description "Erhalte 1 Münze aus der Bank."
                  :edition "?"))
  (setf (aref c 22)
(make-normal-card :name "Wald"
                  :symbol :gear
                  :dice (list 5)
                  :cost 3
                  :activation :any
                  :color :blau
                  :description "Erhalte 1 Münze aus der Bank."
                  :edition "base"))
  (setf (aref c 23)
(make-normal-card :name "Stadion"
                  :symbol :tower
                  :dice (list 6)
                  :cost 6
                  :activation :self
                  :color :pink
                  :description "Erhalte von jedem Mitspieler 2 Münzen."
                  :edition "base"))
  (setf (aref c 24)
(make-normal-card :name "Fernsehsender"
                  :symbol :tower
                  :dice (list 6)
                  :cost 7
                  :activation :self
                  :color :pink
                  :description "Erhalte von einem Mitspieler deiner Wahl 5 Münzen."
                  :edition "base"))
  (setf (aref c 25)
(make-normal-card :name "Bürohaus"
                  :symbol :tower
                  :dice (list 6)
                  :cost 8
                  :activation :self
                  :color :pink
                  :description "Tausche 1 Karte mit einem Mitspieler deiner Wahl. Kein \"tower\" Unternehmen."
                  :edition "base"))
  (setf (aref c 26)
(make-normal-card :name "Verlag"
                  :symbol :tower
                  :dice (list 7)
                  :cost 5
                  :activation :self
                  :color :pink
                  :description "Erhalte von jedem Mitspieler 1 Münze für jedes seiner \"cup\" und \"bread\" Unternehmen."
                  :edition "?"))
  (setf (aref c 27)
(make-normal-card :name "Gross-Reinigung"
                  :symbol :tower
                  :dice (list 8)
                  :cost 4
                  :activation :self
                  :color :pink
                  :description "Wähle ein Unternehmen (kein \"tower\"). Alle Spieler müssen ihre Karten dieses Unternehmens auf \"im Urlaub\" drehen. Erhalte 1 Münze aus der Bank für jede dieser Karten."
                  :edition "?"))
  (setf (aref c 28)
(make-normal-card :name "Finanzamt"
                  :symbol :tower
                  :dice (list 8 9)
                  :cost 4
                  :activation :self
                  :color :pink
                  :description "Erhalte von jedem Mitspieler, der 10 oder mehr Münzen besitzt, die Hälfte seiner Münzen (abgerundet)."
                  :edition "?"))
  (setf (aref c 29)
(make-normal-card :name "Pizzeria"
                  :symbol :cup
                  :dice (list 7)
                  :cost 1
                  :activation :any
                  :color :rot
                  :description "Erhalte 1 Münze von dem Mitspieler, der eine \"7\" gewürfelt hat."
                  :edition "?"))
  (setf (aref c 30)
(make-normal-card :name "Burger-Grill"
                  :symbol :cup
                  :dice (list 8)
                  :cost 1
                  :activation :any
                  :color :rot
                  :description "Erhalte 1 Münze von dem Mitspieler, der eine \"8\" gewürfelt hat."
                  :edition "?"))
  (setf (aref c 31)
(make-normal-card :name "Familienrestaurant"
                  :symbol :cup
                  :dice (list 9 10)
                  :cost 3
                  :activation :any
                  :color :rot
                  :description "Erhalte 2 Münzen von dem Mitspieler, der eine \"9\" oder \"10\" gewürfelt hat."
                  :edition "base"))
  (setf (aref c 32)
(make-normal-card :name "Cocktail-Club"
                  :symbol :cup
                  :dice (list 12 13 14)
                  :cost 4
                  :activation :any
                  :color :rot
                  :description "Hat der Mitspieler, der die \"12\", \"13\" oder \"14\" erzielt hat, 3 oder mehr Großprojekte gebaut, erhältst du alle seine Münzen."
                  :edition "?"))
  (setf (aref c 33)
(make-normal-card :name "Kreditinstitut"
                  :symbol :bag
                  :dice (list 5 6)
                  :cost 0
                  :activation :self
                  :color :grün
                  :description "Sofort nach dem Bau erhältst du 5 Münzen aus der Bank. Würfelst du eine \"5\" oder \"6\", musst du 2 Münzen an die Bank zahlen."
                  :edition "?"))
  (setf (aref c 34)
(make-normal-card :name "Blumenladen"
                  :symbol :bread
                  :dice (list 6)
                  :cost 1
                  :activation :self
                  :color :grün
                  :description "Erhalte 1 Münze aus der Bank für jedes deiner \"Blumenfelder\"."
                  :edition "?"))
  (setf (aref c 35)
(make-normal-card :name "Café"
                  :symbol :cup
                  :dice (list 3)
                  :cost 2
                  :activation :any
                  :color :rot
                  :description "Erhalte 1 Münze von dem Mitspieler, der eine \"3\" gewürfelt hat."
                  :edition "base"))
  (setf (aref c 36)
(make-normal-card :name "3-Sterne-Restaurant"
                  :symbol :cup
                  :dice (list 5)
                  :cost 3
                  :activation :any
                  :color :rot
                  :description "Hat der Spieler, der die \"5\" gewürfelt hat, 2 oder mehr Großprojekte gebaut, erhältst du von ihm 5 Münzen."
                  :edition "?"))
  (setf (aref c 37)
(make-normal-card :name "Sushi-Bar"
                  :symbol :cup
                  :dice (list 1)
                  :cost 2
                  :activation :any
                  :color :rot
                  :description "Hast du einen \"Hafen\" gebaut, erhältst du 3 Münzen von dem Mitspieler, der eine \"1\" gewürfelt hat."
                  :edition "?"))
  (alexandria:define-constant +normal-cards+ c :test #'equalp))

(let* ((placeholder (make-large-card :symbol :large :cost 0))
       (c (make-array 7 :element-type 'large-card :initial-element placeholder)))
  (setf (aref c 0)
(make-large-card :name "Rathaus"
                 :symbol :large
                 :cost 0
                 :description "Hast du zu Beginn deiner Bauphase 0 Münzen, erhältst du 1 Münze aus der Bank."
                 :edition "?"))
  (setf (aref c 1)
(make-large-card :name "Hafen"
                 :symbol :large
                 :cost 2
                 :description "Würfelst du 10 oder mehr, darfst du 2 Würfelaugen hinzuzählen."
                 :edition "?"))
  (setf (aref c 2)
(make-large-card :name "Bahnhof"
                 :symbol :large
                 :cost 4
                 :description "Würfle mit 1 oder 2 Würfeln."
                 :edition "base"))
  (setf (aref c 3)
(make-large-card :name "Einkaufszentrum"
                 :symbol :large
                 :cost 10
                 :description "Erhalte 1 Münze mehr für jedes deiner \"cup\" und \"bread\" Unternehmen."
                 :edition "base"))
  (setf (aref c 4)
(make-large-card :name "Freizeitpark"
                 :symbol :large
                 :cost 16
                 :description "Würfelst du zwei gleiche Zahlen, hast du einen weiteren Zug."
                 :edition "base"))
  (setf (aref c 5)
(make-large-card :name "Funkturm"
                 :symbol :large
                 :cost 22
                 :description "Einmal pro Zug darfst du erneut würfeln."
                 :edition "base"))
  (setf (aref c 6)
(make-large-card :name "Flughafen"
                 :symbol :large
                 :cost 30
                 :description "Baust du in deinem Zug kein Unternehmen oder Großprojekt, erhältst du 10 Münzen aus der Bank."
                 :edition "?"))
  (alexandria:define-constant +large-cards+ c :test #'equalp))
