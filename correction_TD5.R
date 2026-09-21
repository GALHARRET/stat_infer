load("data/Agneaux.RData")
Agneaux
load("data/Veaux.RData")
Veaux$
# HSP signifie Hauteur Sacro-Pubienne. 
# BIM : largeur bi-iliaque médiane.
# LB : longueur du bassin
# LH : largeur des Hanches.  
# Plus l'ouverture pelvienne de la mère est grande, plus le veau (même s'il a un poids de naissance élevé) pourra naître facilement
# En anatomie vétérinaire, le canon désigne la région de la patte située entre le genou (ou le jarret) et le boulet.
# Mode Velage : 1 (sans aide), 2 (aide facile), 3 (aide difficile), 4 (césarienne)
table(Veaux$ModeVelage)

#  Gain Moyen Quotidien de poids de l'agneau entre son 10ème et son 30ème jour de vie GMQ2
(Agneaux$P2-Agneaux$P1)/20
#  Gain Moyen Quotidien de poids de l'agneau entre son 30ème et son 70ème jour de vie GMQ3
(Agneaux$P3-Agneaux$P2)/40
