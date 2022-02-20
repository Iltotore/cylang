//TODO ecrire, lire

FONCTION puissance(x: reel, n: entier): reel
VARIABLE
  res: entier
  i: entier
DEBUT
  res <- 1
  POUR i DE 0 A n FAIRE
    res <- res * x
  FIN POUR
  RETOURNER res
FIN

FONCTION sqrt(x: reel): reel
DEBUT
  RETOURNER stdSqrt(x)
FIN

FONCTION min(x: reel, y: reel): reel
DEBUT
  SI y < x FAIRE
    RETOURNER y
  SINON
    RETOURNER x
  FIN SI
FIN

FONCTION max(x: reel, y: reel): reel
DEBUT
  SI y > x FAIRE
    RETOURNER y
  SINON
    RETOURNER x
  FIN SI
FIN

FONCTION factorielle(x: entier): entier
VARIABLE
  res: entier
  i: entier
DEBUT
  res <- 1
  POUR i DE 1 A x + 1 FAIRE
  res <- res * i
  FIN POUR
  RETOURNER res
FIN

FONCTION alea(x: entier): entier
DEBUT
  RETOURNER stdAlea(x)
FIN