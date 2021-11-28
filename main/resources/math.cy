//TODO sqrt() , puissance(), alea(), facto()
//test commentaire
PROGRAMME math
  FONCTION sqrt(x: reel): reel
  DEBUT
    RETOURNER stdSqrt(x)
  FIN

  FONCTION puissance(x: reel, y: entier): reel
  VARIABLE
    res : reel
  DEBUT
    res <- 1
    POUR i DE 0 A y FAIRE
      res <- res * x
    FIN POUR
    RETOURNER res
  FIN

  FONCTION alea(x: entier): entier
  DEBUT
    RETOURNER stdAlea(x)
  FIN

  FONCTION facto(x: entier): entier
  VARIABLE
    res : entier
  DEBUT
    res <- 1
    POUR i DE 1 A x+1 FAIRE
      res <- res * i
    FIN POUR
    RETOURNER res
  FIN
FIN

