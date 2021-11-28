//TODO écrire stdlib, valider intégration CYLang-> Scala, ecrire(), lire()

PROGRAMME stdlib
  PROCEDURE ecrire(x: string): string
  DEBUT
    stdPrintln(x)
  FIN

  FONCTION ecrire(x: type) : type
  DEBUT
    x <- stdScan(x)
  FIN
FIN