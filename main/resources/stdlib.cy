PROGRAMME stdlib
  PROCEDURE ecrire(x: string): string
  DEBUT
    stdPrintln(x)
  FIN

  FONCTION lire(x: type) : type
  DEBUT
    x <- stdLire(x)
  FIN
FIN