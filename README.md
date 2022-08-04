<p align="center">
  <img src="logo.png" alt="Logo", width="353", height="170.5"/>
</p>

Le langage de programmation basé sur le pseudo-code de CYTech.   

Read the English version [here.](https://github.com/Iltotore/cylang/blob/master/README-EN.md)

## Introduction

CYLang est un projet visant à uniformiser et rendre exécutable le pseudo-code de l'école CYTech
dans le but de faciliter l'introduction à l'informatique aux élèves en première année de cycle (Pré Ing 1/Ing 1)

Le projet se concentre sur trois points fondamentaux :
- Permettre aux étudiants de tester leur algorithme écrit en pseudo-code
- Permettre aux enseignants de tester leurs exemples et de les rendre interactifs
- Uniformiser la syntaxe du pseudo-code entre les différents TD/Enseignants

## Utilisation

*Note : Cette section est susceptible de changer au fur et à mesure du développement de la première release*

[**Lien vers les téléchargements**](https://github.com/Iltotore/cylang/releases)

### Exécutable natif (dépend de la plateforme)

Vous pouvez télécharger l'exécutable natif (sans extension ou .exe pour Windows) et le lancer via la commande suivante :

| Système d'exploitation | Lancer un programme             | Voir la version de CYLang |
|------------------------|---------------------------------|---------------------------|
| Windows                | `.\cylang.exe <nom du fichier>` | `.\cylang.exe --version`  |
| Linux                  | `./cylang <nom du fichier>`     | `./cylang --version`      |

L'exécutable natif est entièrement autonome et ne nécessite pas de prérequis.

### Exécutable Java (universel)

Vous pouvez télécharger l'exécutable Java (JAR) et le lancer via la commande `java -jar cylang.jar <nom du fichier>`.

Vous pouvez vérifier la version de CYLang avec la commande `java -jar cylang.jar --version`.

L'exécutable Java nécessite d'avoir au moins Java 8 ou plus installé sur l'ordinateur.


## Fonctionnement interne

CYLang est interpréteur écrit en [Scala](https://scala-lang.org).
Le fonctionnement général d'un interpréteur est de transformer le code source (étape appelée "parsing") en un arbre appelé AST qui pourra être parcouru à diverses fins :
- Evaluation: "exécuter" l'AST
- Typechecking : Vérifier la cohérence des types des différentes opérations (exemple : ne pas soustraire un texte à un nombre)
- Résolution du scope : Vérifier que chaque variable/fonction/etc utilisée existe
- etc...

Le projet est découpé en trois principales catégories de modules :
- Module principal : Le module contenant le code principal de l'interpréteur (du parsing à l'évaluation), utilisé comme bibliothèque par les autres modules
- Modules de tests : Généralement appelés `test`, contiennent les tests associés à leur module parent (exemple : `main/test` contient les tests du module principal (`main`))
- Autres modules : Principalement les front-ends (interfaces) comme l'interface en ligne de commande

## Contribuer

CYLang est ouvert à toute contribution peu importe le status ou le niveau du contributeur (interne ou externe à l'école).

### Issues

Vous pouvez signaler un bug ou proposer un ajout en ouvrant une [Issue](https://github.com/Iltotore/cylang/issues).
Veuillez simplement respecter le schéma indiqué au moment de rédiger son contenu.

### Modifications du code

#### Préparer son environnement de travail

CYLang utilise un outil nommé [Mill](https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html) qui permet de facilement préparer votre espace de travail.

Si vous utilisez un IDE ou un éditeur de texte comme Intellij IDEA (avec le plugin Scala) ou Visual Studio Code (avec le plugin Metals),
vous pouvez y connecter Mill en utilisant la commande `mill mill.bsp.BSP/install` et en important le projet via BSP (détecté automatiquement sur la plupart des éditeurs)

Vous pouvez ainsi compiler votre code via la commande `mill <nom du module>.compile` et le tester en utilisant `mill <nom du module>.test`

Notes:
- La tâche `test` exécute automatiquement la tâche `compile` si elle n'a pas été lancée avant. Vous n'avez donc pas besoin de `compile` puis `test`.
  Vous pouvez directement utiliser `module.test`.
- Sur Windows, si vous obtenez systématiquement une erreur à la fin d'une tâche lancée via `mill <tâche>`, faites `mill -i <tâche>` à la place.
- Le bouton "build" de la plupart des IDEs marchent et vous pouvez vous en servir à la place de `mill module.compile`.
- Vous pouvez exécuter une tâche (comme `test`) sur tous les modules qui la possèdent en utilisant `__`. Exemple: `mill __.test`

#### Pull Requests
Les [Pull Requests](https://github.com/Iltotore/cylang/pulls) (PR) sont un moyen simple et stable de proposer des modifications/ajouts dans le code de CYLang.

Le processus est le même que pour les autres projets hébergés sur Github ou plateforme similaire :
- Créer un fork du projet (une version alternative automatiquement hébergée sur votre compte)
- Faire les changements à proposer sur votre fork (renseignez vous sur l'utilisation de l'outil Git)
- Ouvrez une Pull Request (Github vous le proposera automatiquement sur la page de votre fork) en décrivant les changements effectués
  et en utilisant le mot clé `Closes #XX` si cette PR ferme/répond à une ou plusieurs issues.

Notez que seules les Pull Requests passant tous les tests seront acceptées. Si ce n'est pas le cas et que l'erreur vient de vos modifications,
vous serez simplement invités à corriger le bug introduit.


## Questions

Pour toute question concernant CYLang, voici les différents moyens de la poser:
- Ouvrir une Issue (Voir la section "Issues")
- Envoyer un mail à `fromentinr@cy-tech.fr` (mail universitaire de l'auteur principal)
- Envoyer un message privé à `Il_totore#9133` (compte de l'auteur principal) sur Discord
