# Compilateur Mini-ADA

Ce projet consiste en la réalisation d'un compilateur (lexeur, parseur, typeur et compilateur) pour le langage Mini-ADA, écrit en OCaml. Ce projet a été réalisé par [Lucas Willems](http://www.lucaswillems.com) pour le cours de [Langages de programmation et compilation](https://www.lri.fr/~filliatr/ens/compil/) donné par Jean-Christophe Filliâtre pour la L3 d'informatique de l'ENS Ulm.

## Structure du projet

Le projet (dans le dossier `src`) contient un `Makefile` permettant d'exécuter les 3 commandes :
- `make` : pour compiler le compilateur
- `make clean` : pour nettoyer les fichiers produits lors de la compilation
- `make tests` : pour tester le compilateur sur la série de programmes contenue dans le dossier `tests`

Et 6 fichiers :
- `ast.mli` : contenant les différentes syntaxes abstraites (après parsage et après typage)
- `lexer.mll` : transformant un fichier `txt` en une suite de lexemes
- `parser.mly` : transformant la suite de lexemes en un arbre de syntaxe abstraite
- `typer.ml` : vérifiant la correction de l'arbre de syntaxe abstraite et créant un nouvel arbre de syntaxe abstraite prêt à la compilation
- `compiler.ml` : transformant l'arbre de syntaxe abstraite en du code assembleur "x86-64"
- `main.ml` : corps du compilateur, liant les fichiers précédents

La bibliothèque tierce "x86-64", créée par Jean-Christophe Filliâtre (CNRS) et Kim Nguyen (Université Paris Sud), est utilisée pour la génération du code assembleur.

## Utilisation du compilateur

Pour utiliser le compilateur, il faut d'abord compiler son code source avec la commande `make`. Le compilateur est alors créé sous le nom `adac` (pour ADA Compiler). Celui prend différents paramètres :
- `(filename)` : définit le nom du fichier à compiler. Celui-ci doit avoir l'extension `ada`.
- `--parse-only` : parse seulement le fichier (n'effectue pas l'étape de typage et de compilation)
- `--type-only` : parse et type seulement le fichier (n'effectue pas l'étape de compilation)

Voici un exemple d'utilisation :

```
./adac ../tests/exec/bst.adb --type-only
```