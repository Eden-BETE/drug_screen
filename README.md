# APP for drug prediction 

## Présentation de l'application 

## Onglet 1

## Onglet Analyse molécule-cluster

Cet onglet réalise une analyse exploratoire des données de sensibilité aux médicaments (AUC) à travers les 7 sous-types moléculaires de cancer du sein identifiés dans le projet. L'objectif est de visualiser quels médicaments sont les plus efficaces selon le sous-type cellulaire, et de tester statistiquement si ces différences sont significatives.

> ⚠️ Les calculs ne sont faits que sur les lignées testées pour chaque médicament. Les données manquantes ne sont pas affichées (absence de test).

### Données utilisées 

L'onglet s'appuie sur le fichier `data/drug_screen_clean.csv`, généré par `data_prep.R`. Ce fichier est produit à partir de deux sources brutes:
 
- **`PRISM Repurposing Secondary Subsetted.csv`** — valeurs d'AUC pour les médicaments testés sur des lignées cellulaires (Broad Institute/Depmap)
- **`classification_uploadedData.csv`** — classification des 70 lignées sein en 7 sous-types (cRegMap)

