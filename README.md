# CartoFreq : A shinyapp to display map of frequency data

## Etapes de préparation des données anuuelles
### Ouvrir le jeu de données (.xlsx) dans Excel.

### Supprimer les formules pour ne garder que des caractères/chiffres dans les cellules.

### Apprêter les données annuelles
1- Faire en sorte que la première ligne du tableur corresponde au nom des colonnes et supprimer toutes les colonnes inutiles, ne garder que : « code_essai » la variable contenant l'étiquette des différents essais (chaines de caractères ou nombres), « commune » la variable contenant le nom des communes (chaine de caractères), « numero_departement » la variable contenant le numéro du département français (nombre avec 1 ou 2 chiffres), « modalite » la variable contenant l’information du type de modalité (chaines de caractères ou nombre, ex : TR si traité et TNT sinon). Le tableur doit également contenir les colonnes qui correspondent aux fréquences de résistance d’intérêt (nombres entier compris entre 0 et 100, exprimés en %), le nom de ces colonnes reste au choix de l’utilisateur (noms courts et sans caractères spéciaux).

2- Faire en sorte que la dernière ligne du tableur non-vide corresponde bien à la dernière observation de fréquence.

3- Supprimer toutes les lignes inutiles pour la cartographie : retirer les essais hors France métropolitaine, les essais où les fréquences n'ont pas été mesurées. 

4- En utilisant l’outil chercher/remplacer d’Excel, remplacer les caractères spéciaux présents dans les cellules restantes : « ; » point-virgule, « ‘ » apostrophe et « # » symbole dièse, par des espaces vides (ex : L’herbergement#MOD001 devient L herbergement MOD001) ;
-Si besoin, sélectionner toutes les cellules et faire Clic-droit – Format des cellules – Personnalisée – mettre \”@\” dans Type. Valider avec OK ;

Enregistrer le nouveau tableur au format .csv. Sous excel : Fichier – Enregistrer sous – Type : « CSV (séparateur : point-virgule). Cliquer sur accepter et quittez sans enregistrer à nouveau (si Excel vous le demande).

Lancer l’application user.R
Importer le nouveau fichier dans l’interface via le bouton Browse. N’importequel fichier importer doit se trouver dans le dossier « /data » pour permettre l’importation.
Vérifier l’adéquation des données entre l’interface graphique et le tableur (bon nombre de lignes, colonnes, présence et forme des données, etc.)
Vérifier les erreurs pour chacune des colonnes : supprimer du fichier .csv toute ligne inutile pour limiter le nombre d’erreurs/lignes retirées données par l’application. Concernant la correction des noms de commune, les modifier dans le document sans mettre les « - » qui sont dans le nom.
Accéder à l’onglet Cartographie, renseigner le formuler et appuyer sur le bouton Soumettre pour générer la cartographie. L’année servira pour le titre du graphique

L’analyse spatiale permet de distinguer plusieurs modalité TNT / TR mais potentielle des n° de traitements. L’analyse ressort de manière automatique un test permettant de comparer les modalité deux à deux. Une cartographie peut être réalisée en prenant en compte l’effet des différentes modalités (e.g. traité vs. non-traité, ou TR1 vs. TR2 vs TNT), ou en ne considérant pas ces différences de modalité de traitement (i.e. toutes les observations d’un même essai sont considérées comme étant des répétitions équivalentes entre-elles : « All »). Pour l’instant seules les modalités TR et TNT ne ressortent pas d’erreurs et peuvent être considérées dans le modèle, mais libre à l’utilisateur d’ajouter de nouvelles modalité (TRx) dans le vecteur au début du script R.

La page d’accueil d’indiquer si le JDD est pluri-annuel (permet d’afficher l’onglet prédiction). Mais l’application détectera automatiquement si la colonne « annee » est présente dans le JDD pour faire les cartographies annuelles ou l’affichage des statistiques annuelles. 
La page d’accueil permet d’indiquer si l’on souhaite afficher les fréquences moyenne pour un phénotype donné en France et dans les différentes régions de France. Si l’application détecte une colonne année, celle-ci demandera à l’utilisateur de choisir l’année. ATTENTION : bien faire les corrections sur le fichier (départements / fréquences) avant de calculer les fréquences moyennes, sinon des données seront retirées sans l’indiquer à l’utilisateur dans le calcul des fréquences. Si le numéro de département est manquant/inconnu, la fréquence associée ne participera pas à la fréquence moyenne de la région, mais participera tout de même dans la fréquence nationale.
