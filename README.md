# CartoFreq : A shinyapp to display and make prediction map of frequency data in France

&nbsp;
&nbsp;




## Etapes de préparation des données annuelles
#### > Ouvrir le jeu de données (JDD) dans Excel (.xls ou .xlsx).

#### > Supprimer les formules pour ne garder que des caractères/chiffres dans les cellules.

#### > Apprêter les données.
**1-** Faire en sorte que la première ligne du tableur corresponde au nom des colonnes et supprimer toutes les colonnes inutiles, ne garder que : "code_essai" la variable contenant l'étiquette des différents essais (chaines de caractères ou nombres), "commune" la variable contenant le nom des communes (chaine de caractères), "numero_departement" la variable contenant le numéro du département français (nombre avec 1 ou 2 chiffres), "modalite" la variable contenant l’information du type de modalité (chaines de caractères ou nombre, ex : TR si traité et TNT sinon). Le tableur doit également contenir les colonnes qui correspondent aux fréquences de résistance d’intérêt (nombres entier compris entre 0 et 100, exprimés en %), le nom de ces colonnes reste au choix de l’utilisateur (noms courts et sans caractères spéciaux).
"code_essai", "commune", "numero_departement" et "modalite" sont des colonnes obligatoires, sans elles l'importation du fichier ne pourra pas se faire dans l'application.

**2-** Faire en sorte que la dernière ligne non-vide du tableur corresponde bien à la dernière observation de fréquence.

**3-** Supprimer toutes les lignes inutiles pour la cartographie : retirer les essais hors France métropolitaine, les essais où les fréquences n'ont pas été mesurées. 

**4-** Remplacer les caractères spéciaux présents dans les cellules en utilisant l’outil chercher/remplacer d’Excel : transformer les ";" (point-virgules), "'" (apostrophes) et "#" (symboles dièse) par des " " (espaces vides).

#### > Enregistrer le nouveau tableur au format CSV.
*Fichier* &#8594; *Enregistrer sous* &#8594; *Type : "CSV (séparateur : point-virgule)*. Cliquer sur accepter puis quittez sans enregistrer à nouveau (même si Excel vous le demande). Placer ce fichier dans le dossier /data.

&nbsp;
&nbsp;




## Utilisation de l'application : correction des erreurs
#### > Ouvrir le script CartoFreqApp.R dans Rstudio.
Sélectionner tout le code (Ctrl+A) et cliquer sur *Run* en haut à droite du script. Si certain packages sont manquants, installez-les grâce aux fonctions *install.packages()*.

#### > Importer des données.
Importer un JDD au format CSV dans l’application via le bouton *Importer un CSV*.

#### > Vérifier la cohérence des données.
Comparer l'affichage des données dans l'application avec le fichier CSV d'origine ouvert sous Excel : le nombre de lignes, colonnes, la présence et la forme des données, etc. 

#### > Corriger les erreurs.
Dans la partie "Affichage des erreurs", regarder et corriger dans le fichier CSV le maximum des erreurs renvoyées par l'application. Moins il y aura d'erreurs, moins l'application sera forcée à retirer des observations lors de l'analyse automatisée des données.
Concernant la colonne "commune" il peut être nécessaire de chercher dans le fichier *communesGPS.csv* afin de retrouver le bon nom de commune (ou un nom d'une commune proche).
Le fichier CSV peut être modifié en direct alors que celui-ci est importé dans l'application. Dans ce cas, après avoir modifié puis enregistrer les modifications (sous Excel), cliquer sur le bouton *Rafraîchi* pour automatiquement ré-importer le fichier.

&nbsp;
&nbsp;




## Utilisation de l'application : affichage des statistiques annuelles
#### > Cliquer sur le bouton *Afficher les statistiques annuelles* qui se situe sur le panneau de gauche sous le bouton d'importation 
Il est préférable d'avoir corrigé le maximum d'erreurs (notamment dans les colonnes département et fréquence) avant de calculer les fréquences moyennes régionales et nationale. Acronymes des régions (découpage administratif pré-réforme territoriale de 2015) : ALS, Alsace; AUV, Auvergne; AQU, Aquitaine; BNO, Basse-Normandie; BOU, Bourgogne; BRE, Bretagne; CEN, Centre, CHA, Champagne-Ardennes; FCO, Franche-Comté; HNO, Haute-Normandie; IDF, Ile-de-France; LAR, Languedoc-Roussillon; LIM, Limousin; LOR, Lorraine; MPY, Midi-Pyrénées; NPC, Nord-Pas-de-Calais; PCH, Poitou-Charentes; PDL, Pays de la Loire; PIC, Picardie; RAL, Rhône-Alpes. Ces acronymes sont compilés dans le fichier *departementsToRegions.csv*.
Il est conseillé de décocher le bouton d'affichage des statistiques annuelles avant la ré-importation d'un nouveau fichier CSV pour éviter les bugs.

&nbsp;
&nbsp;




## Utilisation de l'application : cartographie annuelle des fréquences
#### > Cliquer sur l'onglet *Cartographie* qui apparait après l'importation d'un fichier.

#### > Renseigner les informations nécessaires dans le formulaire à gauche.
L'année, le phénotype et la modalité choisi seront indiqués en titre du graphique. L'utilisateur à le choix de cartographier un type de modalité en particulier (ex: témoin non-traité VS modalité traitée), mais il peut également considérer que toutes les modalités sont des répétitions homogènes au sein d'un essai. 

#### > Si besoin, modifier l'échelle de couleur pour les fréquences.
Cliquer sur la palette pour redéfinir les couleurs aux seuil : 0%, 25%, 50%, 75% et 100%. L'application se charge automatiquement de réaliser le gradient de couleur correspondant.
L'échelle de couleur peut être sauvegardée au format PDF en cliquant sur le bouton *Exporter la légende au format PDF*. Le titre du fichier généré correspondra aux différents seuils de couleurs choisis (en code hexadécimal) pour ré-utilisation si nécessaire. Les couleurs du gradient de base peuvent être modifiées en changeant les valeurs dans la liste *defaultColors* du script R. Le titre intègre également la date (jj-mm-aaaa) à laquelle a été exporté le PDF.

#### > Cliquer sur le bouton *Soumettre*.
L'application affiche automatiquement le nombre de données de fréquences (hors NA), qui ont dûes être écartées de l'analyse par faute ou défaut d'information dans le JDD. 
La carte affiche les fréquences extrapolées sur le territoire français à partir des points d'observations des essais. Chaque point correspond à un essai. Dans chaque point, sont représentées sous forme de camembert les fréquences observées. Les fréquences observées sont affichées sous forme de camemberts. Les camemberts représentent la fraction de fréquences observées entre 0-25%, 26-50%, 51-75%, 76-100%. Les couleurs choisies pour représenter ces quatres catégories correspondent respectivement aux couleurs des fréquences 12%, 37%, 63% et 88%.

#### > Enregistrer la carte
Cliquer sur le bouton *Exporter au format PDF*. Le nom de fichier nouvellement créer comporte les informations du phénotype, de l'année et de la modalité cartographiés, ainsi que la date (jj-mm-aaaa) à laquelle a été exporté le PDF.

&nbsp;
&nbsp;




## Mise en forme d'un JDD pluri-annuel
#### > Compiler à la main un JDD pluri-annuel.
Après avoir réalisé les étapes de préparation des données et traité un maximum des erreurs retournées par l'application pour une première analyse annuelle, il est intéressant de compiler les différentes données annuelles dans un seul et même JDD pluri-annuel. L'objectif étant de réaliser une analyse dynamique de l'évolution des fréquences.
Pour cela, concaténer les JDD annuels dans un seul et même fichier Excel (copier-coller), en rajoutant l'information de l'année correspondant dans une nouvelle colonne "annee".
:warning: ATTENTION à bien faire coincider les colonnes entre-elles, en particulier les colonnes de fréquence. Si un nouveau type de fréquence apparait, ou qu'une fréquence n'est plus mesurée à une année donnée laisser les cases correspondantes vides.

#### > Importer le fichier pluri-annuel dans l'application
 

La page d’accueil d’indiquer si le JDD est pluri-annuel (permet d’afficher l’onglet prédiction). Mais l’application détectera automatiquement si la colonne "annee" est présente dans le JDD pour faire les cartographies annuelles ou l’affichage des statistiques annuelles. 
La page d’accueil permet d’indiquer si l’on souhaite afficher les fréquences moyenne pour un phénotype donné en France et dans les différentes régions de France. Si l’application détecte une colonne année, celle-ci demandera à l’utilisateur de choisir l’année. ATTENTION : bien faire les corrections sur le fichier (départements / fréquences) avant de calculer les fréquences moyennes, sinon des données seront retirées sans l’indiquer à l’utilisateur dans le calcul des fréquences. Si le numéro de département est manquant/inconnu, la fréquence associée ne participera pas à la fréquence moyenne de la région, mais participera tout de même dans la fréquence nationale.

&nbsp;
&nbsp;




## Utilisation de l'application : prédiction régionale des fréquences dans les parcelles non traitées

