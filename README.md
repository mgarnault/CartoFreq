# CartoFreq : A shinyapp to display and make prediction map of frequency data in France.


&nbsp;


&nbsp;

## Etapes de préparation des données annuelles.
#### > Ouvrir le jeu de données (JDD) dans Excel (.xls ou .xlsx).


&nbsp;

#### > Réaliser un premier enregistrement **brut** au format CSV.
*Fichier* &#8594; *Enregistrer sous* &#8594; *Type : "CSV (séparateur : point-virgule)*. Cliquer sur accepter puis quittez sans enregistrer à nouveau (même si Excel vous le demande).


&nbsp;

#### > Apprêter les données.
**1-** Faire en sorte que la première ligne du tableur corresponde au nom des colonnes.
Les noms de colonnes obligatoires sont : "code_essai" la variable contenant l'étiquette des différents essais (chaines de caractères ou nombres qui doivent être **UNIQUE** pour chaque essai différent d'une année donnée), "commune" la variable contenant le nom des communes (chaine de caractères), "numero_departement" la variable contenant le numéro du département français (nombre avec 1 ou 2 chiffres), "modalite" la variable contenant l’information du type de modalité (chaines de caractères "TR" si traité, "T0" et "TNT" sinon). Sans ces colonnes l'importation du fichier dans l'application échouera. Le nom de ces colonnes sont stockées dans le vecteur *mandatoryVar* du script de l'appplication.

Le tableur doit également contenir les colonnes qui correspondent aux fréquences de résistance d’intérêt (nombres entiers compris entre 0 et 100, exprimés en %). Les colonnes fréquences reconnues par l'application sont stockées dans le vecteur *usualFrequencies* du script de l'application.

D'autres colonnes peuvent figurer dans le tableau mais elles ne seront pas accessibles dans l'application (ex: "code_INRA" qui permet de faire le lien entre les différents JDD, ou "traitement_1"/"traitement_2"/"traitement_3"/"traitement_4" qui contiennent l'information de la composition des traitements sur les parcelles traitées).

Concernant la colonne "code_essai", elle peut parfois comporter des valeurs fausses (répétées sur plusieurs essais manifestement différents) ou manquantes, ce qui pose problème lors de la modélisation des données. Il peut être intéressant de corriger les valeurs de la colonne "code_essai" en se basant sur l'information du préleveur et de l'organisme commanditaire. Pour ce faire dans Excel, aller dans l'onglet *Accueil* &#8594; *Tirer et Filtrer* &#8594; choisir *Tri personnalisé* et trier par "code_essai", cliquer sur *Ajouter un niveau* et classer dans un second temps par "commune" (cela permet de mieux se rendre compte de l'homogénéité ou non des noms des essais). Enfin, étiqueter la "code_essai" au mieux.
Si cette procédure de correction est trop longue (ou pas satisfaisante), laisser tomber la colonne "code_essai" et laisser la telle quelle. Dans le script de l'application la partie *# règle les problèmes de la colonne code-essai* se chargera de remplacer l'information de la colonne "code_essai" par la colonne "commune" qui comporte bien moins de valeurs manquantes et/ou fausses. Cette procédure remplacera les effets aléatoires liés aux essais dans les modèles par des effets aléatoires liés aux communes (la quantification de bruit sera donc moins locale, mais c'est mieux que rien !). En revanche remplacer le "code_essai" par la "commune" est déconseillé si l'on souhaite faire une analyse de l'effet des traitements parcellaires, car alors, il sera devenu impossible de retrouvé le témoin associé dans l'essai (sauf s'il n'y a systématiquement qu'un seul essai par commune, mais c'est rarement le cas).


&nbsp;

**2-** Faire en sorte que la dernière ligne non-vide du tableur corresponde bien à la dernière observation de fréquence.


&nbsp;

**3-** Supprimer toutes les lignes inutiles pour la cartographie : retirer les essais hors France métropolitaine, les lignes où aucune fréquence n'a été mesurée.


&nbsp;

**4-** Remplacer les caractères spéciaux encore présents dans les cellules en utilisant l’outil chercher/remplacer d’Excel : transformer les ";" (point-virgules), "'" (apostrophes), et "#" (symboles dièse) par des " " (espaces vides). Transformer les "," (virgules) en "." (points).


&nbsp;

#### > Réaliser un second enregistrement **corrigé** au format CSV.


&nbsp;


&nbsp;

## Utilisation de l'application : correction des erreurs.
#### > Ouvrir le script CartoFreqApp.R dans Rstudio.
Sélectionner tout le code (Ctrl+A) et cliquer sur *Run* en haut à droite du script. Si certain packages sont manquants, installez-les grâce aux fonctions *install.packages()*.


&nbsp;

#### > Importer des données.
Importer un JDD au format CSV dans l’application via le bouton *Importer un CSV*.


&nbsp;

#### > Vérifier la cohérence des données.
Comparer l'affichage des données dans l'application avec le fichier CSV d'origine ouvert sous Excel : le nombre de lignes, colonnes, la présence et la forme des données, etc.


&nbsp;

#### > Corriger les erreurs.
Dans la partie "Affichage des erreurs", regarder et corriger dans le fichier CSV le maximum des erreurs renvoyées par l'application. Moins il y aura d'erreurs, moins l'application sera forcée à retirer des observations de fréquence lors de l'analyse automatisée des données. Concernant la colonne "commune" il peut être nécessaire de chercher dans le fichier *communesGPS.csv* afin de retrouver le bon nom de commune (ou un nom d'une commune proche en se basant sur le code postal).
Le fichier CSV peut être modifié en direct alors que celui-ci est déjà importé dans l'application. Dans ce cas, après avoir modifié puis enregistrer les modifications (sous Excel dans le fichier CSV), cliquer sur le bouton de rafraîchissement automatique se situant à droite du bouton d'importation, afin de ré-importer le fichier courant.


&nbsp;


&nbsp;

## Utilisation de l'application : affichage des statistiques annuelles.
#### > Cliquer sur le bouton *Afficher les statistiques annuelles* qui se situe sur le panneau de gauche sous le bouton d'importation.
Il est préférable d'avoir corrigé le maximum d'erreurs (notamment dans les colonnes "numero_departement" et de la fréquence considérée) avant de calculer les fréquences moyennes régionales et nationale. Si un numero de département est manquant/faux la fréquence associée ne sera pas considérée dans la moyenne régionale. En revanche, toutes les fréquences observées dans le JDD seront considérées dans la moyenne nationale. C'est pourquoi il est important de bien avoir supprimé toutes les lignes correspondantes à des observations dans d'autres pays. Dans le tableau affichant les fréquences moyennes, sont associés le nombre d'observations sur lequelles la moyenne a été calculé (colonne "n").

Acronymes des régions (découpage administratif avant la réforme territoriale de 2015) : ALS, Alsace; AUV, Auvergne; AQU, Aquitaine; BNO, Basse-Normandie; BOU, Bourgogne; BRE, Bretagne; CEN, Centre, CHA, Champagne-Ardennes; FCO, Franche-Comté; HNO, Haute-Normandie; IDF, Ile-de-France; LAR, Languedoc-Roussillon; LIM, Limousin; LOR, Lorraine; MPY, Midi-Pyrénées; NPC, Nord-Pas-de-Calais; PCH, Poitou-Charentes; PDL, Pays de la Loire; PIC, Picardie; RAL, Rhône-Alpes. Ces acronymes sont compilés dans le fichier *departementsToRegions.csv*.


&nbsp;


&nbsp;

## Utilisation de l'application : cartographie annuelle des fréquences.
#### > Cliquer sur l'onglet *Cartographie* qui apparait après l'importation d'un fichier CSV valide.


&nbsp;

#### > Renseigner les informations nécessaires dans le formulaire à gauche.
L'année, le phénotype et la modalité choisi seront indiqués en titre du graphique. L'utilisateur à le choix de cartographier un type de modalité en particulier, mais il peut également considérer que toutes les modalités sont des répétitions homogènes au sein d'une même étiquette "code_essai" (qui peut correspondre à la commune, *cf.* fin de la Partie1-Chapitre 1).


&nbsp;

#### > Si besoin, modifier l'échelle de couleur pour les fréquences.
Cliquer sur la palette pour redéfinir les couleurs aux seuil : 0%, 25%, 50%, 75% et 100%. L'application se charge automatiquement de réaliser le gradient de couleur correspondant. L'échelle de couleur peut être sauvegardée au format PDF en cliquant sur le bouton *Exporter la légende au format PDF*. Le titre du fichier généré correspondra aux différents seuils de couleurs choisis (en code hexadécimal) pour ré-utilisation future si nécessaire. Les couleurs du gradient de base (à l'ouverture de l'application) peuvent être modifiées en changeant les valeurs dans la liste *defaultColors* du script de l'application. Le titre intègre également la date (jj-mm-aaaa) à laquelle a été exporté le PDF de la légende de couleur.


&nbsp;

#### > Cliquer sur le bouton *Soumettre*.
L'application affiche automatiquement le nombre de fréquences (hors NA), qui ont dûes être écartées de l'analyse par faute et/ou défaut d'information dans les autres colonnes du JDD. La carte affiche les fréquences extrapolées sur le territoire français à partir des points d'observations des essais. Chaque point correspond à un essai (ou commune, *cf.* fin de la Partie1-Chapitre 1). Dans chaque point, sont représentées sous forme de camembert les fréquences observées. Les camemberts représentent la fraction de fréquences observées entre 0-25%, 26-50%, 51-75%, 76-100%. Les couleurs choisies pour représenter ces quatres catégories correspondent respectivement aux couleurs des fréquences 12%, 37%, 63% et 88% (*i.e.* couelurs des fréquences médianes de chacune des catégories).


&nbsp;

#### > Enregistrer la carte.
Cliquer sur le bouton *Exporter au format PDF*. Le nom de fichier nouvellement créer comporte les informations du phénotype, de l'année et de la modalité cartographiés, ainsi que la date (jj-mm-aaaa) à laquelle a été exporté le PDF de la cartographie.


&nbsp;


&nbsp;

## Mise en forme d'un JDD pluri-annuel
#### > Compiler à la main un JDD pluri-annuel.
Il peut être intéressant de compiler les différentes données annuelles dans un seul et même JDD pluri-annuel afin de pouvoir réaliser une analyse dynamique de l'évolution des fréquences années après années. Pour cela, il faut concaténer les JDD annuels dans un seul et même fichier CSV sous Excel. Il faudra alors ajouter une colonne au sein du JDD pluri-annuel : "annee"; qui permet de distinguer les différentes années d'observation dans le JDD pluri-annuel.

:warning: Lors de la compilation pluri-annuelles sous Excel bien faire attention à faire coincider les bon noms de colonnes entre-elles (les JDD annuels ne sont pas forcément homogènes entre-eux). Par exemple, si un nouveau type de fréquence apparait, ou qu'une fréquence n'est plus mesurée à une année donnée laisser les cases où la fréquence n'est plus/n'a pas été mesurée vides.


&nbsp;

#### > Importer le fichier pluri-annuel dans l'application.
Après avoir sélectionné le JDD pluri-annuel lors de l'importation d'un fichier CSV, cliquer sur le bouton *Données pluri-annuelles* afin de faire apparaitre un nouvel onglet : *Prediction*. L'application retournera une erreur si la colonne "annee" n'est pas présente dans le JDD. Si l'utilisateur ne coche pas la case *Données pluri-annuelle*, l'onglet *Prediction* n'apparaitra pas, mais l'application détecteraquand même automatiquement la présence/absence de la colonne "annee", et proposera donc de sélectionner l'année parmi celles présentes dans le JDD lors du calcul des statistiques annuelles ou des cartographies des fréquences par exemple.


&nbsp;


&nbsp;

## Utilisation de l'application : prédiction régionale des fréquences dans les parcelles non traitées.
#### > Cliquer sur l'onglet *Prediction*.


&nbsp;

#### > Choisir la fréquence à étudier.

#### > Choisir la plage temporelle d'apprentissage du modèle dynamique.
Utiliser le slider pour sélectionner la plage temporelle sur lequel le modèle dynamique va estimer les taux de croissance. Plus la plage temporelle sélectionnée sera grande plus l'estimation sera robuste. Par défaut le slider année comporte un plage temporelle d'étude "optimale" qui est calculée automatiquement par l'application. Cette plage temporelle correspond à la plus longue plage temporelle continue où les fréquences du phénotype étudié sont assez différentes de 0% et de 100% (*i.e.* années où les fréquences 0% et 100% ne représentent pas plus de 90% des fréquences observées). Les années hors de cette plage temporelle sont considérées comme inutiles du point de vue de l'analyse de la dynamique d'évolution des fréquences. En effet, si trop de 0% sont observés le phénotype est probablement encore dans sa phase d'émergence (ou des modèles de présence/absence seraient peut-être plus adaptés) ou alors déjà éteind. En revanche, si trop de 100% sont observées le phénotype est complètement généralisé et il est alors très difficile de quantifier des effets de sélection.

:warning: Bien que l'application propose une plage temporelle d'étude optimale celle-ci peut parfois être inadaptée. En effet, en plus d'avoir des fréquences assez différentes de 0% et 100%, il est conseillé de considérer une plage temporelle "homogène" du point de vue de la dynamique d'évolution de la fréquence (évolution strictement positive, ou strictement négative, ou stagnante). Par exemple, une courbe en cloche (avec une première phase de sélection positive, puis une seconde phase de sélection négative), sera mal fittée par le modèle car celui-ci estime des taux de croissance et/ou effets de sélection constants au cours du temps. Dans les cas où il est possible de ditinguer plusieurs phases dans l'évolution de la fréquence, il fortement recommandé de faire autant d'analyses que l'on distingue de phases d'évolution.


&nbsp;

#### > Choisir l'indice de confiance souhaité.
L'indice de confiance correspond au nombre minimal de point observé en moyenne par région et par an pour réaliser l'estimation de la dynamique. Lorsque cette moyenne est strictement en dessous de l'indice de confiance souhaité dans une région donnée, alors tous les points observés dans cette région sont retirés de l'analyse. En effet, un trop faible répétition de l'échantillonage dans le temps entrainerait une estimation peu robuste de la dynamique. Il est important de noter que l'ajout et le retrait des données de certaines régions (par modification de l'indice de confiance) peuvent jouer sur les valeurs estimées des taux de croissance nationaux et régionnaux. C'est le rôle de l'utilisateur de faire son choix et de trouver une cohérence dans les différentes estimations aux différents indices de confiance testés.


&nbsp;

#### > Lancer l'analyse : cliquer sur le bouton *Soumettre*.
Le tableau qui s'affiche sur le panneau de droite correspond à l'estimation des taux de croissance en échelle logit : la ligne FRANCE correspond au taux de croissance global national : une valeur positive (respectivement négative) implique une augmentation (respectivement diminution) globale de la fréquence en France. Les autres lignes correspondent au taux de croissance régionnaux **relativement** au taux national. Les taux de croissance régionnaux absolus s'obtiennent en sommant le taux de croissance national au taux relatifs régionnaux.

Exemple : Si on observe +0.86 (\*\*\*) en FRANCE, -0.5 () dans REGION1 et +0.32 (\*) dans REGION2, cela signifie que la fréquence augmente de manière significative en FRANCE (**+0.86**), que la fréquence augmente également dans REGION1 mais que la dynamqiuye régionale ne diffère pas significativement de la tendance nationale (+0.68-0.5=**+0.36**), alors que la sélection du phénotype dans la REGION2 est significativement accélérée par rapport à la tendance nationale (+0.86+0.32=**+1.18**).
La fitness relative apparente du phénotype (*i.e.* vitesse relative à laquelle le phénotype évolue dans la population vis-à-vis des autres phénotypes complémentaires dans la population : résistant vs. sensible pour les phénotypes qualitatifs; résistant 1 vs. autres résistants pour les phénotypes quantitatifs), s'obtient en prenant l'exponentielle des taux de croissance du tableau. Si on reprend le même exemple on a donc un phénotype de résistance qui croit exp(0.86)=2.36 fois plus vite que le/les phénotypes complémentaires dans la popuation. Une fitness relative apparente <1 indique une décroissance de la fréquence dasn la population, >1 indique une augmentation de la fréquence dans la population, ~1 indique une fréquence stable.
L'EFD (Expected Frequency Difference), correspond au changement de fréquence observé entre deux années pour une population initialement prise à sa moyenne observée dans les données. Si la fréquence moyenne du phénotype étudié dans les données est de 25% (F=0.25), l'EFD se calcule comme suit : (F - *logit*<sup>-1</sup>(*logit*(F) + 0.86)) \* 100= pour reprendre l'exemple précédent. Ceci indique que globalement en France, on tend à observe une augmentation annuelle de  

#### > Changement des couleurs de la palette graphique.
La palette graphique utilisée pour généré les graphiques de prédiction correspond à celle utilisée dans l'onglet *Cartographie*. Modifier la palette et l'exporter à partir de cet onglet, puis revenir à l'onglet *Prédiction*. Si les cartographies de prédiction ont déjà été générées et que l'échelle de couleur a été modifiée par la suite, il est nécessaire de resoumettre l'anlyse prédictive pour appliquer le changement de couleur sur les cartographies.

#### > Ajout de variables explicatives régionales.
Une fois l'importation des fichiers terminée, re-soumettre l'analyse.

Les données de surfaces cultivées en blé sont tirées de JDD d'AGRESTE (extraction *via* ARVALIS) : superficie en blé dur et en blé tendre d'hiver, exprimées en HA, par département et par an (/data/SurfacesBlé/Surfaces.xlsx). Ces données ont été mises enregistrées au format CSV (séparateur:point-virgule) dans deux fichiers distincts : un pour le blé dur (/data/SurfacesBDH.csv); et un pour le blé tendre (data/SurfacesBTH.csv). Ces deux JDD ont été mise en forme (/data/SurfacesBDH - Corrige.csv & SurfacesBTH - Corrige.csv) : supression des premières lignes inutiles, ajout d'une colonne "HA" qui correspond aux surfaces cultivées (value * 1000), suppression des caractères spéciaux dans les noms de départements ("'", ";", ... remplacés par des espaces vides " "), sélection des colonnes d'intérêt renommées en "departement", "annee" et "HA". Enfin, l'utilisation du script *Build_SurfacesBle.R* permet de fusionner les deux JDD et de mettre les bons acronymes des régions, pour finalement obtenir la donnée de surface cultivée en blé (dur+tendre) exprimée en HA, par région et par an. Ces données sont à compléter et à recompiler chaque année.

Les données d'utilisation des fongicides sont tirées de JDD de BAYER : hectares déployés par susbtance active, par région et par an (/data/Fongicides/FongiCéré_Qté_HA_AI_1990-2019_Blé.xlsx). C'est données ont été enregistrées au format CSV (séparateur:point-virgule) dans le fichier /data/PanelFongicides.csv. Ce JDD a été mise en forme (/data/PanelFongicides - Corrige.csv) : suppression des premières lignes inutiles, supression des colonnes liées à la quantité de matière active exprimée en KG, renommage des colonnes "matiere_active" et "region". Enfin, l'utilisation du script *Build_UtilisationFongicide.R* permet de transformer légèrement le JDD afin d'attribuer les modes d'actions aux différentes substances actives (*via* le fichier Affiliation_MatieresActives.csv), et de mettre les bons acronymes des régions, pour finalement avoir obtenir la donnée de l'utilisation des substances actives exprimée en hectares déployés, par région et par an. Ces données sont à compléter et à recompiler chaque année.
