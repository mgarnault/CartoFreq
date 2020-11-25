### NETTOYAGE DE LA MEMOIRE ###
rm(list=ls())






### PACKAGES ###
# install.packages("shiny")
library("shiny")
# install.packages("shinyjs")
library("shinyjs")
# install.packages("shinybusy")
library("shinybusy")
# install.packages("shinyFiles")
library("shinyFiles")
# install.packages("shinyWidgets")
library("shinyWidgets")
# install.packages("shinyBS")
library("shinyBS")
# install.packages("colourpicker")
library("colourpicker")
# install.packages("dplyr")
library("dplyr")
# install.packages("stringr")
library("stringr")
# install.packages("ggplot2")
library("ggplot2")
# install.packages("varhandle")
library("varhandle")
# install.packages("spaMM")
library("spaMM")
# install.packages("scatterpie")
library("scatterpie")
# install.packages("DT")
library("DT")
# install.packages("matrixStats")
library("matrixStats")






### VARIABLES GLOBALES ###
## Set de variable obligatoires et accessoires pour déterminer les colonnes qui seront des fréquences de résistance (i.e. les autres colonnes) ##
mandatoryVar=c("code_essai","commune","numero_departement","modalite")
accessoryVar=c("annee")
# accessoryVar=c("annee","latitude","longitude") # AJOUTER UNE POSSIBILITE D'AJOUTER DIRECTEMENT LES COORDONNEES GPS ?


## Noms des modalités connues par défaut par l'application, les autres formes sortiront une erreur ##
usualModilities=c("TR","TNT","T0")


## Nom des fréquences connues par défaut par l'application ##
usualFrequencies=c("BenR","StrR","CarR","TriS","TriLR","TriMR","TriR1.R3","TriR2.R4","TriR5","TriR6","TriR7.R8","MDR","TriHR")


## Palette de couleur par defaut utlisée pour les cartographies ##
defaultColors=list("0"="#00CD00","25"="#FFFF00","50"="#FFA500","75"="#FF0000","100"="#8B0000")


## Environnement local du script (/path) pour localiser plus facilement les data ##
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # /!\ le script R doit toujours être placé à la racine du dossier /CartoFreq






### FONCTIONS ###
## Fonction qui permet d'homogénéiser le nom des communes entre les différents data ##
formatCommune=function(data,column){
  return(
    sapply(data[,column],function(x){
      x=str_to_upper(iconv(as.character(x),to="ASCII//TRANSLIT"))
      if(grepl("-",x)){x=gsub("-"," ",x)}
      if(grepl("SAINT ",x)){x=gsub("SAINT ","ST ",x)}
      if(grepl("SAINTE ",x)){x=gsub("SAINTE ","STE ",x)}
      return(trimws(gsub("\\s+"," ",x)))
    })
  )
}


## Fonction qui permet de réaliser le krigeage spatial ##
fillMAP=function(predict,coords,color.range){
  # Formule du krigeage
  map.formula=as.formula(paste(attr(predict,"fittedName"),"~1+Matern(1|",paste(coords,collapse="+"),")"))
  
  # Krigeage
  smoothObject=fitme(map.formula,data=predict,method="REML")
  smoo=predict(smoothObject,binding="dummy")
  
  # Création de la matrice des fréquences extrapolées
  x=smoo[,coords[1]]
  y=smoo[,coords[2]]
  margin=1/2 # marge prise pour l'extrapolation : max = +Inf ; min = 0
  xrange=range(x)
  margex=(xrange[2]-xrange[1])*margin
  xrange=xrange+margex*c(-1,1)
  yrange=range(y)
  margey=(yrange[2]-yrange[1])*margin
  yrange=yrange+margey*c(-1,1)
  plot.axes=quote({axis(1);axis(2)})
  gridSteps=400 # finesse (résolution) de la grille d'extrapolation
  xGrid=seq(xrange[1],xrange[2],length.out=gridSteps)
  yGrid=seq(yrange[1],yrange[2],length.out=gridSteps)
  
  # Création d'un data frame pour stocker les fréquences extrapolées
  newdata=expand.grid(xGrid,yGrid)
  colnames(newdata)=coords
  gridpred=predict(smoothObject,newdata=newdata)
  matrixplot=cbind(newdata,gridpred[,1])
  colnames(matrixplot)=c("CX","CY","Pred")
  matrixplot[,"Pred"]=round(100*matrixplot[,"Pred"])
  
  # Retrait des valeurs abbérantes
  if(length(which(matrixplot[,"Pred"]>100))>0){matrixplot[which(matrixplot[,"Pred"]>100),"Pred"]=100}
  if(length(which(matrixplot[,"Pred"]<0))>0){matrixplot[which(matrixplot[,"Pred"]<0),"Pred"]=0}
  
  # Association des fréquences à une couleur
  matrixplot=cbind(matrixplot,Freq=matrixplot[,"Pred"])
  matrixplot[,"Pred"]=color.range[(matrixplot[,"Pred"]+1)]
  
  return(as.data.frame(matrixplot))
}


## Fonction qui permet de dessiner les cartographies ##
MakeCartoPlot=function(phenotype,annee,modalite,color.range,colorLegend,plotRatio,ext_FR,ext_FRplot,krig,d2,regionsplot,regionsplotNOBSV){ 
  p=ggplot() # creation du plot par phenotype et par annee
  
  p=p+scale_x_continuous(limits=c(ext_FR$range[1]/plotRatio,ext_FR$range[2]/plotRatio),expand=c(0,0)) # dimension de l'axe x
  p=p+scale_y_continuous(limits=c(ext_FR$range[3],ext_FR$range[4]),expand=c(0,0)) # dimension de l'axe y
  p=p+coord_equal()
  
  p=p+geom_point(aes(x=CX/plotRatio,y=CY),data=krig,color=krig$Pred,pch=15,size=1) # arriere-plan couleur indiquant les frequences extrapolees par krigeage

  colorLegend=colorLegend[as.numeric(which(apply(d2[which(colnames(d2)%in%c("F0.to.25","F26.to.50","F51.to.75","F76.to.100"))],
                                                 2,
                                                 function(x){!all(x==0)})))]
  p=p+scale_fill_manual(values=colorLegend)
  
  p=p+theme(panel.background=element_blank(), # arriere-plan blanc
            legend.position="none", # pas de legende
            plot.margin=margin(c(0,0,0,0),unit="cm"), # pas de marges (respect du ratio X/Y France)
            axis.line.x=element_blank(), # pas de ligne representant l'axe des x
            axis.title.x=element_blank(), # pas de titre a l'axe des x
            axis.ticks.x=element_blank(), # pas de graduation sur l axe des x
            axis.text.x=element_blank(), # pas d'annotation sur l'axe des x
            axis.line.y=element_blank(), # de meme pour l'axe y ....
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.y=element_blank())
  
  p=p+geom_polygon(aes(x=X/plotRatio,y=Y),data=ext_FRplot,fill="white",color="white",size=0) # delimination de la France
  
  p1=p
  p1=p1+geom_polygon(aes(x=X/plotRatio,y=Y,group=Region),data=regionsplot,fill="NA",color="black",size=1) # delimination des regions
  p1=p1+geom_polygon(aes(x=X/plotRatio,y=Y,group=Region),data=regionsplotNOBSV,fill="grey50",color="black",size=1) # grisage des regions non observees a l'annee du graphique
  p1=p1+geom_point(aes(x=longitude/plotRatio,y=latitude),data=d2,color="black",pch=20,size=10) # arriere-plan des points de couleur des frequences observees
  p1=p1+geom_scatterpie(aes(x=longitude/plotRatio,y=latitude,r=0.08),data=d2,cols=c("F0.to.25","F26.to.50","F51.to.75","F76.to.100"),color=NA)
  p1=p1+geom_text(aes(x=as.numeric(quantile(ext_FR$x,.25,na.rm=T))/plotRatio,y=as.numeric(quantile(ext_FR$y,.975,na.rm=T))),label=paste0(phenotype," (",annee," - ",modalite,")"),size=8) # titre annee
  
  p2=p
  p2=p2+geom_polygon(aes(x=X/plotRatio,y=Y,group=Region),data=regionsplot,fill="NA",color="black",size=.75) # delimination des regions
  p2=p2+geom_polygon(aes(x=X/plotRatio,y=Y,group=Region),data=regionsplotNOBSV,fill="grey50",color="black",size=.75) # grisage des regions non observees a l'annee du graphique
  p2=p2+geom_point(aes(x=longitude/plotRatio,y=latitude),data=d2,color="black",pch=20,size=6) # arriere-plan des points de couleur des frequences observees
  p2=p2+geom_scatterpie(aes(x=longitude/plotRatio,y=latitude,r=0.08),data=d2,cols=c("F0.to.25","F26.to.50","F51.to.75","F76.to.100"),color=NA)
  p2=p2+geom_text(aes(x=as.numeric(quantile(ext_FR$x,.25,na.rm=T))/plotRatio,y=as.numeric(quantile(ext_FR$y,.975,na.rm=T))),label=paste0(phenotype," (",annee," - ",modalite,")"),size=5) # titre annee
  
  return(list(toPlot=p1,toSave=p2))
}


## Fonction qui permet de créer une liste de plot correspondant à la prédiction des fréquences au temps t+1 à t+5 ##
MakePredictionPlots=function(namePhenotype,yearRange,color.range,plotRatio,ext_FR,regionsplot,regionsplotPred,predicts,confidenceIndex){
  pList=list()
  count=0
  
  for(Time in min(predicts$t):max(predicts$t)){
    count=count+1
    
    regionsplotPred$Color=sapply(regionsplotPred$Region,function(x){
      freq=predicts$Prediction[which(predicts$region==x & predicts$t==Time)]
      return(color.range[freq+1])
    })
    
    p=ggplot()
    p=p+scale_x_continuous(limits=c(ext_FR$range[1]/plotRatio,ext_FR$range[2]/plotRatio),expand=c(0,0)) # dimension de l'axe x
    p=p+scale_y_continuous(limits=c(ext_FR$range[3],ext_FR$range[4]),expand=c(0,0)) # dimension de l'axe y
    p=p+coord_equal()
    p=p+theme(panel.background=element_blank(), # arriere-plan blanc
              legend.position="none", # pas de legende
              plot.margin=margin(c(0,0,0,0),unit="cm"), # pas de marges (respect du ratio X/Y France)
              axis.line.x=element_blank(), # pas de ligne representant l'axe des x
              axis.title.x=element_blank(), # pas de titre a l'axe des x
              axis.ticks.x=element_blank(), # pas de graduation sur l axe des x
              axis.text.x=element_blank(), # pas d'annotation sur l'axe des x
              axis.line.y=element_blank(), # de meme pour l'axe y ....
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.y=element_blank())
    p=p+geom_polygon(aes(x=X/plotRatio,y=Y,group=Region)
                      ,data=regionsplot,fill="grey",color="black",size=.75) # grisage des regions non observees
    p=p+geom_polygon(aes(x=X/plotRatio,y=Y,group=Region),
                       data=regionsplotPred,fill=regionsplotPred$Color,color="black",size=.75) # affichage de la prédiction dans les régions monitorées
    p=p+geom_text(aes(x=as.numeric(quantile(ext_FR$x,.175,na.rm=T))/plotRatio,
                        y=as.numeric(quantile(ext_FR$y,.975,na.rm=T))),
                    label=paste0("Prédiction ",namePhenotype," en ",(max(yearRange)+count),"\n",
                                 "(Indice de confiance = ",confidenceIndex,"/10)","\n",
                                 "(Apprentissage sur ",min(yearRange)," - ",max(yearRange),")"),size=5) # titre 
    
    pList[[count]]=p
    names(pList)[count]=as.character(Time)
  }
  
  return(pList)
}





### IMPORTATION DES FICHIERS OBLIGATOIRES ###
## Ficher des coordonnees GPS a partir de la commune ##
Q=read.table(paste0(getwd(),"/data/CommunesGPS.csv"),header=TRUE,sep=";",
             stringsAsFactors=FALSE,colClasses=c(code_postal="character"))


## Fichier d'association des departements aux regions ##
deptReg=read.table(paste0(getwd(),"/data/departementsToRegions.csv"),header=TRUE,sep=";",
                   stringsAsFactors=FALSE)
deptReg$Departement=sapply(deptReg$Departement,function(x){ # homogeneisation des numeros de departement
  if(nchar(x)==1){return(paste0("0",x))}else{return(paste0(x))}
})


## Fichiers contenant les polygones (coordonnees GPS) de France metropolitaine et des regions ##
ext_FR=readRDS(paste0(getwd(),"/data/layer_ext_france_VF.rds"))
ext_FRplot=data.frame(X=ext_FR$x[-length(ext_FR$x)],Y=ext_FR$y[-length(ext_FR$y)])

regions=readRDS(paste0(getwd(),"/data/layer_regions_VF.rds"))
regionTEMP=c()
count=1
for(j in regions$x){
  if(is.na(j)){
    regionTEMP=c(regionTEMP,NA)
    count=count+1
  }else{
    regionTEMP=c(regionTEMP,regions$names[count])
  }
}
regionsplot=data.frame(X=regions$x,Y=regions$y,Region=regionTEMP) # tableau contenant les polygones de toutes les regions
regionsplot=regionsplot[-which(is.na(regionsplot$X)),] 






### APPLICATION SUR NAVITAGEUR INTERNET ###
if(interactive()){
  
  
  
  
  ### Interface graphique ###
  ui <- fluidPage(useShinyjs(),
                  useSweetAlert(),
                  position="left",
                  div(HTML("<h1>Outil <em>CartoFreq</em></h1>")), # Titre
                  sidebarLayout(
                    
                    ## Panel proposant l'importation + corrections ##
                    sidebarPanel(
                      conditionalPanel(condition="input.tabs==1",
                                       div(HTML("<p style=\"font-size:20px;font-weight:bold\">Importation des fichiers :</p>")),
                                       fluidRow(column(4,shinyFilesButton(id="file1",label="Importer un CSV",title="",multiple=FALSE,icon=icon("file-upload"))),
                                                column(1,uiOutput("refreshData"))),
                                       uiOutput("pluriannual"),
                                       htmlOutput("fileName"),
                                       hr(),
                                       uiOutput("annualStats"),
                                       uiOutput("phenotypeNameStats"),
                                       uiOutput("yearNameStats"),
                                       uiOutput("modalityNameStats"),
                                       tableOutput("tableStats"),
                                       hr(),
                                       uiOutput("columnChoiceErr"),
                                       htmlOutput("errors")),
                      conditionalPanel(condition="input.tabs==2",
                                       uiOutput("columnChoicePlot"),
                                       uiOutput("yearName"),
                                       uiOutput("modalityChoicePlot"),
                                       fluidRow(column(10,imageOutput("legendPic",height="30px")),
                                                column(2,uiOutput("graphicsButton"))),
                                       uiOutput("legendSave"),
                                       hr(),
                                       uiOutput("cartoGO"),
                                       uiOutput("cartoSave")
                      ),
                      conditionalPanel(condition="input.tabs==3",
                                       uiOutput("columnChoicePlotPred"),
                                       uiOutput("sliderY"),
                                       uiOutput("sliderConfidenceIndex"),
                                       tableOutput("resultSummary"),
                                       hr(),
                                       div(HTML("<p style=\"font-size:20px;font-weight:bold\">Importation de variables explicatives :</p>")),
                                       fluidRow(column(4,shinyFilesButton(id="file2",label="Importer un Panel",title="",multiple=FALSE,icon=icon("file-upload"))),
                                                column(1,uiOutput("deleteExplanatoryData"))),
                                       uiOutput("fileName2"),
                                       uiOutput("showExplanatoryData"),
                                       hr(),
                                       uiOutput("predGO"),
                                       uiOutput("cartoPredSave"))),
                    
                    ## Panel affichant le fichier importé sour la forme d'un tableau ##
                    mainPanel(
                      tabsetPanel(id="tabs",
                                  tabPanel(title=h4("Tableau"),value="1",DT::dataTableOutput("contents"))))))
  
  
  
  
  ### Serveur ###
  server <- function(input,output,session){
    
    
    
    ## ONGLET 1 : Tableau ##
    ## Importation du tableau de donnees et bouton de rafraichissement ##
    shinyFileChoose(input,"file1",roots=c(wd='.'),filetypes=c("csv")) # bouton d'importation des fichiers CSV
    file1 <- reactiveVal(NULL)
    observeEvent(input$file1,{
      file1(input$file1)
    })
    
    currentFilePath <- reactiveVal(NULL) # variable stockant le chemin du fichier importe
    data <- reactiveVal(NULL)
    observeEvent(c(input$file1,input$refresh),{
      # reset du data
      currentFilePath(NULL)
      data(NULL)
      
      # reset les valeurs réactives de l'onglet 1 : Tableau
      selectedPhenotypeStats(NULL)
      selectedYearStats(NULL)
      selectedModalityStats(NULL)
      annualStatsTable(NULL)
      showStats(NULL)
      
      # reset les valeurs réactives de l'onglet 2 : Cartographie
      plottedPhenotype(NULL)
      plottedYear(NULL)
      plottedModality(NULL)
      plotCarto(NULL)
      
      # reset les valeurs réactives de l'onglet 3 : Prédiction
      file2(NULL)
      currentFilePath2(NULL)
      data2(NULL)
      selectedColumnPlotPred(NULL)
      timeRange(NULL)
      confidenceIndex(NULL)
      estimateSummary(NULL)
      plotPredList(NULL)
      plotPred(NULL)
      
      # reset des onglets
      removeTab(inputId="tabs",target="2")
      removeTab(inputId="tabs",target="3")
      
      inFile=parseFilePaths(roots=c(wd='.'),file1())
      if(length(inFile$datapath)==0){
        return(NULL)
      }

      currentFilePath(paste0(getwd(),substr(as.character(inFile$datapath),2,nchar(as.character(inFile$datapath))))) # enregistrement du chemin pour affichage
      dTemp=read.table(currentFilePath(),header=TRUE,sep=";",
                       stringsAsFactors=FALSE,na.strings=c("","-"))
      
      if(!all(mandatoryVar%in%colnames(dTemp))){ # test de l'existence de toutes les variables obligatoires dans le fichier importé par l'utilisateur
        sendSweetAlert(session,title="Erreur",
                       text=paste0("Toutes les colonnes obligatoires (",paste(mandatoryVar,collapse=", "),") ne sont pas présentes dans le fichier."),
                       type="error")
        file1(NULL)
        currentFilePath(NULL)
        return(NULL)
      }
      
      dTemp$commune=formatCommune(dTemp,"commune") # homogénéisation du nom des communes du JDD importé + apport de corrections des noms de communes
      dTemp$numero_departement=as.character(dTemp$numero_departement)
      if(file.exists(paste0(getwd(),"/data/CorrectionsCommunes.csv"))){
        dCorrection=read.table(paste0(getwd(),"/data/CorrectionsCommunes.csv"),
                               header=TRUE,sep=";",stringsAsFactors=FALSE)
        newCD=t(apply(dTemp,1,function(x){
          test=which(dCorrection$commune_old==as.character(x["commune"]) & 
                       dCorrection$numero_departement_old==as.character(x["numero_departement"]))
          if(length(test)==1){
            c(dCorrection$commune_new[test],dCorrection$numero_departement_new[test])
          }else if(length(test)==0){
            c(x["commune"],x["numero_departement"])
          }else{
            c(x["commune"],x["numero_departement"])
            print(paste0("!!! ERROR !!! Plusieurs corrections possibles pour le couple ",
                         x["commune"]," * ",x["numero_departement"]))
          }
        }))
        dTemp$commune=newCD[,1]
        dTemp$numero_departement=newCD[,2]
      }
      
      dTemp$commune=formatCommune(dTemp,"commune") # homogénéisation des noms de commune
      dTemp$numero_departement=sapply(dTemp$numero_departement,function(x){ # homogénéisation des numéros de département
        if(!is.na(x)){
          if(nchar(x)==1){return(paste0("0",x))}else{return(paste0(x))}
        }else{return(NA)}
      })
      
      if("annee"%in%colnames(dTemp)){ # règle les problèmes de la colonne code-essai : étiquettes inconnues ou fausses (i.e. répétées sur plusieurs essais différents)
        dTemp$code_essai=paste(dTemp$annee,dTemp$commune,sep="-")
      }else{
        dTemp$code_essai=dTemp$commune
      }
      
      data(dTemp)
      insertTab(inputId="tabs",
                tabPanel(title=h4("Cartographie"),value="2",plotOutput("graph")),
                target="1",
                position="after")
    })
    
    output$refreshData <- renderUI({
      return(tipify(actionButton("refresh","",icon("sync-alt")),"Rafraîchir le fichier importé",
                    placement="bottom",trigger="hover"))
    })

    output$fileName <- renderText({
      return(paste0("<font color=\"#5A5AFF\"><i>",currentFilePath(),"</i></font>"))
    })
    
    
    
    ## Affichage des statistiques annuelles ##
    output$annualStats <- renderUI({ # bouton du choix de l'affichage ou non
      if(is.null(data())){
        return(NULL)
      }
      
      return(prettySwitch("showStats","Afficher les statistiques annuelles",fill=TRUE))
    })
    showStats <- reactiveVal(NULL)
    observeEvent(input$showStats,{
      showStats(input$showStats)
    })
    
    output$phenotypeNameStats <- renderUI({ # affiche le champ pour selectionner le Phenotype
      if(is.null(showStats())){
        return(NULL)
      }
      if(!showStats()){
        return(NULL)
      }

      d=data()
      return(selectInput("selectedPhenotypeStats","Fréquence :",
                         colnames(d)[which(colnames(d)%in%usualFrequencies)]))
    })
    selectedPhenotypeStats <- reactiveVal(NULL)
    observeEvent(c(input$selectedPhenotypeStats,input$showStats),{
      selectedPhenotypeStats(input$selectedPhenotypeStats)
    })

    output$yearNameStats <- renderUI({ # affiche le champ pour selectionner l'Annee
      if(is.null(showStats())){
        return(NULL)
      }
      if(!showStats()){
        return(NULL)
      }
      if(is.null(selectedPhenotypeStats())){
        return(NULL)
      }
      
      d=data()
      if(input$pluriY | "annee"%in%colnames(d)){
        return(selectInput("selectedYearStats","Année :",
                           names(table(d$annee[which(!is.na(d[,selectedPhenotypeStats()]))]))))
      }else{
        selectedYearStats(NULL)
        return(NULL)
      }
    })
    selectedYearStats <- reactiveVal(NULL)
    observeEvent(c(input$selectedYearStats,input$showStats),{
      selectedYearStats(input$selectedYearStats)
    })
    
    output$modalityNameStats <- renderUI({ # affiche le champ pour selectionner la Modalité
      if(is.null(showStats())){
        return(NULL)
      }
      if(!showStats()){
        return(NULL)
      }
      if(is.null(selectedPhenotypeStats())){
        return(NULL)
      }
      
      d=data()
      
      if(input$pluriY | "annee"%in%colnames(d)){
        if(is.null(selectedYearStats())){
          return(NULL)
        }
        modalities=table(d$modalite[which(!is.na(d[,selectedPhenotypeStats()]) & 
                                            d$annee==selectedYearStats())])
      }else{
        modalities=table(d$modalite[which(!is.na(d[,selectedPhenotypeStats()]))])
      }
      
      modalities=modalities[rev(order(modalities))]
      return(selectInput("selectedModalityStats","Modalité :",c("All",names(modalities))))
    })
    selectedModalityStats <- reactiveVal(NULL)
    observeEvent(c(input$selectedModalityStats,input$showStats),{
      selectedModalityStats(input$selectedModalityStats)
    })
    
    annualStatsTable <- reactiveVal(NULL)
    output$tableStats <- renderTable({ # Tableau des statistiques annuelles des fréquences : nationales + regionales
      if(is.null(showStats())){
        return(NULL)
      }
      if(!showStats()){
        return(NULL)
      }
      
      return(annualStatsTable())
    })
    observeEvent(c(selectedPhenotypeStats(),
                   selectedYearStats(),
                   selectedModalityStats()),{
                     d=data()
                     
                     if(is.null(selectedPhenotypeStats())){
                       return(NULL)
                     }else{
                       d=d[which(check.numeric(d[,selectedPhenotypeStats()]) &
                                   !is.na(d[,selectedPhenotypeStats()])),]
                       d[,selectedPhenotypeStats()]=as.numeric(d[,selectedPhenotypeStats()])
                       d=d[which(d[,selectedPhenotypeStats()]>=0 &
                                   d[,selectedPhenotypeStats()]<=100 &
                                   d[,selectedPhenotypeStats()]==round(d[,selectedPhenotypeStats()])),]
                     }

                     if(is.null(selectedModalityStats())){
                       return(NULL)
                     }else{
                       if(!selectedModalityStats()=="All"){
                         d=d[which(d$modalite==selectedModalityStats()),]
                       }
                     }

                     if(!is.null(selectedYearStats())){
                       d=d[which(d$annee==selectedYearStats()),]
                     }

                     if(dim(d)[1]==0){ # test si le data contraint par le phénotype+modalité+année n'est pas vide
                       return(NULL)
                       annualStatsTable(NULL)
                     }

                     d$region=sapply(d$numero_departement,function(x){
                       if(!is.na(x)){
                         if(x%in%deptReg$Departement){
                           return(deptReg$Region[which(deptReg$Departement==x)])
                         }else{
                           return(NA)
                         }
                       }else{
                         return(NA)
                       }
                     })

                     out=aggregate(d[,selectedPhenotypeStats()],
                                   list(d$region),
                                   "mean")
                     out=data.frame(cbind(out,as.numeric(table(d$region))))
                     
                     FRA=mean(d[,selectedPhenotypeStats()])
                     colnames(out)=c("Région",
                                     paste(selectedPhenotypeStats(),selectedYearStats(),selectedModalityStats(),sep=" "),
                                     "n")
                     out=data.frame(rbind(c("FRANCE",FRA,nrow(d)),out))
                     out[,2]=as.numeric(out[,2])

                     annualStatsTable(out)
                   })

    
    
    ## Permet à l'utilisateur d'indiquer si le JDD est pluri-annuel : débloque l'onglet Prédiction ##
    output$pluriannual <- renderUI({
      if(is.null(data())){
        return(NULL)
      }
      
      testPluriY() # dependance a testPluriY pour reset le bouton si le fichier importé ne comporte pas de colonne année
      return(prettySwitch("pluriY","Données pluri-annuelles",fill=TRUE))
    })
    
    testPluriY <- reactiveVal(0)
    observeEvent(input$pluriY,{
      if(input$pluriY){
        if(!"annee"%in%colnames(data())){
          sendSweetAlert(session,title="Erreur",
                         text=paste0("Le fichier importé ne possède pas la colonne annee."),
                         type="error")
          testPluriY(testPluriY()+1)
          removeTab(inputId="tabs",target="3")
        }else{
          insertTab(inputId="tabs",
                    tabPanel(title=h4("Prédiction"),value="3",fluidRow(column(12,align="center",uiOutput("tPredict")),
                                                                       plotOutput("graphPred"))),
                    target="2",
                    position="after")
        }
      }else{
        removeTab(inputId="tabs",target="3")
      }
    })
    
    
    
    ## Affiche le contenu du jeu de données importé sous la forme d'un tableau ##
    output$contents <- DT::renderDataTable({
      if(is.null(data())){
        return(NULL)
      }
      
      return(cbind(ligne=c(2:(nrow(data())+1)),data()))
    },options=list(lengthMenu=list(c(10,100,1000,-1),c("10","100","1000","All")),
                   pageLength=100))
    
    
    
    ## Propose à l'utilisateur d'afficher les erreurs pour les différentes colonnes du tableau importé ##
    output$columnChoiceErr <- renderUI({
      if(is.null(data())){
        return(NULL)
      }
      
      d=data()
      return(selectInput("selectedColumnErr",
                         HTML("<p style=\"font-size:20px;font-weight:bold\">Affichage des erreurs :</p>"),
                         colnames(d)[which(colnames(d)%in%mandatoryVar |
                                             colnames(d)%in%accessoryVar |
                                             colnames(d)%in%usualFrequencies)]))
    })
    
    
    
    ## Création des paragraphes contenant les erreurs relevées dans la colonne selectionée par l'utilisateur (en HTML) ##
    output$errors <- renderText({ # affichage du texte des erreurs
      if(is.null(data())){
        return(NULL)
      }
      currentCol=input$selectedColumnErr # colonne choisie par l'utilisateur pour affichage des erreurs
      if(is.null(currentCol)){
        return(NULL)
      }
      if(!currentCol%in%colnames(data())){ # evite les bug lors des re-importation de data(), qui peuvent comporter des colonnes differentes (ex: un JDD pluri-annuel comporte la colonne "annee", qui n'existe pas dans un JDD annuel)
        return(NULL)
      }
      
      d=data()

      # Erreurs liees au code de l'essai #
      if(currentCol=="code_essai"){
        if(length(which(is.na(d$code_essai)))!=0){
          return(paste0("<p>La colonne \"code_essai\" contient ",length(which(is.na(d$code_essai)))," NA (",
                        round((length(which(is.na(d$code_essai)))/nrow(d))*100,2),"%).<br><br></p>
                        <p align=\"justify\">Veuillez ajouter de nouvelles valeurs dans le fichier. 
                        Les nouveaux noms d'essai (chaines de caractères/nombres) doivent êtres différents 
                        de ceux déjà présents les données. L'identification des essais peut être faite en 
                        se basant sur le préleveur/organisme.</p>"))
        }
        else{
          return(paste0("<p>RAS pour la colonne \"code_essai\".</p>"))
        }
        
        
      # Erreurs liees a la commune et a l'affiliation GPS (couples commune * numero de departement inconnus) #
      }else if(currentCol=="commune"){
        obsCD=paste(d$commune, # couples communes / numero de departement du fichier importe
                    d$numero_departement,
                    sep=" * ")
        dtCD=paste(Q$nom_commune, # couples communes / numero de departement du fichier gouvernemental
                   sapply(Q$code_postal,function(x){substr(x,1,2)}),
                   sep=" * ")
        unkwnCD=unique(obsCD[which(!obsCD%in%dtCD)]) # liste des couples manquants

        if(length(unkwnCD)>0){ # il existe des couples "communes * numero de departement" inconnus => pas d'affiliation GPS possible
          unkwnCDLines=lapply(unkwnCD,function(x){which(obsCD==x)}) # lignes du fichier importe correspondant a des couples manquants
          names(unkwnCDLines)=unkwnCD
          unkwnCDMessages=sapply(c(1:length(unkwnCDLines)),function(x){paste0( # liste des couples d'erreurs + lignes correspondantes
            names(unkwnCDLines)[x]," [ligne(s) : ",paste((as.numeric(unkwnCDLines[[x]])+1),collapse=" "),"]"
          )})

          return(paste0("<p>La colonne \"commune\" contient ",length(which(is.na(d$commune)))," NA (",
                        round((length(which(is.na(d$commune)))/nrow(d))*100,2),"%).<br><br></p>
                        <p align=\"justify\">Veuillez corriger à la main dans le fichier
                        ces combinaisons commune * numero de departement inconnues :</p>",
                        paste("<p align=\"justify\">",unkwnCDMessages,"</p>",collapse="",sep="")))
        }else{
          return(paste0("<p>RAS pour la colonne \"commune\".<p>"))
        }
        
        
      # Erreurs liées au numero de département #
      }else if(currentCol=="numero_departement"){
        unkwnDepartement=names(table(d$numero_departement[which(!d$numero_departement%in%deptReg$Departement)]))
        unkwnMessages=sapply(unkwnDepartement,function(x){ # liste des numero de departement inconnus + lignes correspondantes
          paste0(x," [ligne(s) : ",paste((which(d$numero_departement==x)+1),collapse=" "),"]")})
        
        if(length(unkwnMessages)!=0){
          return(paste0("<p>La colonne \"numero_departement\" contient ",length(which(is.na(d$numero_departement)))," NA (",
                        round((length(which(is.na(d$numero_departement)))/nrow(d))*100,2),"%).<br><br></p>
                        <p align=\"justify\">Veuillez corriger à la main dans le fichier ces numeros de département inconnus :</p>",
                        paste("<p align=\"justify\">",unkwnMessages,"</p>",collapse="",sep="")))
        }else if(length(which(is.na(d$numero_departement)))!=0){
          return(paste0("<p>La colonne \"numero_departement\" contient ",length(which(is.na(d$numero_departement)))," NA (",
                        round((length(which(is.na(d$numero_departement)))/nrow(d))*100,2),"%).</p>"))
        }else{
          return(paste0("<p>RAS pour la colonne \"numero_departement\".</p>"))
        }
      }
      
      
      # Erreurs liées aux noms des modalités (différents de usualModalities) #
      else if(currentCol=="modalite"){
        unkwnModality=names(table(d$modalite[which(!d$modalite%in%usualModilities)]))
        unkwnMessages=sapply(unkwnModality,function(x){ # liste des types de modalite inconnues + lignes correspondantes
          paste0(x," [ligne(s) : ",paste((which(d$modalite==x)+1),collapse=" "),"]")})
        
        if(length(unkwnModality)!=0){
          return(paste0("<p>La colonne \"modalite\" contient ",length(which(is.na(d$modalite)))," NA (",
                        round((length(which(is.na(d$modalite)))/nrow(d))*100,2),"%).<br><br></p>
                        <p align=\"justify\">Veuillez corriger/supprimer à la main dans le fichier 
                        ces modalités inconnues :</p>",
                        paste("<p align=\"justify\">",unkwnMessages,"</p>",collapse="",sep="")))
        }else if(length(which(is.na(d$modalite)))!=0){
          return(paste0("<p>La colonne \"modalite\" contient ",length(which(is.na(d$modalite)))," NA (",
                        round((length(which(is.na(d$modalite)))/nrow(d))*100,2),"%).</p>"))
        }else{
          return(paste0("<p>RAS pour la colonne \"modalite\".</p>"))
        }
        
        
      # Erreurs liées au fréquences et/ou aux années (categorie par defaut) #
      }else{
        unkwnValue=names(table(d[,currentCol][which(!check.numeric(d[,currentCol]))])) # Fréquences/Années qui ne sont pas des valeur numériques
        
        memory=d[,currentCol]
        if(length(unkwnValue)>0){ # Transformation des fréquences/années en valeurs numériques
          d[,currentCol][which(!check.numeric(d[,currentCol]))]=NA # Evite de générer un message d'erreur avec le as.numeric() qui suit
        }
        d[,currentCol]=as.numeric(d[,currentCol])
        
        if(currentCol!="annee"){
          unkwnValue=c(unkwnValue, # Fréquences qui sont des valeurs numériques <0 ou >100
                           d[,currentCol][which(d[,currentCol]>100 | d[,currentCol]<0)])
        }
        
        unkwnValue=c(unkwnValue, # Fréquences/Années qui ne sont pas des entiers
                         d[,currentCol][
                           which(d[,currentCol]!=round(d[,currentCol]))
                         ])
        
        unkwnValue=names(table(unkwnValue))
        
        unkwnMessages=sapply(unkwnValue,function(x){ # liste des fréquences/années à erreur avec leurs lignes correspondantes dans le fichier importé
          paste0(x," [ligne(s) : ",paste((which(memory==x)+1),collapse=" "),"]")})
        
        if(length(unkwnValue)!=0){
          return(paste0("<p>La colonne \"",currentCol,"\" contient ",length(which(is.na(d[,currentCol])))," NA (",
                        round((length(which(is.na(d[,currentCol])))/nrow(d))*100,2),"%).<br><br></p>
                        <p align=\"justify\">Veuillez corriger/supprimer à la main dans le fichier 
                        ces fréquences :</p>",
                        paste("<p align=\"justify\">",unkwnMessages,"</p>",collapse="",sep="")))
        }else if(length(which(is.na(d[,currentCol])))!=0){
          return(paste0("<p>La colonne \"",currentCol,"\" contient ",length(which(is.na(d[,currentCol])))," NA (",
                        round((length(which(is.na(d[,currentCol])))/nrow(d))*100,2),"%).</p>"))
        }else{
          return(paste0("<p>RAS pour la colonne \"",currentCol,"\".</p>"))
        }
        return(NULL)
      }
    })
    
    
    
    ## ONGLET 2 : Cartographie ##
    ## Bouton Soumettre, permettant à l'utilisateur de créer une cartographie par krigeage ##
    output$cartoGO <- renderUI({
      return(actionButton("do", "Soumettre",icon("sync-alt")))
    })
    
    
    
    ## Choix de l'utilisateur pour la fréquence cartographiée ##
    output$columnChoicePlot <- renderUI({ # affiche une liste deroulante avec les Phenotypes du fichier importé
      if(is.null(data())){
        return(NULL)
      }
      
      d=data()
      return(selectInput("selectedColumnPlot","Fréquence représentée :",
                         colnames(d)[which(colnames(d)%in%usualFrequencies)]))
    })
    plottedPhenotype <- reactiveVal(NULL)
    observeEvent(c(input$selectedColumnPlot,input$do),{
      plottedPhenotype(input$selectedColumnPlot)
    })
    
    
    
    ## Choix de l'utilisateur pour l'année cartographiée ##
    output$yearName <- renderUI({ # affiche le champ pour selectionner l'Annee
      if(is.null(plottedPhenotype())){
        return(NULL)
      }
      
      d=data()
      if(input$pluriY | "annee"%in%colnames(d)){
        return(selectInput("year","Année représentée :",
                           names(table(d$annee[which(!is.na(d[,plottedPhenotype()]))]))))
      }else{
        return(textInput("year","Année :"))
      }
    })
    plottedYear <- reactiveVal(NULL)
    observeEvent(c(input$year,input$do),{
      plottedYear(input$year)
    })
    
    
    
    ## Choix de l'utilisateur pour la modalité cartographiée TR / TNT / All=TR+TNT ##
    output$modalityChoicePlot <- renderUI({
      if(is.null(plottedPhenotype())){
        return(NULL)
      }
      
      d=data()
      d=d[which(check.numeric(d[,plottedPhenotype()]) &
                  !is.na(d[,plottedPhenotype()])),]
      d[,plottedPhenotype()]=as.numeric(d[,plottedPhenotype()])
      d=d[which(d[,plottedPhenotype()]>=0 &
                  d[,plottedPhenotype()]<=100 &
                  d[,plottedPhenotype()]==round(d[,plottedPhenotype()])),]

      if(input$pluriY | "annee"%in%colnames(d)){
        if(is.null(plottedYear())){
          return(NULL)
        }
        d=d[which(d$annee==plottedYear()),]
      }
      
      modalities=table(d$modalite)
      modalities=modalities[rev(order(modalities))]
      return(selectInput("selectedModalityPlot","Modalité représentée :",
                         paste0(c("All",names(modalities)),
                                " (n=",c(nrow(d),as.numeric(modalities)),")")))
    })
    plottedModality <- reactiveVal(NULL)
    observeEvent(c(input$selectedModalityPlot,input$do),{
      plottedModality(substr(input$selectedModalityPlot,
                             1,
                             as.numeric(gregexpr(" \\(n=",input$selectedModalityPlot))-1))
    })
    
    
    
    ## Création de la carte ##
    plotCarto <- reactiveVal(NULL)
    observeEvent(input$do,{
      d=data()
      
      if(trimws(gsub("\\s+"," ",plottedYear()))==""){ # renvoi une erreur si l'utilisateur n'a pas renseigné d'année
        sendSweetAlert(session,title="Erreur",
                       text=paste0("Veuillez renseigner une année."),
                       type="error")
        return(NULL)
      }

      colfunc=colorRampPalette(c(color0(),color25(),color50(),color75(),color100())) 
      color.range=colfunc(101) # background colors
      colorLegend=c(color.range[12],color.range[37],color.range[63],color.range[88]) # pie colors
      
      d=d[which(!is.na(d[,plottedPhenotype()])),]
      if(input$pluriY | "annee"%in%colnames(d)){
        d=d[which(d$annee==plottedYear()),]
      }
      if(plottedModality()!="All"){
        d=d[which(d$modalite==plottedModality()),]
      }
      

      # Définition des lignes a retirer pour réaliser l'analyse spatiale #
      obsCD=paste(d$commune,d$numero_departement,sep=" * ") # couples Commune/Département du fichier importé
      dtCD=paste(Q$nom_commune,sapply(Q$code_postal,function(x){substr(x,1,2)}),sep=" * ") # couples Commune/Département du fichier gouvernemental
      missingGPS=which(!obsCD%in%dtCD) # lignes du fichier importe correspondant à des couples commune*numero_departement inconnus
      
      unkwnFreq=which(!check.numeric(d[,plottedPhenotype()])) # fréquences qui ne sont pas des valeurs numériques
      tempFreq=d[,plottedPhenotype()]
      if(length(unkwnFreq)>0){ # transformation des fréquences en valeurs numériques
        tempFreq[unkwnFreq]=NA # évite de générer un message d'erreur avec le as.numeric() qui suit
      }
      tempFreq=as.numeric(tempFreq)
      unkwnFreq=c(unkwnFreq, # fréquences qui sont des valeurs numériques entières comprises entre 0 et 100
                 which(tempFreq>100 | tempFreq<0 | tempFreq!=round(tempFreq)))

      rmLines=names(table(c(missingGPS,unkwnFreq,which(is.na(d$code_essai))))) # bilan des lignes à retirer car indisposées a la modélisation spatiale


      # Alerte indiquant le nombre de fréquences retirées pour la cartographie #
      Nremoved=length(rmLines)
      Ntotal=nrow(d)
      if(Nremoved/Ntotal==1){
        sendSweetAlert(session,title="Error",
                       text=paste0("[",Nremoved," - ",round((Nremoved/Ntotal)*100,2),
                                   "%] des fréquences ",plottedPhenotype()," (",plottedModality(),")"," en ",plottedYear(),
                                   " ont dû être retirées pour l'analyse spatiale. Cartographie impossible !"),
                       type="error")
        return(NULL)
      }
      
      sendSweetAlert(session,title="Warning",
                     text=paste0("[",Nremoved," - ",round((Nremoved/Ntotal)*100,2),
                                 "%] des fréquences ",plottedPhenotype()," (",plottedModality(),")"," en ",plottedYear(),
                                 " ont dû être retirées pour l'analyse spatiale. Causes possibles : présence (i) de NA 
                                 dans les colonnes code_essai, commune ou numero_departement, ou (ii) de couples 
                                 commune/numero_departement inconnus (problème d'affiliation GPS), ou (iii) de valeurs 
                                 inconnues dans la colonne numero_departement (problème d'affiliation regionale), ou 
                                 encore (iv) de valeurs non numériques, ou inférieures à 0, ou supérieures à 100 dans la colonne ",
                                 plottedPhenotype(),". Toutes ces erreurs sont détaillées pour chaque colonne dans l'onglet Tableau."),
                     type="warning")
      d=d[-as.numeric(rmLines),]
      
      
      # Debut du processus d'analyse spatiale #
      show_modal_spinner(spin="orbit",text="Analyse en cours",color="#08298A")
      
      
      # Appariement des regions + coordonnees GPS aux donnees importees #
      longitude=rep(NA,nrow(d))
      latitude=rep(NA,nrow(d))
      region=rep(NA,nrow(d))
      for(i in 1:nrow(d)){
        commune=d$commune[i]
        departement=d$numero_departement[i]
        longitude[i]=Q$longitude[which(Q$nom_commune==commune & substr(Q$code_postal,1,2)==departement)]
        latitude[i]=Q$latitude[which(Q$nom_commune==commune & substr(Q$code_postal,1,2)==departement)]
        region[i]=deptReg$Region[which(deptReg$Departement==as.character(departement))]
      }
      d$longitude=longitude
      d$latitude=latitude
      d$region=region
      

      # Run du modele spatial (estimation) #
      d[,plottedPhenotype()]=as.numeric(d[,plottedPhenotype()])
      d$y=cbind(d[,plottedPhenotype()],100-d[,plottedPhenotype()])

      form=as.formula(paste0("y~1+(1|code_essai)+Matern(1|longitude+latitude)")) # formule du modele du modele pour la cartographie
      model=corrHLfit(form,d,family=binomial()) # modele GLMM
      
      predictions=predict(model,binding="fitted") # tableau des frequences predites par le modele spatial
      coordinates=colnames(attr(model,"info.uniqueGeo")[[1]]) # memoire du nom des coordonnees GPS
      

      # Run du krigeage spatial (extrapolation) #
      krig=fillMAP(predictions,coordinates,color.range)
      if(length(which(krig$CX<min(regions$x,na.rm=T) | krig$CX>max(regions$x,na.rm=T)))>0){ # supression des points en dehors des ranges cartographiques, axe X
        krig=krig[-which(krig$CX<min(regions$x,na.rm=T) | krig$CX>max(regions$x,na.rm=T)),]
      }
      if(length(which(krig$CY<min(regions$y,na.rm=T) | krig$CY>max(regions$y,na.rm=T)))>0){ # supression des points en dehors des ranges cartographiques, axe Y
        krig=krig[-which(krig$CY<min(regions$y,na.rm=T) | krig$CY>max(regions$y,na.rm=T)),]
      }
      
      
      # Nouveau data contenant les frequences observees par essai, representees sous forme de camembert (4 categories: 0-25 25-50 50-75 75-100) #
      d2=rep(NA,7)
      names(d2)=c("region","longitude","latitude","F0.to.25","F26.to.50","F51.to.75","F76.to.100")
      for(j in names(table(d$code_essai))){ # regroupement des fréquences observées dans des caemberts par code_essai
        d2=rbind(d2,c(
          d$region[which(d$code_essai==j)][1],
          d$longitude[which(d$code_essai==j)][1],
          d$latitude[which(d$code_essai==j)][1],
          length(which(d[which(d$code_essai==j),input$selectedColumnPlot]<=25)),
          length(which(d[which(d$code_essai==j),input$selectedColumnPlot]>25 & d[which(d$code_essai==j),input$selectedColumnPlot]<=50)),
          length(which(d[which(d$code_essai==j),input$selectedColumnPlot]>50 & d[which(d$code_essai==j),input$selectedColumnPlot]<=75)),
          length(which(d[which(d$code_essai==j),input$selectedColumnPlot]>75 & d[which(d$code_essai==j),input$selectedColumnPlot]<=100))
        ))
      }
      d2=d2[-1,]
      d2=data.frame(d2)
      for(j in 2:ncol(d2)){d2[,j]=as.numeric(as.character(d2[,j]))}

      
      # Definition du ratio X/Y du plot (necessaire pour avoir des camemberts ronds et pas deformes/ovoides)
      rangeX=ext_FR$range[1]-ext_FR$range[2]
      rangeY=ext_FR$range[3]-ext_FR$range[4]
      plotRatio=rangeX/rangeY
      
      
      # Enregistrement des regions monitorees et non monitorees #
      regionsplotNOBSV=regionsplot[which(!regionsplot$Region%in%names(table(d$region))),]
      regionsplot$Region=as.factor(regionsplot$Region)
      regionsplotNOBSV$Region=as.factor(regionsplotNOBSV$Region)
      
      
      # Creation du graphique #
      p=MakeCartoPlot(plottedPhenotype(),plottedYear(),plottedModality(),color.range,colorLegend,plotRatio,ext_FR,ext_FRplot,krig,d2,regionsplot,regionsplotNOBSV)
      
      
      # Fin du processus d'analyse spatiale #
      remove_modal_spinner()
      plotCarto(p)
    })
    
    
    
    ## Affichage de la carte ##
    output$graph <- renderPlot({ # affiche la cartographie
      if(is.null(plotCarto())){ 
        return(NULL)
      }
      
      return(plotCarto()[["toPlot"]])
    },height=750,width=750)
    
    
    
    ## Modification des paramètres graphiques ##
    output$graphicsButton <- renderUI({
      input$resetColors # dependance au bouton reset des couleurs par defaut
      isolate({
        currentColors=list("0"=color0(),"25"=color25(),"50"=color50(),"75"=color75(),"100"=color100())
      })
      
      return(dropdownButton(lapply(seq_along(currentColors),function(x){
        colourInput(paste0("color",names(currentColors)[x]),
                    label=paste0("Couleur seuil ",names(currentColors)[x],"%"),
                    value=currentColors[[x]])}),
        actionButton("resetColors","Couleurs par défaut"),
        circle=TRUE,
        icon=icon("palette"),
        tooltip=tooltipOptions(title="Afficher les réglages de la palette graphique")))
    })
    
    color0 <- reactiveVal(defaultColors[[1]]) # variable memoire stockant la couleur 0% choisi par l'utilisateur
    observeEvent(input$color0,{
      color0(input$color0)})
    
    color25 <- reactiveVal(defaultColors[[2]]) # variable memoire stockant la couleur 25% choisi par l'utilisateur
    observeEvent(input$color25,{
      color25(input$color25)})
    
    color50 <- reactiveVal(defaultColors[[3]]) # variable memoire stockant la couleur 50% choisi par l'utilisateur
    observeEvent(input$color50,{
      color50(input$color50)})
    
    color75 <- reactiveVal(defaultColors[[4]]) # variable memoire stockant la couleur 75% choisi par l'utilisateur
    observeEvent(input$color75,{
      color75(input$color75)})
    
    color100 <- reactiveVal(defaultColors[[5]]) # variable memoire stockant la couleur 100% choisi par l'utilisateur
    observeEvent(input$color100,{
      color100(input$color100)})
    
    observeEvent(input$resetColors,{
      color0(defaultColors[[1]])
      color25(defaultColors[[2]])
      color50(defaultColors[[3]])
      color75(defaultColors[[4]])
      color100(defaultColors[[5]])
    })
    
    
    
    ## Enregistrement de la carte au format PDF ##
    output$cartoSave <- renderUI({ # affiche le bouton d'exportation du plotCarto()
      if(is.null(plotCarto())){
        return(NULL)
      }
      
      return(actionButton("exportCartography","Exporter la carte au format PDF",icon("download")))
    })
    
    observeEvent(input$exportCartography,{ # permet d'enregistrer le plotCarto() lors du clic-bouton
      savePath=choose.dir()
      
      if(is.na(savePath)){
        return(NULL)
      }else{
        pdf(paste0(savePath,"\\",plottedPhenotype(),"_",plottedYear(),"_",plottedModality(),
                   "_(",format(Sys.time(),"%d-%m-%Y"),").pdf"))
        print(plotCarto()[["toSave"]])
        dev.off()
      }
    })
    
    
    
    ## Enregistrement de la légende de couleur de la carte au format PDF ##
    # Affichage de la légende dans l'application #
    output$legendSave <- renderUI({
      return(actionButton("exportLegend","Exporter la légende au format PDF",icon("download")))
    })
    
    output$legendPic <- renderImage({
      outfile=tempfile(fileext=".png")
      
      colfunc=colorRampPalette(c(color0(),color25(),color50(),color75(),color100())) 
      color.range=colfunc(101) # background colors
      
      p=ggplot()
      p=p+scale_x_continuous(limits=c(0,length(color.range)+1),expand=c(0,0)) # axe Frequence
      p=p+scale_y_continuous(limits=c(0,100),expand=c(0,0)) # axe unitaire
      p=p+geom_point(aes(x=c(1:length(color.range)),y=1),shape=15,size=20,color=color.range)
      p=p+theme(panel.background=element_rect(fill="#F5F5F5"), # arriere-plan gris (1)
                panel.grid.major=element_line(colour="#F5F5F5"), # arriere-plan gris (2)
                panel.grid.minor=element_line(colour="#F5F5F5"), # arriere-plan gris (3)
                plot.background=element_rect(fill="#F5F5F5"), # arriere-plan gris (4)
                legend.position="none", # pas de legende
                plot.margin=margin(c(0,0,0,0),unit="cm"),  # pas de marges (respect du ratio X/Y France)
                axis.line.x=element_line(colour="#F5F5F5",size=2), # pas de ligne représentant l'axe des x
                axis.title.x=element_blank(), # pas de titre a l'axe des x
                axis.ticks.x=element_blank(), # pas de graduation sur l axe des x
                axis.text.x=element_blank(), # pas d'annotation sur l'axe des x
                axis.line.y=element_line(colour="#F5F5F5",size=2), # pareil pour l'axe y ....
                axis.title.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.text.y=element_blank())
      png(outfile,width=session$clientData$output_legendPic_width,height=38)
      print(p)
      dev.off()
      
      list(src=outfile,
           contentType="image/png",
           width=session$clientData$output_legendPic_width,
           height=38)
    },deleteFile=TRUE)
    
    # Enregistrement de la légende #
    observeEvent(input$exportLegend,{
      savePath=choose.dir()
      
      if(is.na(savePath)){
        return(NULL)
      }else{
        colfunc=colorRampPalette(c(color0(),color25(),color50(),color75(),color100())) 
        color.range=colfunc(101) # background colors
        
        pdf(paste0(savePath,"\\Legende_0=",substr(color0(),2,nchar(color0())),"_",
                   "25=",substr(color25(),2,nchar(color25())),"_",
                   "50=",substr(color50(),2,nchar(color50())),"_",
                   "75=",substr(color75(),2,nchar(color75())),"_",
                   "100=",substr(color100(),2,nchar(color100())),"_",
                   "(",format(Sys.time(),"%d-%m-%Y"),").pdf"))
        p=ggplot()
        p=p+scale_x_continuous(limits=c(0,length(color.range)+1),expand=c(0,0),name="Frequency (%)") # axe Frequence
        p=p+scale_y_continuous(limits=c(0,100),expand=c(0,0)) # axe unitaire
        p=p+geom_point(aes(x=c(1:length(color.range)),y=1),shape=15,size=10,color=color.range)
        p=p+theme(panel.background=element_blank(), # arriere-plan blanc
                  legend.position="none", # pas de legende
                  plot.margin=margin(c(0,1,7,1),unit="cm"),  # pas de marges (respect du ratio X/Y France)
                  axis.line.x=element_blank(), # pas de ligne représentant l'axe des x
                  axis.title.x=element_text(size=25), # pas de titre a l'axe des x
                  axis.ticks.x=element_line(size=1.5,colour="black"), # pas de graduation sur l axe des x
                  axis.text.x=element_text(size=20), # pas d'annotation sur l'axe des x
                  axis.line.y=element_blank(), # pareil pour l'axe y ....
                  axis.title.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.text.y=element_blank())
        print(p)
        dev.off()
      }
    })
    
    
    
    
    ## ONGLET 3 : Prédiction ##
    ## Sélection des données par l'utilisateur pour l'analyse prédictive régionale ##
    # Liste déroulante des phénotypes disponibles à la prédiction #
    output$columnChoicePlotPred <- renderUI({
      if(is.null(data())){
        return(NULL)
      }
      
      d=data()
      return(selectInput("selectedColumnPlotPred","Fréquence prédite :",
                         colnames(d)[which(colnames(d)%in%usualFrequencies)]))
    })
    selectedColumnPlotPred <- reactiveVal(NULL)
    observeEvent(c(input$selectedColumnPlotPred,input$doPred),{
      selectedColumnPlotPred(input$selectedColumnPlotPred)
    })
    
    # Slider permettant à l'utilisateur de sélectionner les années d'intérêt pour le phénotype choisi #
    output$sliderY <- renderUI({
      if(is.null(selectedColumnPlotPred())){
        return(NULL)
      }
      d=data()

      d=d[which(check.numeric(d[,selectedColumnPlotPred()]) &
                  !is.na(d[,selectedColumnPlotPred()])),]
      d[,selectedColumnPlotPred()]=as.numeric(d[,selectedColumnPlotPred()])
      d=d[which(d[,selectedColumnPlotPred()]>=0 &
                  d[,selectedColumnPlotPred()]<=100 &
                  d[,selectedColumnPlotPred()]==round(d[,selectedColumnPlotPred()])),]

      yearsBefore=table(d$annee)
      dTest=d[-which(d[,selectedColumnPlotPred()]==0 |
                       d[,selectedColumnPlotPred()]==100),]
      yearsAfter=table(dTest$annee)
      
      result=unlist(sapply(c(1:length(yearsBefore)),function(x){
        round((yearsAfter[which(names(yearsAfter)==names(yearsBefore)[x])]/yearsBefore[x])*100)
      }))
      result[which(result<10)]=NA    
      
      totalYears=names(result)
      
      if(length(which(!is.na(result)))>1){
        advicedYears=names(na.contiguous(result))
        return(sliderInput("timeRange","Plage temporelle d'apprentissage :",
                           min=min(as.numeric(totalYears)),max=max(as.numeric(totalYears)),sep="",
                           value=c(min(as.numeric(advicedYears)),max(as.numeric(advicedYears))),
                           ticks=FALSE))
      }else{
        timeRange(NULL)
        return(paste0("Impossible de trouver une plage temporelle adaptée à la modélisation."))
      }
    })
    timeRange <- reactiveVal(NULL)
    observeEvent(c(input$timeRange,input$doPred),{
      timeRange(input$timeRange)
    })
    
    
    
    ## Slider permettant à l'utilisateur de choisir l'indice de confiance souhaité ##
    output$sliderConfidenceIndex <- renderUI({
      return(sliderInput(inputId="confidenceIndex",
                         label="Indice de confiance souhaité :",
                         min=1,
                         max=10,
                         value=1))
    })
    confidenceIndex <- reactiveVal(NULL)
    observeEvent(c(input$confidenceIndex,input$doPred),{
      confidenceIndex(input$confidenceIndex)
    })    
    
    
    
    ## Bouton permettant de lancer l'analyse dynamique des fréquences ##
    output$predGO <- renderUI({ # affiche le bouton Soumettre pour la creation de la cartographie
      return(actionButton("doPred","Soumettre",icon("sync-alt")))
    })
    
    
    
    ## Rendu du tableau des estimations (summary) du modèle dynamique ##
    estimateSummary <- reactiveVal(NULL)
    output$resultSummary <- renderTable({
      return(estimateSummary())
    },digits=3)
    
    
    
    ## Affichage du graphique des prédictions ##
    # Slider permettant à l'utilisateur de choisir la prédiction à t+X #
    output$tPredict <- renderUI({
      if(is.null(plotPredList())){
        return(NULL)
      }
      
      return(sliderInput(inputId="timeToPredict",
                         label="Afficher la prédiction à T+... :",
                         min=1,
                         max=5,
                         value=1))
    })
    
    # Choix des paramètres et rendu du plot de prédiction #
    plotPredList <- reactiveVal(NULL)
    plotPred <- reactiveVal(NULL)
    output$graphPred <- renderPlot({
      return(plotPred())
    },height=750,width=750)
    
    observeEvent(c(input$timeToPredict,input$doPred),{
      if(is.null(plotPredList())){
        return(NULL)
      }
      if(is.null(input$timeToPredict)){
        return(NULL)
      }
      
      plotPred(plotPredList()[[input$timeToPredict]])
    })
    
    
    
    ## Estimation du modèle dynamique GLM et calculs des cartographies de prédiction ##
    observeEvent(input$doPred,{
      d=data()

      # Sélection des données pour la modélisation #
      d=d[which(check.numeric(d[,selectedColumnPlotPred()]) &
                  !is.na(d[,selectedColumnPlotPred()])),]
      d[,selectedColumnPlotPred()]=as.numeric(d[,selectedColumnPlotPred()])
      d=d[which(d[,selectedColumnPlotPred()]>=0 &
                  d[,selectedColumnPlotPred()]<=100 &
                  d[,selectedColumnPlotPred()]==round(d[,selectedColumnPlotPred()])),]

      obsCD=paste(d$commune,d$numero_departement,sep=" * ") # couples communes * numéro de département du fichier importé
      dtCD=paste(Q$nom_commune,sapply(Q$code_postal,function(x){substr(x,1,2)}),sep=" * ") # couples communes * numéro de département du fichier gouvernemental
      missingGPS=which(!obsCD%in%dtCD) # lignes du fichier importé correspondant a des couples commune*numero_departement inconnus
      
      rmLines=names(table(c(missingGPS,which(d$modalite!="TNT" | # lignes a retirer car indisposees a la modelisation
                                               !d$annee%in%c(timeRange()[1]:timeRange()[2])))))
      d=d[-as.numeric(rmLines),]

      # Appariement des régions #
      region=rep(NA,nrow(d))
      for(i in 1:nrow(d)){
        region[i]=deptReg$Region[which(deptReg$Departement==as.character(d$numero_departement[i]))]
      }
      d$region=region

      # Création du data à analyser #
      d$t=d$annee-timeRange()[1]
      
      d[which(d[,selectedColumnPlotPred()]==0),selectedColumnPlotPred()]=1 # !!!!!! A CHANGER ? : retirer les 0% et 100% du JDD et estimer leurs proportions annuelle
      d[which(d[,selectedColumnPlotPred()]==100),selectedColumnPlotPred()]=99
      
      d$y=cbind(d[,selectedColumnPlotPred()],
                100-d[,selectedColumnPlotPred()])
      
      d=d[,which(colnames(d)%in%c("t","region","y"))]
      
      # !!!!!!!! AVEC SUBSTANCES ACTIVES PAS TOUTES LES REGIONS (ex: LIMOUSIN)
      # !!!!!!!! VERIFIER QUE TOUTES LES ANNEES-1 SONT BIEN DANS LE PANEL
      tXregion=table(d$t,d$region)
      meanOcc=round(colMeans(tXregion))
      tXregion[which(tXregion!=0)]=1
      timeOcc=colSums(tXregion)
      keptReg=names(table(d$region))[which(
        timeOcc>1 &
        meanOcc>=confidenceIndex()
      )]
      d=d[which(d$region%in%keptReg),]
      
      d$region=as.factor(d$region)
      basicLevels=names(table(d$region))

      # Test si suffisament de régions différentes pour faire une prédiction régionalisée #
      if(length(basicLevels)>1){
        changedLevels=basicLevels[c(length(basicLevels),1:(length(basicLevels)-1))]
      }else{
        sendSweetAlert(session,title="Warning",
                       text=paste0("Pas assez de points de monitoring pour réaliser 
                                   une prédiction régionalisée avec un indice de confiance de ",
                                   input$confidenceIndex,"."),
                       type="error")
        plotPredList(NULL)
        plotPred(NULL)
        estimateSummary(NULL)
        return(NULL)
      }

      # Analyse 1 (levels de d$region=basicLevels) #
      d1=d
      contrasts(d1$region)="contr.sum"
      model1=glm(y~region*t,
                 data=d1,
                 family=quasibinomial("logit"))
      estimates1=summary(model1)$coefficients[-1,c(1,2,4)]
      estimates1=estimates1[grep("t",rownames(estimates1)),]
      RN1=rownames(estimates1)
      RN1=sub(":t","",RN1)
      RN1=basicLevels[as.numeric(str_extract(RN1,"[0-9]+"))]
      RN1[which(is.na(RN1))]="FRANCE"
      rownames(estimates1)=RN1
      
      # Analyse 2 (levels de d$region=changedLevels) #
      d2=d
      d2$region=factor(d2$region,
                       levels=changedLevels)
      contrasts(d2$region)="contr.sum"
      model2=glm(y~region*t,
                 data=d2,
                 family=quasibinomial("logit"))
      estimates2=summary(model2)$coefficients[-1,c(1,2,4)]
      estimates2=estimates2[grep("t",rownames(estimates2)),]
      RN2=rownames(estimates2)
      RN2=sub(":t","",RN2)
      RN2=changedLevels[as.numeric(str_extract(RN2,"[0-9]+"))]
      RN2[which(is.na(RN2))]="FRANCE"
      rownames(estimates2)=RN2

      # Fusion des summary de l'analyse 1 et 2 #
      estimates=data.frame(rbind(round(estimates1,3),round(estimates2,3)))
      colnames(estimates)=c("Estimation","Ecart-type","P-value")
      estimates=estimates[!grepl(".1",rownames(estimates)),]

      # Calcul des seuils de significativité #
      estimates$"Signif."=sapply(estimates$"P-value",function(x){
        if(x<=0.001){return("***")}
        else if(x<=0.025){return("**")}
        else if(x<=0.05){return("*")}
        else if(x<=0.1){return(".")}
        else{return("")}
      })
      estimates=data.frame(cbind("Paramètre"=rownames(estimates),estimates))
      estimateSummary(estimates) # stockage du data contenant le summary
      
      # Calcul des fréquences régionales prédites par le modèle GLM #
      predTable=expand.grid(t=c(c(1:5)+max(as.numeric(names(table(d$t))))),
                  region=names(table(d$region)))
      predTable=data.frame(predTable,Prediction=(predict.glm(model1,predTable,"response")*100))
      predTable$Prediction=round(predTable$Prediction)

      # Construction de la liste des graphiques de prédiction (de t+1 à t+5) #
      colfunc=colorRampPalette(c(color0(),color25(),color50(),color75(),color100())) 
      color.range=colfunc(101) # background colors
      
      rangeX=ext_FR$range[1]-ext_FR$range[2]
      rangeY=ext_FR$range[3]-ext_FR$range[4]
      plotRatio=rangeX/rangeY
      
      regionsplotPred=regionsplot[which(regionsplot$Region%in%names(table(predTable$region))),]
      
      pList=MakePredictionPlots(selectedColumnPlotPred(),
                                c(timeRange()[1]:timeRange()[2]),
                                color.range,
                                plotRatio,
                                ext_FR,
                                regionsplot,
                                regionsplotPred,
                                predTable,
                                confidenceIndex())

      plotPredList(pList) # stockage de la liste de plot des fréquences prédites
      
      # !!!!! (NULL DEVIANCE - RESIDUAL DEVIANCE)/NULL DEVIANCE ou R²
    })
    
    
    ## Choix de l'utilisateur pour enregistrer les cartes de prédiction au format PDF ##
    output$cartoPredSave <- renderUI({
      if(is.null(plotPredList())){
        return(NULL)
      }
      
      return(actionButton("exportPrediction","Exporter les prédictions au format PDF",icon("download")))
    })
    
    observeEvent(input$exportPrediction,{
      savePath=choose.dir()
      if(is.na(savePath)){
        return(NULL)
      }else{
        pdf(paste0(savePath,"\\","Predictions_",selectedColumnPlotPred(),"_",
                   (timeRange()[2]+1),"-",(timeRange()[2]+length(plotPredList())),
                   "_(",format(Sys.time(),"%d-%m-%Y"),").pdf"))
        for(i in 1:length(plotPredList())){
          plot(plotPredList()[[i]])
        }
        dev.off()
      }
    })
    
    
    
    ## Importation de données explicatives pour le modèle de prédiction ##
    shinyFileChoose(input,"file2",roots=c(wd='.'),filetypes=c("csv")) # bouton d'importation des fichiers CSV
    file2 <- reactiveVal(NULL)
    observeEvent(input$file2,{
      file2(input$file2)
    })
    
    currentFilePath2 <- reactiveVal(NULL) # variable stockant le chemin du fichier importé
    data2 <- reactiveVal(NULL)
    observeEvent(input$file2,{
      inFile=parseFilePaths(roots=c(wd='.'),file2())
      if(length(inFile$datapath)==0){
        return(NULL)
      }
      
      currentFilePath2(paste0(getwd(),substr(as.character(inFile$datapath),2,nchar(as.character(inFile$datapath))))) # enregistrement du chemin pour affichage
      dTemp=read.table(currentFilePath2(),header=TRUE,sep=";",
                       stringsAsFactors=FALSE,na.strings=c("","-"))
      
      data2(dTemp)
    })

    output$showExplanatoryData <- renderTable({
      if(is.null(data2())){
        return(NULL)
      }
      return(head(data2()))
    })
    
    output$deleteExplanatoryData <- renderUI({
      if(is.null(data2())){
        return(NULL)
      }
      return(tipify(actionButton("deleteExpData","",icon("trash-alt")),"Supprimer les variables explicatives",
                    placement="bottom",trigger="hover"))
    })
    observeEvent(input$deleteExpData,{
      file2(NULL)
      currentFilePath2(NULL)
      data2(NULL)
    })
    
    output$fileName2 <- renderText({
      return(paste0("<font color=\"#5A5AFF\"><i>",currentFilePath2(),"</i></font>"))
    })
    
    
    
    ## Arret de l'application ##
    # Lorsque l'utilisateur quitte l'application (ferme l'onglet dans le navigateur WEB), l'application s'arrête automatiquement #
    session$onSessionEnded(function(){
      stopApp()
    })
    
    
    
  }
  
  
  
  
  ### Run de l'application ###
  # Ligne de commande qui permet de lancer l'application dans le navigateur web par défaut #
  runApp(list(ui=ui,server=server),launch.browser=TRUE)
}