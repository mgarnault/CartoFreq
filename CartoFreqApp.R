### VIDANGE DE LA MEMOIRE ###
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
# install.packages("glmmTMB")
library("glmmTMB")





### VARIABLES GLOBALES ###
## Set de variable obligatoires et accessoires pour déterminer les colonnes qui seront des fréquences de résistance (i.e. les autres colonnes) ##
mandatoryVar=c("code_essai","commune","numero_departement","modalite")
accessoryVar=c("annee","latitude","longitude") # AJOUTER UNE POSSIBILITE D'AJOUTER DIRECTEMENT LES COORDONNEES GPS ?


## Noms des modalités acceptées par l'application et l'analyse spatiale ##
usualModilities=c("TR","TNT") # AJOUTER LA POSSIBILITE D'AVOIR D'AUTRES MODALITES STANDARD "T1/2/3/4/X" ?


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
fillMAP=function(predict,coords,color.range){ # fonction de krigeage spatial
  map.formula=as.formula(paste(attr(predict,"fittedName"),"~1+Matern(1|",paste(coords,collapse="+"),")"))
  
  smoothObject=fitme(map.formula,data=predict,method="REML") # extrapotation des frequences par krigeage
  smoo=predict(smoothObject,binding="dummy")
  x=smoo[,coords[1]]
  y=smoo[,coords[2]]
  margin=1/2 # marge prise pour l'extrapolation des frequences a partir du carre minimum (delimite par les localisation extremes des points de prelevement) : max = +Inf ; min = 0
  xrange=range(x)
  margex=(xrange[2]-xrange[1])*margin
  xrange=xrange+margex*c(-1,1)
  yrange=range(y)
  margey=(yrange[2]-yrange[1])*margin
  yrange=yrange+margey*c(-1,1)
  plot.axes=quote({axis(1);axis(2)})
  gridSteps=400 # resolution de la grille d'extrapolation
  xGrid=seq(xrange[1],xrange[2],length.out=gridSteps)
  yGrid=seq(yrange[1],yrange[2],length.out=gridSteps)
  
  newdata=expand.grid(xGrid,yGrid)
  colnames(newdata)=coords
  gridpred=predict(smoothObject,newdata=newdata)
  
  matrixplot=cbind(newdata,gridpred[,1]) # tableau contenant les frequences extrapolees par krigeage
  colnames(matrixplot)=c("CX","CY","Pred")
  matrixplot[,"Pred"]=round(100*matrixplot[,"Pred"])
  if(length(which(matrixplot[,"Pred"]>100))>0){matrixplot[which(matrixplot[,"Pred"]>100),"Pred"]=100}
  if(length(which(matrixplot[,"Pred"]<0))>0){matrixplot[which(matrixplot[,"Pred"]<0),"Pred"]=0}
  
  matrixplot=cbind(matrixplot,Freq=matrixplot[,"Pred"])
  matrixplot[,"Pred"]=color.range[(matrixplot[,"Pred"]+1)] # association des frequences extrapolees a une couleur
  return(as.data.frame(matrixplot))
}


## Fonction qui permet de dessiner les graphiques ##
MakePlot=function(phenotype,annee,modalite,color.range,colorLegend,plotRatio,ext_FR,ext_FRplot,krig,d2,regionsplot,regionsplotNOBSV){ 
  p=ggplot() # creation du plot par phenotype et par annee
  
  p=p+scale_x_continuous(limits=c(ext_FR$range[1]/plotRatio,ext_FR$range[2]/plotRatio),expand=c(0,0)) # dimension de l'axe x
  p=p+scale_y_continuous(limits=c(ext_FR$range[3],ext_FR$range[4]),expand=c(0,0)) # dimension de l'axe y
  p=p+coord_equal()
  
  p=p+geom_point(aes(x=CX/plotRatio,y=CY),data=krig,color=krig$Pred,pch=15,size=1) # arriere-plan couleur indiquant les frequences extrapolees par krigeage
  
  print(d2)
  print(summary(d2))
  print(dim(d2))
  print(which(colnames(d2)%in%c("F0.to.25","F26.to.50","F51.to.75","F76.to.100")))
  print(d2[which(colnames(d2)%in%c("F0.to.25","F26.to.50","F51.to.75","F76.to.100"))])
  print(apply(d2[which(colnames(d2)%in%c("F0.to.25","F26.to.50","F51.to.75","F76.to.100"))],
              2,
              function(x){!all(x==0)}))
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
  
  p1=p+geom_polygon(aes(x=X/plotRatio,y=Y),data=ext_FRplot,fill="white",color="white",size=0) # delimination de la France
  p1=p1+geom_polygon(aes(x=X/plotRatio,y=Y,group=Region),data=regionsplot,fill="NA",color="black",size=1) # delimination des regions
  p1=p1+geom_polygon(aes(x=X/plotRatio,y=Y,group=Region),data=regionsplotNOBSV,fill="grey50",color="black",size=1) # grisage des regions non observees a l'annee du graphique
  p1=p1+geom_point(aes(x=longitude/plotRatio,y=latitude),data=d2,color="black",pch=20,size=10) # arriere-plan des points de couleur des frequences observees
  p1=p1+geom_scatterpie(aes(x=longitude/plotRatio,y=latitude,r=0.08),data=d2,cols=c("F0.to.25","F26.to.50","F51.to.75","F76.to.100"),color=NA)
  p1=p1+geom_text(aes(x=as.numeric(quantile(ext_FR$x,.25,na.rm=T))/plotRatio,y=as.numeric(quantile(ext_FR$y,.975,na.rm=T))),label=paste0(phenotype," (",annee," - ",modalite,")"),size=10) # titre annee
  
  p2=p+geom_polygon(aes(x=X/plotRatio,y=Y),data=ext_FRplot,fill="white",color="white",size=0) # delimination de la France
  p2=p2+geom_polygon(aes(x=X/plotRatio,y=Y,group=Region),data=regionsplot,fill="NA",color="black",size=.75) # delimination des regions
  p2=p2+geom_polygon(aes(x=X/plotRatio,y=Y,group=Region),data=regionsplotNOBSV,fill="grey50",color="black",size=.75) # grisage des regions non observees a l'annee du graphique
  p2=p2+geom_point(aes(x=longitude/plotRatio,y=latitude),data=d2,color="black",pch=20,size=6) # arriere-plan des points de couleur des frequences observees
  p2=p2+geom_scatterpie(aes(x=longitude/plotRatio,y=latitude,r=0.08),data=d2,cols=c("F0.to.25","F26.to.50","F51.to.75","F76.to.100"),color=NA)
  p2=p2+geom_text(aes(x=as.numeric(quantile(ext_FR$x,.25,na.rm=T))/plotRatio,y=as.numeric(quantile(ext_FR$y,.975,na.rm=T))),label=paste0(phenotype," (",annee," - ",modalite,")"),size=6) # titre annee
  
  return(list(toPlot=p1,toSave=p2))
}






### IMPORTATION DES FICHIERS OBLIGATOIRES ###
## Ficher des coordonnees GPS a partir de la commune ##
Q=read.table(paste0(getwd(),"/data/communesGPS.csv"),header=TRUE,sep=";",
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
  
  
  
  
  ### Affichage ###
  ui <- fluidPage(useShinyjs(),
                  useSweetAlert(),
                  position="left",
                  div(HTML("<h1>Outil cartographique <em>spFr</em></h1>")), # Titre
                  sidebarLayout(
                    
                    ## Panel proposant l'importation + corrections ##
                    sidebarPanel(
                      conditionalPanel(condition="input.tabs==1",
                                       div(HTML("<p style=\"font-size:20px;font-weight:bold\">Importation des fichiers :</p>")),
                                       fluidRow(column(4,shinyFilesButton(id="file1",label="Importer un CSV",title="",multiple=FALSE,icon=icon("file-upload"))),
                                                column(1,uiOutput("refreshData"))),
                                       h4(htmlOutput("fileName")),
                                       uiOutput("annualStats"),
                                       uiOutput("pluriannual"),
                                       uiOutput("yearNameStats"),
                                       uiOutput("phenotypeNameStats"),
                                       uiOutput("tableStats"),
                                       hr(),
                                       uiOutput("columnChoiceErr"),
                                       htmlOutput("errors")),
                      conditionalPanel(condition="input.tabs==2",
                                       uiOutput("yearName"),
                                       uiOutput("columnChoicePlot"),
                                       uiOutput("modalityChoicePlot"),
                                       fluidRow(column(10,imageOutput("legendPic",height="30px")),
                                                column(2,uiOutput("graphicsButton"))),
                                       uiOutput("legendSave"),
                                       hr(),
                                       uiOutput("cartoGO"),
                                       uiOutput("cartoSave")
                      ),
                      conditionalPanel(condition="input.tabs==3",
                                       uiOutput("sliderY"),
                                       uiOutput("columnChoicePlotPred"),
                                       hr(),
                                       uiOutput("predGO"))),
                    
                    ## Panels affichant le data ou les cartes ##
                    mainPanel(
                      tabsetPanel(id="tabs",
                                  tabPanel(title=h4("Tableau"),value="1",tableOutput("contents"))))))
  
  
  
  
  ### Fonctions ###
  server <- function(input,output,session){
    
    
    
    ## Importation du tableau de donnees ##
    shinyFileChoose(input,"file1",roots=c(wd='.'),filetypes=c("csv")) # bouton d'importation des fichiers CSV
    
    currentFilePath <- reactiveVal("") # variable stockant le chemin du fichier importe
    
    data <- reactive({ # creation d'une variable dynamique pour le data importe par l'utilisateur
      inFile=parseFilePaths(roots=c(wd='.'),input$file1)
      if(length(inFile$datapath)==0){
        return(NULL)
      }
      input$refresh # dependance au bouton refresh
      
      currentFilePath(paste0(getwd(),substr(as.character(inFile$datapath),2,nchar(as.character(inFile$datapath))))) # enregistrement du chemin
      dTemp=read.table(currentFilePath(),header=TRUE,sep=";",
                       stringsAsFactors=FALSE,na.strings=c("","-"))
      if(!all(mandatoryVar%in%colnames(dTemp))){ # test de l'existence de toutes les variables obligatoires dans le fichier importe par l'utilisateur
        sendSweetAlert(session,title="Erreur",
                       text=paste0("Toutes les colonnes obligatoires (",paste(mandatoryVar,collapse=", "),") ne sont pas présentes dans le fichier."),
                       type="error")
        return(NULL)
      }
      
      return(dTemp)
    })
    
    observeEvent(input$file1,{
      if(is.null(data())){
        removeTab(inputId="tabs",target="2")
        removeTab(inputId="tabs",target="3")
      }else{
        insertTab(inputId="tabs",
                  tabPanel(title=h4("Cartographie"),value="2",plotOutput("graph")),
                  target="1",
                  position="after")
      }
    })
    
    output$refreshData <- renderUI({
      return(actionButton("refresh","Rafraîchir",icon("sync-alt")))
    })
    
    output$fileName <- renderText({
      if(is.null(data())){
        return(NULL)
      }
      return(paste0("<font color=\"#5A5AFF\"><i>",currentFilePath(),"</i></font>"))
    })
    
    
    
    ## Affichage des statistiques annuelles ##
    output$annualStats <- renderUI({ # bouton du choix de l'affichage ou non
      if(is.null(data())){
        return(NULL)
      }
      
      return(prettySwitch("showStats","Afficher les statistiques annuelles",fill=TRUE))
    })
    
    output$yearNameStats <- renderUI({ # affiche le champ pour selectionner l'Annee
      if(is.null(input$showStats)){
        return(NULL)
      }
      if(!input$showStats){
        return(NULL)
      }
      if(is.null(data())){
        return(NULL)
      }
      
      d=data()
      if(!"annee"%in%colnames(d)){
        return(NULL)
      }else{
        return(selectInput("selectedYearStats","Année :",names(table(d$annee))))
      }
    })
    
    output$phenotypeNameStats <- renderUI({ # affiche le champ pour selectionner le Phenotype
      if(is.null(input$showStats)){
        return(NULL)
      }
      if(!input$showStats){
        return(NULL)
      }
      if(is.null(data())){
        return(NULL)
      }
      
      d=data()
      return(selectInput("selectedPhenotypeStats","Fréquence :",colnames(d)[
        which(!colnames(d)%in%c(mandatoryVar,accessoryVar))]))
    })
    
    output$tableStats <- renderTable({ # Statistiques annuelles des frequences : nationales + regionales
      print(paste(input$selectedPhenotypeStats,input$selectedYearStats))
      print(paste("showStats:",input$showStats))
      
      if(is.null(input$showStats)){
        return(NULL)
      }
      if(!input$showStats){
        return(NULL)
      }
      if(is.null(data())){
        return(NULL)
      }
      if(is.null(input$selectedPhenotypeStats)){
        return(NULL)
      }
      
      d=data()
      d$numero_departement=sapply(d$numero_departement,function(x){ # homogeneisation des numeros de departement
        if(!is.na(x)){
          if(nchar(x)==1){return(paste0("0",x))}else{return(paste0(x))}
        }else{return(NA)}
      })
      
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
      
      d=d[which(check.numeric(d[,input$selectedPhenotypeStats]) &
                  !is.na(d[,input$selectedPhenotypeStats])),]
      
      if(input$pluriY | "annee"%in%colnames(d)){
        d=d[which(d$annee==input$selectedYearStats),]
      }
      
      out=aggregate(d[,input$selectedPhenotypeStats],
                    list(d$region),
                    "mean")
      out=data.frame(cbind(out,as.numeric(table(d$region))))
      
      FRA=mean(d[,input$selectedPhenotypeStats])
      colnames(out)=c("Région",input$selectedPhenotypeStats,"n")
      out=data.frame(rbind(c("FRANCE",FRA,nrow(d)),out))
      out[,input$selectedPhenotypeStats]=as.numeric(out[,input$selectedPhenotypeStats])
      
      return(out)
    })
    
    
    
    ## Permet à l'utilisateur d'indiquer si le JDD est pluri-annuel ##
    testPluriY <- reactiveVal(0)
    
    output$pluriannual <- renderUI({
      if(is.null(data())){
        return(NULL)
      }
      
      testPluriY() # dependance a testPluriY pour reset le bouton si le fichier importe ne comporte pas de colonne annee
      return(prettySwitch("pluriY","Données pluri-annuelles",fill=TRUE))
    })
    
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
                    tabPanel(title=h4("Prediction"),value="3",plotOutput("graphPred")),
                    target="2",
                    position="after")
        }
      }else{
        removeTab(inputId="tabs",target="3")
      }
    })
    
    
    
    ## Montre le jeu de donnees importe sous la forme d'un tableau ##
    output$contents <- renderTable({ # affiche le tableau des donnees brutes
      if(is.null(data())){
        return(NULL)
      }
      return(cbind(ligne=c(2:(nrow(data())+1)),data()))
    })
    
    
    
    ## Choix d'une colonne afin d'afficher les erreurs ##
    output$columnChoiceErr <- renderUI({ # affiche le menu deroulant pour que l'utilisateur choisse la colonne pour laquelle les erreurs s'afficheront
      if(is.null(data())){
        return(NULL)
      }
      return(selectInput("selectedColumnErr",
                         HTML("<p style=\"font-size:20px;font-weight:bold\">Affichage des erreurs :</p>"),
                         colnames(data())))
    })
    
    
    
    ## Creation des paragraphes contenant les erreurs trouvees dans la colonne selectionee (return en HTML) ##
    output$errors <- renderText({ # affiche le texte compilant toutes les erreurs trouvees dans la colonne choisie par l'utilisateur
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
      d$commune=formatCommune(d,"commune") # homogeneisation du nom des communes
      d$numero_departement=sapply(d$numero_departement,function(x){ # homogeneisation des numeros de departement
        if(!is.na(x)){
          if(nchar(x)==1){return(paste0("0",x))}else{return(paste0(x))}
        }else{return(NA)}
      })
      
      
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
          unkwnCDLines=sapply(unkwnCD,function(x){which(obsCD==x)}) # lignes du fichier importe correspondant a des couples manquants
          unkwnCDMessages=sapply(c(1:length(unkwnCDLines)),function(x){paste0( # liste des couples d'erreurs + lignes correspondantes
            names(unkwnCDLines)[x]," [ligne(s) : ",paste((as.numeric(unlist(unkwnCDLines[x]))+1),collapse=" "),"]"
          )})
          
          return(paste0("<p>La colonne \"commune\" contient ",length(which(is.na(d$commune)))," NA (",
                        round((length(which(is.na(d$commune)))/nrow(d))*100,2),"%).<br><br></p>
                        <p align=\"justify\">Veuillez corriger à la main dans le fichier
                        ces combinaisons commune * numero de departement inconnues :</p>",
                        paste("<p align=\"justify\">",unkwnCDMessages,"</p>",collapse="",sep="")))
        }else{
          return(paste0("<p>RAS pour la colonne \"commune\".<p>"))
        }
        
        
        # Erreurs liees au numero de departement #
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
      
      
      # Erreurs liees aux noms des modalites (TR/TNT) #
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
        
        
        # Erreurs liees au frequences de resistance (categorie par defaut) #
      }else{
        unkwnFrequency=names(table(d[,currentCol][which(!check.numeric(d[,currentCol]))]))
        unkwnMessages=sapply(unkwnFrequency,function(x){ # liste des frequences non numeriques + lignes correspondantes
          paste0(x," [ligne(s) : ",paste((which(d[,currentCol]==x)+1),collapse=" "),"]")})
        
        if(length(unkwnFrequency)!=0){
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
    
    
    
    ## Bouton permettant la creation d'une carte ##
    output$cartoGO <- renderUI({ # affiche le bouton Soumettre pour la creation de la cartographie
      return(actionButton("do", "Soumettre",icon("sync-alt")))
    })
    
    
    
    ## Choix d'une colonne de frequence pour creer la carte ##
    output$columnChoicePlot <- renderUI({ # affiche une liste deroulante avec les Phenotypes du fichier importe (colonnes autres que mandatoryVar & accessoryVar)
      return(selectInput("selectedColumnPlot","Fréquence :",colnames(data())[
        which(!colnames(data())%in%c(mandatoryVar,accessoryVar))]))
    })
    
    
    
    ## Annee renseignee par l'utilisateur pour le titre de la carte ##
    output$yearName <- renderUI({ # affiche le champ pour selectionner l'Annee
      if(is.null(input$pluriY)){
        return(NULL)
      }
      if(is.null(data())){
        return(NULL)
      }
      
      d=data()
      if(input$pluriY  | "annee"%in%colnames(d)){
        return(selectInput("year","Année :",names(table(d$annee))))
      }else{
        return(textInput("year","Année :"))
      }
    })
    
    
    
    ## Choix de l'utilisateur pour enregistrer la carte au format PDF ##
    output$cartoSave <- renderUI({ # affiche le bouton d'exportation du plot()
      if(is.null(plot())){
        return(NULL)
      }
      
      return(actionButton("exportCartography","Exporter la carte au format PDF",icon("download")))
    })
    
    plottedPhenotype <- reactiveVal("") # variable memoire stockant le nom du Phenotype cartographie
    
    plottedYear <- reactiveVal("") # variable memoire stockant le nom de l'Annee cartographiee
    
    plottedModality <- reactiveVal("") # variable memoire stockant le nom de la Modalite cartographiee
    
    observeEvent(input$exportCartography,{ # permet d'enregistrer le plot() lors du clic-bouton
      savePath=choose.dir()
      if(is.na(savePath)){
        return(NULL)
      }else{
        pdf(paste0(savePath,"\\",plottedPhenotype(),"_",plottedYear(),"_",plottedModality(),
                   "_(",format(Sys.time(),"%d-%m-%Y"),").pdf"))
        print(plot()[["toSave"]])
        dev.off()
      }
    })
    
    
    
    ## Choix de l'utilisateur pour enregistrer la legende de la carte au format PDF ##
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
    
    observeEvent(input$exportLegend,{ # permet d'enregistrer la legende du plot() lors du clic-bouton
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
    
    
    
    ## Choix de l'utilisateur pour faire la cartographie des modalites TR / TNT / All=TR+TNT
    output$modalityChoicePlot <- renderUI({
      if(is.null(input$selectedColumnPlot)){
        return(NULL)
      }
      if(input$selectedColumnPlot==""){
        return(NULL)
      }
      
      d=data()
      d=d[which(!is.na(d[,input$selectedColumnPlot]) &
                  d$modalite%in%usualModilities),]
      if(input$pluriY | "annee"%in%colnames(d)){
        d=d[which(d$annee==input$year),]
      }
      modalities=table(d$modalite)
      
      return(selectInput("selectedModalityPlot","Modalité :",
                         paste0(c(names(modalities),"All")," (n=",c(as.numeric(modalities),nrow(d)),")")))
    })
    
    
    
    ## Creation de la carte ##
    plot <- reactive({ # objet contenant le graphique de l'analyse spatiale (glmm + krigeage)
      if(is.null(input$do)){ # evite de creer un graphique avant d'avoir clique sur le bouton Soumettre + creer la dependance a input$do pour le refresh de plot()
        return(NULL)
      }
      if(input$do==0){ # evite de creer un graphique avant d'avoir clique sur le bouton Soumettre + creer la dependance a input$do pour le refresh de plot()
        return(NULL)
      }
      
      isolate({ # evite de recalculer la cartographie lorsque l'utilisateur modifie simplement les valeurs de Annee et Phenotype
        d=data()
        
        if(trimws(gsub("\\s+"," ",input$year))==""){ # renvoie une erreur si l'utilisateur n'a pas renseigne d'Annee
          sendSweetAlert(session,title="Erreur",
                         text=paste0("Veuillez renseigner une année."),
                         type="error")
          return(NULL)
        }
        
        plottedPhenotype(paste0(input$selectedColumnPlot)) # enregistrement du Phenotype cartographie
        
        plottedYear(paste0(input$year)) # enregistrement de l'Annee cartographiee
        
        chosenModality=substr(input$selectedModalityPlot,1,as.numeric(gregexpr(" ",input$selectedModalityPlot))-1) # Modalite choisie par l'utilisateur pour la cartographie
        plottedModality(paste0(chosenModality)) # enregistrement de la Modalite cartographiee
        
        colfunc=colorRampPalette(c(color0(),color25(),color50(),color75(),color100())) 
        color.range=colfunc(101) # background colors
        colorLegend=c(color.range[12],color.range[37],color.range[63],color.range[88]) # pie colors
        
        
        # Definition des lignes a retirer pour l'analyse spatiale #
        d$commune=formatCommune(d,"commune") # homogeneisation du noms des communes
        d$numero_departement=sapply(d$numero_departement,function(x){ # homogeneisation des numeros de departement
          if(!is.na(x)){
            if(nchar(x)==1){return(paste0("0",x))}else{return(paste0(x))}
          }else{return(NA)}
        })
        
        obsCD=paste(d$commune, # couples communes / numero de departement du fichier importe
                    d$numero_departement,
                    sep=" * ")
        dtCD=paste(Q$nom_commune, # couples communes / numero de departement du fichier gouvernemental
                   sapply(Q$code_postal,function(x){substr(x,1,2)}),
                   sep=" * ")
        missingGPS=which(!obsCD%in%dtCD) # lignes du fichier importe correspondant a des couples commune*numero_departement inconnus
        
        rmLines=names(table(c(missingGPS, # lignes a retirer car indisposees a la modelisation spatiale
                              which(is.na(d$code_essai) | 
                                      is.na(d$modalite) | 
                                      is.na(d[,input$selectedColumnPlot]) | 
                                      !d$modalite%in%usualModilities | 
                                      !check.numeric(d[,input$selectedColumnPlot]) | 
                                      !d$numero_departement%in%deptReg$Departement))))
        
        if(input$pluriY | "annee"%in%colnames(d)){
          rmLines=names(table(c(rmLines,
                                which(d$annee!=input$year))))
          Nremoved=length(rmLines[which(rmLines%in%which(!is.na(d[,input$selectedColumnPlot]) & d$annee==input$year))])
          Ntotal=length(which(!is.na(d[,input$selectedColumnPlot]) & d$annee==input$year))
        }else{
          Nremoved=length(rmLines[which(rmLines%in%which(!is.na(d[,input$selectedColumnPlot])))])
          Ntotal=length(which(!is.na(d[,input$selectedColumnPlot])))
        }
        
        
        # Alerte indiquant le nombre de donnees de frequence retirees pour l'analyse #
        sendSweetAlert(session,title="Warning",
                       text=paste0("[",Nremoved," - ",round((Nremoved/Ntotal)*100,2),
                                   "%] des données importées (hors NA) de ",input$selectedColumnPlot," en ",input$year," ont dû être retirées pour 
                              l'analyse spatiale. Causes possibles : présence (i) de NA dans les colonnes code_essai, 
                              modalite, commune ou numero_departement, ou (ii) de couples commune/numero_departement 
                              inconnus (affiliation GPS), ou (iii) de valeurs dans numero_departement inconnus (affiliation regionale),
                              ou (iv) d'autres valeurs que TR et TNT dans la colonne modalite, ou encore (v) de valeurs non numériques 
                              dans la colonne ",input$selectedColumnPlot,". Toutes ces erreurs sont détaillées par colonne dans 
                              l'onglet Tableau."),
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
        d$y=cbind(as.numeric(d[,input$selectedColumnPlot]),100-as.numeric(d[,input$selectedColumnPlot]))
        
        form=as.formula(
          paste0("y~1+(1|code_essai)+Matern(1|longitude+latitude)")) # formule du modele du modele pour la cartographie
        if(chosenModality!="All"){
          d=d[which(d$modalite==chosenModality),]
        }
        model=corrHLfit(form,d,family=binomial()) # modele GLMM
        predictions=predict(model,binding="fitted") # tableau des frequences predites par le modele spatial
        # predictions=predictions[!duplicated(predictions),]
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
        for(j in names(table(d$code_essai))){
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
        p=MakePlot(input$selectedColumnPlot,input$year,chosenModality,color.range,colorLegend,plotRatio,ext_FR,ext_FRplot,krig,d2,regionsplot,regionsplotNOBSV)
        
        
        # Fin du processus d'analyse spatiale #
        remove_modal_spinner()
        return(p)
      })
    })
    
    
    
    ## Affichage de la carte ##
    output$graph <- renderPlot({ # affiche la cartographie
      if(is.null(plot())){ 
        return(NULL)
      }
      
      return(plot()[["toPlot"]])
    }, height=750, width=750)
    
    
    
    ## Modification des parametres graphiques ##
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
    
    
    
    ## Affichage du graphique des predictions ##
    output$graphPred <- renderPlot({
      print(input$timeRange)
      
      return(ggplot())
    })
    
    plotPred <- reactive({ # objet contenant le graphique de l'extrapolation temporelle (glmmTMB)
      if(is.null(input$doPred)){ # evite de creer un graphique avant d'avoir clique sur le bouton Soumettre + creer la dependance a input$do pour le refresh de plot()
        return(NULL)
      }
      if(input$doPred==0){ # evite de creer un graphique avant d'avoir clique sur le bouton Soumettre + creer la dependance a input$do pour le refresh de plot()
        return(NULL)
      }
      
      isolate({ # evite de recalculer la cartographie lorsque l'utilisateur modifie simplement les valeurs de sliderAnnee et Phenotype
        d=data()
      })
    })
    
    
    
    ## Selection des donnees par l'tilisation pour l'analyse predictive regionale ##
    output$sliderY <- renderUI({
      if(is.null(input$pluriY)){
        return(NULL)
      }
      
      d=data()
      firstY=min(as.numeric(names(table(d$annee))))
      lastY=max(as.numeric(names(table(d$annee))))
      return(sliderInput("timeRange","Années :",
                         min=firstY,max=lastY,sep="",
                         value=c(firstY,lastY)))
    })
    
    output$columnChoicePlotPred <- renderUI({ # affiche une liste deroulante avec les Phenotypes du fichier importe (colonnes autres que mandatoryVar & accessoryVar)
      return(selectInput("selectedColumnPlotPred","Fréquence :",colnames(data())[
        which(!colnames(data())%in%c(mandatoryVar,accessoryVar))]))
    })
    
    output$predGO <- renderUI({ # affiche le bouton Soumettre pour la creation de la cartographie
      return(actionButton("doPred","Soumettre",icon("sync-alt")))
    })
    
    
    ## Arret de l'application ##
    session$onSessionEnded(function(){ # lorsque l'utilisateur quitte l'onglet l'application se termine
      stopApp()
    })
    
    
    
  }
  
  
  
  
  ### Run de l'application ###
  runApp(list(ui=ui,server=server),launch.browser=TRUE) # lance l'application dans le navigateur par defaut
}