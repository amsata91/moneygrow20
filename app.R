#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(tidyr)
library(dplyr)
library(sf)
library(ggplot2)
library(DT)
library(readr)
library(cartography)
library(shiny)
library(shinydashboard)
load("got_data.RData")
load("got.RData")
load("amsa.RData")
load("abdou.RData")
scenesLocations=st_read("./data/GoTRelease/ScenesLocations.shp",crs=4326)
character_name_list = appearences %>% pull(name) %>% unique()
ui = dashboardPage(
    dashboardHeader(title = "STAT-GOT"),
    dashboardSidebar(
        hr(),
        sidebarMenu(id="tabs",
                    menuItem("INTRODUCTION", tabName = "Intro", icon = icon("dashboard")),
                    menuItem("DATAFRAME", tabName = "dataframe", icon = icon("table"),
                             menuSubItem("Données non spatiales", tabName = "nonspat",
                                         icon=icon("angle-right")),
                             menuSubItem("Données spatiales", tabName = "spat", icon
                                         =icon("angle-right"))),
                    menuItem("PERSONNAGES",tabName="perso",icon=icon("fas fa-user"),
                             menuSubItem("Etude par personnage", tabName="parperso",
                                         icon=icon("angle-right")),
                             menuSubItem("Etude sur l'ensemble des personnages",
                                         tabName="enperso",icon=icon("angle-right"))),
                    menuItem("AMATEURS",tabName="etud",icon=icon("fas fa-user")),
                    menuItem("AUTEURS",tabName="aut",icon=icon("fas fa-user")))
    ),
    
    
    dashboardBody(
        tabItems(
            tabItem(tabName="Intro",
                    tabsetPanel(
                        tabPanel("INTRODUCTION",
                                 plotOutput("got"),
                                 p(""),
                                 p(""),
                                 p(""),
                                 p("Shiny est un package R qui facilite la construction d'applications web 
                                 interactives depuis R. Les utilisateurs peuvent simplement manipuler une
                                 application cliques- boutons pour exécuter et afficher des résultats fournis
                                 par du code R. "),
                                 p("Grâce à ce package nous avons développé une appplicaton
                                   nommée STAT-GOT qui nous  permet de faire une étude 
                                   statistique sur la serie Game Of Thrones (GOT)."),
                                 p("Egalement appelée Le Trône de fer selon le titre français de l'œuvre romanesque 
                                 dont elle est adaptée, GOT est une série télévisée américaine 
                                   créée par David Benioff et D. B. Weiss, diffusée entre le 17 avril 2011 et le 19 mai 2019."),
                                 
                                p("Grâce au menu DATAFRAME, vous pouvez télécharger l'ensemble des données,
                                  avoir une visualisation et une description  des données.
                                  Le menu PERSONNAGES vous permet d'avoir les informations sur chaque acteur de la serie. 
                                  Une étude par épisode et par saison est possible dans la section AMATEURS. ")
                    
                
            ))),
            
            tabItem(tabName ="nonspat",
                    selectInput("dataset", "Choisir un dataset:",
                                choices = c("appearences","characters", "episodes","scenes")),
                    numericInput("obs", "Nombre d'observation :", 10),
                    tags$h5("actionButton:"),
                    actionButton("update", "Action button", class = "btn-primary"),
                    p("Click sur le bouton pour visualiser les données"),
                    tags$h5("Téléchargement des données"),
                    downloadButton("downloadData", "Download"),
                    tabsetPanel(
                        tabPanel("Dataset",
                                 h4("Observation"),
                                 tableOutput("view")),
                        tabPanel("Dataset View",
                                 h4("Observation View"),
                                 DT::dataTableOutput('view1')),
                        tabPanel("Summary",
                                 h4("Summary"),
                                 verbatimTextOutput("summary")),
                        tabPanel("str",
                                 h4("str"),
                                 verbatimTextOutput("str")),
                        tabPanel("Etude du dataset Scenes",
                                 h4("Le nombre de personnages morts de la serie"),
                                 verbatimTextOutput("nbth"),
                                 h4("Le nombre de personnages morts lors de la première saison"),
                                 verbatimTextOutput("nbsaison"),
                                 h4("La durée de la scène la plus longue et l’id de l’episode "),
                                 verbatimTextOutput("dur")),
                        tabPanel("Visualisation morts cum",
                                 h4("visualisation des morts cumulés"),
                                 plotOutput("cum"))
                        
                    )),
            tabItem(tabName = "spat",
                    selectInput("datasetsp", "Choisir un dataset:",
                                choices = c("locations","lakes", "conts","land",
                                            "wall","islands","kingdoms",
                                            "landscapes","roads","rivers")),
                    actionButton("update1", "Action button", class = "btn-primary"),
                    p("Click sur le bouton pour visualiser les données"),
                    tags$h5("Téléchargement des données"),
                    downloadButton("downloadData1", "Download"),
                    tabsetPanel(
                        tabPanel("Dataset View",
                                 h4("Observation "),
                                 DT::dataTableOutput("view2"))
                    ),
                    
                    ),
            tabItem(tabName = "parperso",
                    selectInput("charactername", "Acteurs:",character_name_list),
                    tabsetPanel(
                        tabPanel("Visibilité",
                                 h4("L’épisode ou le personnage selectionné à passé plus de temps aux écrans "),
                                 verbatimTextOutput("txtout")
                        ),
                        tabPanel("Visualisation de temps de présence  ",
                                 h4(""),
                                 plotOutput("maplot")),
                        tabPanel("Geolocalisation ",
                                 h4(""),
                                 plotOutput("mapplot"))
                    )
                    
            ),
            tabItem(tabName = "enperso",
                    tabsetPanel(
                        tabPanel("Clustering hierarchique",
                                 h4("  "),
                                 plotOutput("clust")
                        ),
                        tabPanel("Temps d'apparition par personnage et par saison  ",
                                 h4(""),
                                 plotOutput("fff"))
                    )),
            
            tabItem(tabName="etud",
                    selectInput("seasonnum", "Choisir le numéro de la saison" ,choices=c(1:8) ),
                    selectInput("episodnum","Choisir le numéro de l'épisode de la saison",choices=c(1:10)),
                    tabsetPanel(
                        tabPanel("Etude",
                            tableOutput("dat")))),
            tabItem(tabName="aut",
                    tabsetPanel(
                        tabPanel("AMSATA NDIAYE",
                                 h4("Amsatata NDIAYE,  étudiant en master 2 en Ingénierie Mathématique
                                    et Biostatistique à l'université de Paris."),
                                 plotOutput("im1")),
                        tabPanel("ANTOINE KODJI",
                                 h4("Antoine KODJI, étudiant en master 2 en Ingénierie Mathématique
                                    et Biostatistique à l'université de Paris."),
                                 plotOutput("im2")),
                        tabPanel("ABDOURAHMAN ABDILLAHI TOUKALEH",
                                 h4("ABDOURAHMAN ABDILLAHI , étudiant en master 2 en Ingénierie Mathématique
                                    et Biostatistique à l'université de Paris."),
                                 plotOutput("im3")),
                        tabPanel("JATSA NGUETSE BLANDINE",
                                 h4("ATSA NGUETSE BLANDINE, étudiant en master 2 en Ingénierie Mathématique
                                    et Biostatistique à l'université de Paris."),
                                 plotOutput("im4"))
                        
                        ))
            )))
server = function(input, output) {
    datasetspInput = eventReactive(input$update1, {
        switch(input$datasetsp,
               "locations"=locations,
               "lakes"=lakes,
               "conts"=conts,
               "land"=land,
               "wall"=wall,
               "islands"=islands,
               "kingdoms"=kingdoms,
               "landscapes" = landscapes,
               "roads"=roads,
               "rivers"=rivers)
    }, ignoreNULL = FALSE)
    
    
    datasetInput = eventReactive(input$update, {
        switch(input$dataset,
               "appearences"=appearences,
               "characters"=characters,
               "episodes"=episodes,
               "scenes"=scenes)
    }, ignoreNULL = FALSE)
    
    
    output$view2 = DT::renderDataTable(
        DT::datatable(datasetspInput(), options = list(pageLength = 25))
    )
    output$view = renderTable({
        head(datasetInput(), n = isolate(input$obs))
    })
    output$view1 = DT::renderDataTable(
        DT::datatable(datasetInput(), options = list(pageLength = 25))
    )
    output$summary = renderPrint({
        dataset = datasetInput()
        summary(dataset)
    })
    output$view = renderTable({
        head(datasetInput(), n = isolate(input$obs))
    })
    output$str = renderPrint({
        dataset = datasetInput()
        str(dataset)
    })
    output$nbth = renderPrint({
        dataset = datasetInput()
        if(dataset==scenes){
            sum(dataset$nbdeath)
        }
    })
    output$nbsaison = renderPrint({
        dataset = datasetInput()
        if(dataset==scenes){
            sum(dataset$nbdeath[dataset$episodeId<=10])
        }
    })
    output$dur = renderPrint({
        dataset = datasetInput()
        if(dataset==scenes){
            dataset[which.max(dataset$duration),]
            
        }
    })
    output$cum = renderPlot({
        # nombre de morts cumulé et temps passé
        deaths = scenes %>% select(nbdeath,duration,location,episodeId) %>%
            mutate(t=cumsum(duration),tdeath=cumsum(nbdeath))
        # instant de changement de saison
        # ? lag
        season_t = episodes %>% mutate(ld=lag(total_duration)) %>%
            mutate(ld=if_else(is.na(ld),0,ld), td = cumsum(ld)) %>% 
            filter(episodeNum==1) %>% pull(td)# nombre de morts cumulé et temps passé
        # geom_line + labels personalisés
        ggplot(deaths) + geom_line(aes(x=t/3600,y=tdeath)) +
            scale_x_continuous("",expand = c(0,0),breaks = season_t/3600,
                               labels =   paste("Saison",1:8),)+
            scale_y_continuous("Nombre de morts cumulés", expand=c(0,0))+
            theme_bw()+theme(axis.text.x=element_text(angle=90))+
            ggtitle("Evolution du nombre de mort au cours du temps")
    })
    # Downloadable csv of selected dataset ----
    output$downloadData = downloadHandler(
        filename = function() {
            paste(input$dataset, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
        }
    )
    output$downloadData1 = downloadHandler(
        filename = function() {
            paste(input$datasetsp, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetspInput(), file, row.names = FALSE)
        }
    )
    
    
    
    output$txtout = renderPrint({
        appearances %>%filter(name==input$charactername) %>% 
            left_join(scenes) %>% left_join(episodes) %>%
            group_by(name,episodeId,episodeTitle) %>% 
            summarise(screenTime=sum(duration)) %>% 
            arrange(desc(screenTime)) %>% head(1)
    })
    output$maplot = renderPlot({
        
        jstime = appearances %>% filter(name==input$charactername) %>% 
            left_join(scenes) %>% 
            group_by(episodeId) %>% 
            summarise(time=sum(duration))
        ggplot(jstime) + 
            geom_line(aes(x=episodeId,y=time))+
            theme_bw()+
            xlab("épisode")+ylab("temps")+
            ggtitle("Temps de présence par épisode ")
    })
    
    
    
    
    
    
    
    output$mapplot = renderPlot({
        
        
        landpol = st_union(st_geometry(land))
        islandpol = st_union(st_geometry(islands))
        backpol=st_union(landpol,islandpol)
        background = st_as_sf(backpol)
        
        loc_time= appearances %>% filter(name %in% input$chartername) %>%
            left_join(scenes) %>%
            group_by(location,input$chartername) %>%
            summarize(duration=sum(duration,na.rm=TRUE))
        loc_time_mc = scenesLocations %>% left_join(loc_time)
        
        
        ggplot()+geom_sf(data=background,color="ivory3",fill="ivory")+
            geom_sf(data=loc_time_mc%>% filter(!is.na(duration)),aes(size=duration))+
            geom_sf_text(data=loc_time_mc,aes(label=location),color="#000000",vjust="bottom",
                         family="CM Roman", fontface="italic")+
            geom_sf(data=rivers,col=colriver)+
            geom_sf(data=lakes,col=colriver,fill=colriver)+
            geom_sf(data=wall,col="black",size=1)+
           
            scale_color_discrete(guide="none")+
            scale_size_area("Duree (seconde) :",max_size =
                               8,breaks=c(30,60,120,240,480,500,600))+
            theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
           # theme(panel.background = element_rect(fill = "#87cdde",color=NA),
                # text = element_text(family="CM Roman",face = "bold",size = 12))+
                 # legend.key = element_rect(fill="#ffffff")+
            labs(title = "Temps de presence des personnage ",
                 caption = "",x="",y="")
        
    })
    
    output$clust = renderPlot({
        duration_location_character = scenes %>% left_join(appearances) %>% 
            group_by(name,location) %>% 
            summarize(duration=sum(duration))
        
        duration_large = duration_location_character %>% 
            pivot_wider(values_from = duration,names_from = location,values_fill = c("duration"=0))
        X=as.matrix(duration_large[,-1])
        Xs=X[rowSums(X)>60*60,]
        Xns=Xs/rowSums(Xs)
        rownames(Xns)=duration_large$name[rowSums(X)>60*60]
        hc=hclust(dist(Xns,method="manhattan"))
        plot(hc,main = "Clustering des personnages principaux suivant leur lieux de présences",
             sub ="STAT-GOT, 2020",xlab = "")
        
    })
    
    
    output$fff = renderPlot({
        screenTimePerSeasons = appearances %>% left_join(scenes) %>% 
            left_join(episodes) %>% 
            group_by(name,seasonNum) %>% 
            summarise(screenTime=sum(duration)) %>% 
            arrange(desc(screenTime)) 
        screenTimeTotal = screenTimePerSeasons %>% 
            group_by(name) %>% 
            summarise(screenTimeTotal=sum(screenTime))
        mainCharacters = screenTimeTotal %>% 
            filter(screenTimeTotal>60*60) %>% 
            arrange(screenTimeTotal) %>% 
            mutate(nameF=factor(name,levels = name))
        data = screenTimePerSeasons %>% left_join(mainCharacters) %>% filter(!is.na(nameF))
        ggplot(data)+
            geom_bar(aes(y=nameF,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
            scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
            geom_text(data=mainCharacters,aes(y=nameF,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
            scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
            ylab("")+ggtitle("Temps d'apparition cumulé par personnage et saison")
    })
    
    output$dat = renderTable({
        
        #partiefonction
        
        data=appearances%>% left_join(scenes)%>% 
            left_join(episodes)%>%
            filter(episodeNum==input$episodnum,seasonNum==input$seasonnum)
        
        #identifiant de l'episode choisie
        
        id=data$episodeId[which((data$episodeNum==input$episodnum)&(data$seasonNum==input$seasonnum ))][1]
        
        
        #titre de l'episode
        tiltle=data$episodeTitle[which((data$episodeNum==input$episodnum)&(data$seasonNum==input$seasonnum ))][1]
        
        
        # duree totale de l'episode
        time_tot= data$total_duration[which((data$episodeNum==input$episodnum)&(data$seasonNum==input$seasonnum ))][1]
        #jointure des tableaux
        
        #nombres de morts dans l'épisode
        nb_mort=sum(data[,"nbdeath"])
        
        #nombre totale de scenes
        nb_scene= data[,"sceneId"]%>% unique()%>% length()
        #noms de tous les acteurs
        noms_acteurs=data[,"name"]%>% unique()
        
        
        #le nombre d'acteurs 
        nb_acteurs=length(noms_acteurs)
        
        #Acteurs le plus visible
        plus_visible= (data%>%group_by(name)%>%
                           summarise(duree=sum(duration))%>%
                           arrange(desc(duree)) %>% head(1))[,"name"]
        
        #scene la plus longue
        scene_longue=(data%>%group_by(sceneId)%>%
                          summarise(duree=sum(duration))%>%
                          arrange(desc(duree)) %>% head(1))[,"sceneId"]
        ##lieu le plus visite
        lieu_plus_visite=(data%>%group_by(location)%>%
                              summarise(duree=sum(duration))%>%
                              arrange(desc(duree)) %>% head(1))[,"location"]
        
        
        
        #####Résultats à la sortie#####
        
        val=data.frame(id,tiltle,time_tot,nb_mort,scene_longue,plus_visible,lieu_plus_visite)
        names(val)=c("Identite de l'episode","Titre de l'episode","Durée totale (s) ",
                     "Nombre de morts","ID de la scene la plus longue",
                     "Acteurs le plus visible","lieu le plus visite") 
        head(val)
    })      
    
    output$im1 = renderImage({
       return(list(
           src = "amsata.jpeg",
           contentType = "image/jpeg",
           width = 200,
           height = 200,
          alt = "This is alternate text"
    ))
    }, deleteFile = FALSE)
    
    
    output$im3 = renderImage({
        return(list(
            src = "abdou.jpeg",
            contentType = "image/jpeg",
            width = 200,
            height = 200,
            alt = "This is alternate text"
        ))
    }, deleteFile = FALSE)
    
    
    output$im4 = renderImage({
        return(list(
            src = "photo.png",
            contentType = "image/png",
            width = 200,
            height = 200,
            alt = "This is alternate text"
        ))
    }, deleteFile = FALSE)
    output$im2 = renderImage({
        return(list(
            src = "antoine.jpeg",
            contentType = "image/jpeg",
            width = 200,
            height = 200,
            alt = "This is alternate text"
        ))
    }, deleteFile = FALSE)
    output$got = renderImage({
        return(list(
            src = "got.jpeg",
            contentType = "image/jpeg",
            width = 600,
            height = 400,
            alt = "This is alternate text"
        ))
    }, deleteFile = FALSE)
}

shinyApp(ui, server)
