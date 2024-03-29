

##############################################
#                  Module                    #
##############################################

ui_6 <- function(id){
  ns <- NS(id)
  tagList(
    ######### the "Détails statistiques" toggle ######## 
    tags$head(
      tags$script(HTML("
      $(document).ready(function() {
        $('#toggleMethodButton_6').click(function() {
          $('#methodText_6').toggle();
        });
      });
    "))
    ),
    ######### display ######## 
    column(3,wellPanel(
      selectInput(ns("sensor1"),
                  label = "Choix du premier capteur",
                  choices = NULL),
      selectInput(ns("direction1"), label = "Direction du capteur",
                  choices = c("Toute"=" ","B vers A"="_rgt","A vers B" ="_lft")),
      selectInput(ns("sensor2"),
                  label = "Choix du second capteur",
                  choices = NULL),
      selectInput(ns("direction2"), label = "Direction du capteur",
                  choices = c("Toute"=" ","B vers A"="_rgt","A vers B" ="_lft")),
      selectInput(ns("heure"), label = "Choix de l'heure",
                  choices = c("6h-7h"=7,"7h-8h"=8,"8h-9h"=9,"9h-10h"=10,"10h-11h"=11,"11h-12h"=12,
                              "12h-13h"=13,"13h-14h"=14,"14h-15h"=15,"15h-16h"=16,"16h-17h"=17,
                              "17h-18h"=18,"18h-19h"=19,"19h-20h"=20,"20h-21h"=21),
                  selected = 9),
      checkboxGroupInput(
        ns("mobility"),
        "Choix du type de mobilité",
        selected = c("car","heavy"),
        choiceNames = c("VL","PL","Piéton","Vélo"),
        choiceValues = c("car","heavy","pedestrian","bike")
      ),
      radioButtons(inputId = ns("norm"), label = "Normaliser :",
                   choices = c("Oui"="YES","Non"="NO"),selected = "NO",inline = TRUE)
    )),
    
          
    h3(" Comparaison de période :"),
    p("Cet onglet permet de comparer le comportement des usagers de 2 capteurs différents. Pour
               cela 3 graphiques sont proposés, avec chacun une courbe par capteur :"),
    p("- Un graphique de la tendance, qui correspond à l’évolution liée à la période de l’année"),
    p("- Un graphique montrant l’effet de chaque jour de la semaine."),
    p("- Un graphique représentant les variations propre à chaque jour (ce qui reste après avoir enlevé
                 l’effet de la tendance et du cycle hebdomadaire ).
                 Le dernier graphique est accompagné de 2 outils pour faciliter la mesure du lien entre les courbes :
                 un concernant la corrélation et un autre la synchronicité de celles-ci."),
    br(),
    p("Vous avez le choix entre 2 options : visualiser les courbes brutes (non normalisées) pour faire
                 une analyse quantitative ou les visualiser en les ramenant à la même échelle (normalisées) 
                 pour une analyse qualitative."),
    

    actionButton("toggleMethodButton_6", "Détails statistiques", style = "display: block; margin: 0 auto;"),
    div(id = "methodText_6", style = "display: none;",
      h4("Séparation en tendance, cycle et bruit :"),
      p("Après un filtrage des données selon les choix de l’utilisateur, on détermine la période d’activité commune des deux capteurs sélectionnées. 
          Pour trouver la tendance (évolution générale du flux),  pour chaque capteur, on réalise une moyenne glissante sur 14 jours (2 périodes hebdomadaires), c’est le graphique du premier onglet.
          On soustrait la tendance au flux total pour avoir de données sans tendance. Pour ces données, on fait une moyenne sur tous les lundis, puis les mardis, etc. Cela nous donne les valeurs associées au cycle de la semaine (second onglet). La partie restante après la soustraction du cycle hebdomadaire correspond au bruit statistique (troisième onglet)."),
      br(),
      h4("Indicateurs du lien entre les bruits :"),
      br(),
      p("Le premier indicateur est le coefficient de corrélation de Pearson. On a fait choix de seuils pour afficher différents commentaires :"),
      p("1. Pour un coefficient plus grand que 0.5 on considère que les courbes sont corrélées."),
      p("2. Pour un coefficient entre 0.2 et 0.5 on considère que la corrélation est légère."),
      p("3. Pour un coefficient inférieur à 0.2 on considère que les courbes sont non corrélées."),
      br(),
      p('Le second indicateur est un indicateur de la proportion d’extremums communs entre les deux courbes. Pour cela, on utilise la fonction « peaks » du package «', 
        tags$a(href="https://github.com/tgouhier/synchrony","synchrony"),
        '». Cette fonction compte le nombre de fois où les deux séries atteignent un maximum en même temps, puis les minimums pour ramener cela à la proportion totale de pics (déterminée en sommant le nombre de maximums de la série en comptant le plus à celui de minimums).
            Pour tester si ce nombre est important la fonction procède à une estimation via  une méthode de Monte Carlo, en mélangeant plusieurs fois les deux séries pour observer le nombre de pics communs dans chaque cas, et voir si ces valeurs sont éloignées ou non de la proportion initiale.
            Si on rejette l’hypothèse que la synchronicité des pics est du au hasard, on affiche "Les pics des deux courbes sont atteints en même temps très souvent.", sinon "On ne peut pas dire que les pics des deux courbes sont souvent atteints en même temps.".')
    ),
      br(),
    br(),
  uiOutput(ns("result"))
  )

}


server_6 <- function(input,output,session,data){
  ns <- session$ns
  
  observe({ # update sensor selection according to import tab
    if (!is.null(data$sensors)){
      names_selected_sensors <- setNames(data$sensors,sensor_names[sensor_ids%in%data$sensors])
      updateSelectInput(session, "sensor1", choices = names_selected_sensors)
      updateSelectInput(session, "sensor2", choices = names_selected_sensors)
    }
    
  })
  
  #--- function application ---
  result <- reactive({
    plot_deseas(data$data, input$sensor1,input$sensor2, input$heure,input$direction1, input$direction2, input$mobility, input$norm)
  })
  
  #--- output definition ---
  output$graph_trend <- renderPlot({
    result()$trend
  })
  
  output$graph_cycle <- renderPlot({
    result()$seasonal
  })
  
  output$graph_noise <- renderPlot({
    result()$random
  })
  
  output$corr <- renderUI({
    ligne1 <- paste("Coefficient de correlation :",result()$correl) # Fabrication du texte pour l'affichage
    if(result()$correl>0.5){
      ligne2 <- "C'est une valeur élevée, les deux courbes sont corrélées "
    }
    if(result()$correl<=0.5){
      ligne2 <- "C'est une valeur moyenne, les deux courbes sont légèrement corrélées "
    }
    if(result()$correl<0.2){
      ligne2 <- "C'est une valeur faible, les deux courbes ne sont pas corrélées "
    }
    HTML(paste(ligne1,ligne2,sep="<br/>"))
  })
  
  output$pic <- renderUI({
    ligne1 <- paste("Taux de synchronicité des pics :",round(result()$peaks$obs,3)) # Fabrication du texte pour l'affichage
    if(result()$peaks$pval<0.05){
      ligne2 <- "Les pics des deux courbes sont atteints en même temps très souvent."
    }else{
      ligne2 <- "On ne peut pas dire que les pics des deux courbes sont souvent atteints en même temps."
    }
    HTML(paste(ligne1,ligne2,sep="<br/>"))
  })
  
  output$result <- renderUI({
    if (is.null(data$sensors)){
      p(class="text-center","Pour afficher le graphique, veuillez sélectionner des capteurs dans l'onglet import.")
    }else if (is.null(result())){
      p("Période commune des deux capteurs trop courte")
    } else {
      column(width = 9,
        tabsetPanel(
          tabPanel("Tendance",
                   h3("Comment évolue la circulation au long de la présence du capteur?"),
                   plotOutput(ns("graph_trend"))),
          tabPanel("Cycle hebdomadaire",
                   h3("Quel est l'effet du jour de la semaine ?"),
                   plotOutput(ns("graph_cycle"))),
          tabPanel("Bruit",
                   h3("Peut-on dire que les usagers ont le même comportement sur les deux segments de routes ?"),
                   plotOutput(ns("graph_noise")),
                   htmlOutput(ns("corr")),
                   htmlOutput(ns("pic")))),
          downloadButton(ns("downloaddonnees_complementaires"), "Import des données")
      )
    }
  })
  
  # Import
  output$downloaddonnees_complementaires <- downloadHandler(
    filename = "Comparaison_capteur.csv",
    content = function(file) {
      write_excel_csv2(result()$export_d, file)
    }
  )
}
