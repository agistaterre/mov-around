

##############################################
#                  Module                    #
##############################################

ui_8 <- function(id) {
  ns <- NS(id)
  tagList(
    column(3, wellPanel(
      selectInput(ns("sensor"),
                  label = "Choix du capteur",
                  choices = NULL),
      dateRangeInput(inputId = ns("ns_date"),
                     "Période",
                     start  = starting_date,
                     end    = ending_date - days(1),
                     min    = starting_date,
                     max    = ending_date - days(1)),
      radioButtons(inputId = ns("ns_vacance"), label = "Vacances comprises",
                   choices = c("Oui" = "YES", "Non" = "NO", "Seulement les vacances" = "ONLY"),
                   selected = "YES"),
      radioButtons(inputId = ns("ns_jf"), label = "Jours fériés compris",
                   choices = c("Oui" = "YES", "Non" = "NO", "Seulement les jours fériés" = "ONLY"),
                   selected = "YES"),
      checkboxGroupInput(
        inputId = ns("ns_sm"),
        label = "Choix des jours",
        # We have to put weekdays in English because shinyapps's server is in English
        selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
        choiceNames = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"),
        choiceValues = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
        inline = TRUE
      ),
      checkboxGroupInput(
        inputId = ns("ns_hour"),
        label = "Choix des heures",
        selected = c("08:00:00", "09:00:00", "10:00:00", "11:00:00", "12:00:00", "13:00:00",
                     "14:00:00", "15:00:00", "16:00:00"),
        choiceNames = c("06h", "07h", "08h", "09h", "10h", "11h", "12h", "13h", "14h", "15h", "16h", "17h", "18h"),
        choiceValues = c("06:00:00", "07:00:00", "08:00:00", "09:00:00", "10:00:00", "11:00:00", "12:00:00", "13:00:00",
                         "14:00:00", "15:00:00", "16:00:00", "17:00:00", "18:00:00"),
        inline = TRUE
      ),
      actionButton(ns("mise_a_j"), "Mettre à jour"),
      downloadButton(ns("downloadData"), "Import des données"))),
    tags$head(
      tags$script(HTML("
      $(document).ready(function() {
        $('#toggleMethodButton_8').click(function() {
          $('#methodText_8').toggle();
        });
      });
    "))
    ),
    column(width = 9,
           h3("Gestions des valeurs manquantes"),
           p("Dans notre analyse, nous considérons les données comme manquantes si l’uptime
             est inférieur à 0,5. De plus, nous ne considérons que les données de 06h à 19h
             puisque les capteurs Telraam ne fonctionne que de jours."),
           actionButton("toggleMethodButton_8", "Détails statistiques", style = "display: block; margin: 0 auto;"),
           div(id = "methodText_8", style = "display: none;",
               p( "Nous remplaçons les données de la manière suivante en suivant le schéma
               ci-dessous, tout d’abord nous transformons le tableau de données de manière
               à avoir les jours en lignes et les heures en colonnes. Ensuite nous faisons
               une interpolation linéaire sur les lignes du tableau puis sur ses colonnes.
               Lors de ce processus, si nous avons plus de 4 valeurs manquantes consécutives
               , en ligne ou en colonne, nous ne remplaçons pas ces données."),
               tags$img(src="logigramme.png", height=400)),
           h3("Quelques informations sur le capteur dans sa globalité"),
           uiOutput(ns("resume")),
           h3("Distribution des valeurs manquantes avant et après traitement (pour le capteur sélectionné)"),
           uiOutput(ns("plot_sensor")),
           h3("Proportion de valeurs manquantes par mois après traitement (pour tous les capteurs importés)"),
           uiOutput(ns("plot_resume")))
  )
}

server_8 <- function(input, output, session, data){
  ns <- session$ns

  # pour mettre a jour le choix des capteurs selon liste_capteur
  observe({
    updateSelectInput(session, "sensor", choices = sensor_names[which(sensor_ids %in% data$sensors)])
  })

  output$plot_sensor <- renderUI({

    if (is.null(data$sensors)){
      p("Import nécessaire")
    }else{

    input$mise_a_j

    isolate({
      sensor <- sensor_ids[which(sensor_names == input$sensor)]
      p <- gg_na_dist(data$data_comp,
                              sensor = sensor,
                              start = as.character(input$ns_date[1]),
                              end = as.character(input$ns_date[2]),
                              hours = input$ns_hour,
                              list_weekday = input$ns_sm,
                              pub_holidays = input$ns_jf,
                              holidays = input$ns_vacance)
    renderPlot(p)
    })}
  })

  output$resume <- renderUI({

    if (is.null(data$sensors)){
      p("Import nécessaire")
    }else{
      sensor <- sensor_ids[which(sensor_names == input$sensor)]

      texte <- na_prop(data$data_comp,
                       start = as.character(input$ns_date[1]),
                       end = as.character(input$ns_date[2]),
                       sensor = sensor)
      HTML(paste(texte, collapse = '<br/>'))}
  })

  output$plot_resume <- renderUI({
    if (is.null(data$sensors)){
      p("Import nécessaire")
    }else{

      input$mise_a_j

      isolate({ p <- gg_na_heatmap(data$data_comp,
                                   start = as.character(input$ns_date[1]),
                                   end = as.character(input$ns_date[2]),
                                   hours = input$ns_hour,
                                   list_weekday = input$ns_sm,
                                   pub_holidays = input$ns_jf,
                                   holidays = input$ns_vacance)
      renderPlot(p)
      })
      }
  })

  output$downloadData <- downloadHandler(
    filename = "Donnee_complete.csv",
    content = function(file) {
      write_excel_csv2(data$data_comp, file)
    }
  )
}


