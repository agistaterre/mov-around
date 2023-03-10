

shinyUI(
  fluidPage(
    theme = shinytheme("journal"),
    # navbarPage
    navbarPage(
      tags$img(src="AGT.png",height=35),
      tabPanel("Accueil",
               p("Bienvenue sur cette application ayant pour objectif de fournir des outils pour l’analyse
                 des données de mobilités captés par les capteurs Telraam installé à Chateaubourg.
                 Cette application se décompose en plusieurs onglets."),
               HTML('<center><img src="chateaubourg.jpg" height="500"></center>'),
               h5("Onglet import :"),
               p("Cet onglet permet d’importer les données que vous voulez analyser. 
                 L'import pouvant être long, il est judicieux de ne choisir que les capteurs et la période qui vous intéresse. 
                 Il est important de commencer par importer les données, sinon aucune analyse ne peut être faite. 
                 L’import est réussi si le texte en bas de l’onglet passe de « Import à faire » à « Fait »."),
               h5("Onglet comparaison de périodes :"),
               p("Cet onglet permet de voir, pour un même capteur, s’il y a une différence de comportement
               des usagers entre périodes différentes. Pour cela, vous devez choisir les caractéristiques
               d’une période de référence et d’une ou deux périodes avec lesquelles vous voulez les comparer."),
               p(" Les différences s’observent avec 2 outils :"),
               p("- Un graphique montrant la circulation moyenne en fonction des heures de la journée
               pour chaque période."),
               p("- Une barre colorée, donnant pour chaque heure, le résultat d’un test statistique pour
                 qualifier la différence de répartition entre 2 périodes."),
               h5("Onglet heure d’engorgement :"),
               p("Cet onglet permet de visualiser, pour un capteur, la circulation dans chaque direction
                 (voitures et poids lourds) et la vitesse V85 (vitesse telle que 85 % des usagers roulent 
                 plus lentement), en fonction des heures de la journée. Cela permet d’observer les heures
                 pouvant causer des ralentissements."),
               h5("Onglet seuil d’engorgement :"),
               p("Cet onglet permet de visualiser le pourcentage d’usagers (voitures et poids lourds) arrivant
                 à dépasser 10km/h, 20km/h, 30km/h et 40km/h en fonction du nombre de véhicules sur la route.
                 L’objectif est de pouvoir chercher un changement brusque dans ces courbes pour déterminer un
                 seuil d’apparition des ralentissements. Le programme propose un seuil calculé automatiquement.
                 Ce seuil n’est pas forcément précis, vous pouvez décider d’afficher un seuil manuel."),
               h5("Onglet comparaison de deux capteurs :"),
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
               h5("Onglet méthodes et avertissements :"),
               p("Donne des avertissements relatifs à l’interprétation des données et les méthodes statistiques
                 utilisées pour mettre en place l’application.")
      ),
      tabPanel("Import",
               h2("Les données"),
               p("L'application importe des données depuis le site de Telraam :",
                 tags$a(href="https://telraam.net/#14/48.1014/-1.3862","ici"),
                 ". L'import de toute les données est relativemant lourd et peut prendre du temps. Veuillez donc ne selectionner que les données que vous utiliserez :"), 
               fluidRow(
                 column(5,
               checkboxGroupInput(
                 inputId="Capteurs",
                 label="Choix des capteurs",
                 choiceNames = listeNombis,
                 choiceValues = listeNom
               ),
               h5("Capteur supplémentaire:"),
               fluidRow(
               column(6,
                      textInput("captIDsup", "Identifiant", value = "")),
               column(6,
                      textInput("nomIDsup", "Nom", value = "")))
               ,
               dateRangeInput("daterange", "Période",
                              start  = "2021-01-01",
                              end    = Sys.Date()-days(1),
                              min    = "2021-01-01",
                              max    = Sys.Date()-days(1)),
               actionButton("go_import", "Import"),
               textOutput("Etat")),
               column(7,
                      tags$img(src="carte capteur apli.jpg",height=500)))
                      
               ),
      tabPanel("Comparaison de périodes", 
               fluidRow(
                 column(3,wellPanel(
                   uiOutput("Box1"),
                   selectInput("sens", label = "Direction", 
                               choices = c("Toute"="Toute","B vers A"="Rgt","A vers B" ="Lft"),
                               selected = "Toute"),
                   checkboxGroupInput(
                     "mobilite",
                     "Choix du type de mobilité",
                     selected = c("car","heavy"),
                     choiceNames = c("VL","PL","Piéton","Vélo"),
                     choiceValues = c("car","heavy","pedestrian","bike")
                   ),
                   h2(span(style="color:#006bb6","Periode de référence")),
                   dateRangeInput("daterange1", "Période",
                                  start  = "2021-01-01",
                                  end    = Sys.Date()-days(1),
                                  min    = "2021-01-01",
                                  max    = Sys.Date()-days(1)),
                   radioButtons(inputId = "Vacance1", label = "Vacances comprises :",
                                choices = c("Oui","Non","Seulement les vacances"),selected = "Oui"),
                   radioButtons(inputId = "JF1", label = "Jours fériés compris :",
                                choices = c("Oui","Non","Seulement les jours fériés"),selected = "Oui"),
                   checkboxGroupInput(
                     "SM1",
                     "Choix des jours",
                     selected = 1:7,
                     choiceNames = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
                     choiceValues = 1:7,
                     inline = TRUE
                   ),
                   h2(span(style="color:#ff5900","Première période de comparaison ")),
                   dateRangeInput("daterange2", "Période",
                                  start  = "2021-01-01",
                                  end    = Sys.Date()-days(1),
                                  min    = "2021-01-01",
                                  max    = Sys.Date()-days(1)),
                   radioButtons(inputId = "Vacance2", label = "Vacances comprises :",
                                choices = c("Oui","Non","Seulement les vacances"),selected = "Oui"),
                   radioButtons(inputId = "JF2", label = "Jours fériés compris :",
                                choices = c("Oui","Non","Seulement les jours fériés"),selected = "Oui"),
                   checkboxGroupInput(
                     "SM2",
                     "Choix des jours",
                     selected = 1:7,
                     choiceNames = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
                     choiceValues = 1:7,
                     inline = TRUE
                   ),
                   h2(span(style="color:#00b308","Seconde période de comparaison")),
                   dateRangeInput("daterange4", "Période",
                                  start  = "2021-01-01",
                                  end    = Sys.Date()-days(1),
                                  min    = "2021-01-01",
                                  max    = Sys.Date()-days(1)),
                   radioButtons(inputId = "Vacance3", label = "Vacances comprises :",
                                choices = c("Oui","Non","Seulement les vacances"),selected = "Oui"),
                   radioButtons(inputId = "JF3", label = "Jours fériés compris :",
                                choices = c("Oui","Non","Seulement les jours fériés"),selected = "Oui"),
                   checkboxGroupInput(
                     "SM3",
                     "Choix des jours",
                     selected = 1:7,
                     choiceNames = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
                     choiceValues = 1:7,
                     inline = TRUE
                   ),
                   actionButton("mise_a_j", "Mettre à jour")
                 )), 
                 column(width = 9,
                        uiOutput("OutBox13"),
                        uiOutput("OutBox1"),
                        uiOutput("OutBox14"),
                        uiOutput("OutBox12"),
                        uiOutput("OutBox11"),
                        uiOutput("OutBox9"),
                        htmlOutput("OutBox15")
                        
                 )
               )
      ),
      tabPanel("Heure d'engorgement",
               fluidRow(
                 column(3,wellPanel(
                   uiOutput("Box5"),
                   dateRangeInput("daterange5", "Période",
                                  start  = "2021-01-01",
                                  end    = Sys.Date()-days(1),
                                  min    = "2021-01-01",
                                  max    = Sys.Date()-days(1)),
                   
                   radioButtons(inputId = "Vacance4", label = "Vacances comprises :",
                                choices = c("Oui","Non","Seulement les vacances"),selected = "Non"),
                   radioButtons(inputId = "JF4", label = "Jours fériés compris :",
                                choices = c("Oui","Non","Seulement les jours fériés"),selected = "Non"),
                   checkboxGroupInput(
                     "SM4",
                     "Choix des jours",
                     selected = 1:5,
                     choiceNames = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
                     choiceValues = 1:7,
                     inline = TRUE
                   ))),
                 column(width = 9,
                        h3("Quelle est l'heure d'engorgement ?"),
                        uiOutput("OutBox16"),
                        p("La vitesse V85 est la vitesse telle que 85% des usagers roulent plus lentement que celle ci."),
                        uiOutput("OutBox19")
                 ))
      ),
      tabPanel("Seuil d'engorgement",
               fluidRow(
                 column(3,wellPanel(
                   uiOutput("Box4"),
                   selectInput("sens3", label = "Direction du capteur", 
                               choices = c("Toute"="Toute","B vers A"="Rgt","A vers B" ="Lft")),
                   selectInput("vit", label = "Choix de la courbe servant à déterminer le seuil",
                               choices = c("Toute","plus de 10km/h","plus de 20km/h","plus de 30km/h","plus de 40km/h"),
                               selected = "Toute"),
                   dateRangeInput("daterange3", "Période",
                                  start  = "2021-01-01",
                                  end    = Sys.Date()-days(1),
                                  min    = "2021-01-01",
                                  max    = Sys.Date()-days(1)),
                   actionButton("mise_a_j2", "Mettre à jour"),
                   radioButtons(
                     "calcul_seuil",
                     "Choix du seuil :",
                     selected = "automatique",
                     choices = c("automatique","manuel"),
                     inline = TRUE
                   ),
                   uiOutput("Box6")
                 )), 
                 column(width = 9,
                        h5("Précisions sur le graphique"),
                        p("Le graphique suivant indique pour chaque courbe le pourcentage de conducteurs (véhicules légers
                     et poids lourds) qui arrivent à dépasser la vitesse spécifiée en fonction du nombre d'autres 
                     conducteurs sur la route durant une même période horaire."),
                        p("Un changement brusque dans les courbes peut indiquer une présence régulière d'embouteillage 
                     lorsqu'on dépasse la valeur du changement. La barre rouge indique cette valeur."),
                        br(),
                        p("Avertissement :"),
                        p("1. La barre apparait toujours, même pour les routes sans embouteillages."),
                        p("2. Le calcul conduisant au placement de la barre n'est pas parfait : elle peut être mal placée."),
                        uiOutput("OutBox2"),
                        uiOutput("OutBox17"),
                        uiOutput("OutBox18")
                 )
               )
      ),
      tabPanel("Comparaison de deux capteurs",
               fluidRow(
                 column(3,wellPanel(
                   uiOutput("Box2"),
                   selectInput("sens1", label = "Direction du capteur",  
                               choices = c("Toute"="Toute","B vers A"="Rgt","A vers B" ="Lft")),
                   uiOutput("Box3"),
                   selectInput("sens2", label = "Direction du capteur",  
                               choices = c("Toute"="Toute","B vers A"="Rgt","A vers B" ="Lft")),
                   selectInput("heure", label = "Choix de l'heure", 
                               choices = c("6h-7h"=7,"7h-8h"=8,"8h-9h"=9,"9h-10h"=10,"10h-11h"=11,"11h-12h"=12,
                                           "12h-13h"=13,"13h-14h"=14,"14h-15h"=15,"15h-16h"=16,"16h-17h"=17,
                                           "17h-18h"=18,"18h-19h"=19,"19h-20h"=20,"20h-21h"=21),
                               selected = 9),
                   checkboxGroupInput(
                     "mobilite2",
                     "Choix du type de mobilité",
                     selected = c("car","heavy"),
                     choiceNames = c("VL","PL","Piéton","Vélo"),
                     choiceValues = c("car","heavy","pedestrian","bike")
                   ),
                   radioButtons(inputId = "Norm1", label = "Normaliser :",
                                choices = c("Oui","Non"),selected = "Non",inline = TRUE),
                   actionButton("mise_a_j3", "Mettre à jour")
                 )), 
                 column(width = 9,
                        tabsetPanel(
                          tabPanel("Tendance",
                          h3("Comment évolue la circulation au long de la présence du capteur?"),
                          uiOutput("OutBox3")),
                          tabPanel("Cycle hebdomadaire",
                          h3("Quel est l'effet du jour de la semaines ?"),
                          uiOutput("OutBox4")),
                          tabPanel("Bruit",
                          h3("Peut-on dire que les usagers ont le même comportement sur les deux segments de routes ?"),
                          uiOutput("OutBox5"),
                          uiOutput("OutBox6"),
                          uiOutput("OutBox7"))),
                        uiOutput("OutBox10")
                 )
               )
      ),
      tabPanel("Méthodes et avertissements",
          h3("Avertissement relatif à la qualité des données :"),
          p("Les données des capteurs Telraam ne sont pas issues d’une mesure continue sur une heure. Pour améliorer la qualité des données futures, les capteurs dédient une partie de leur temps d’activité à l’apprentissage. Les données totales sont reconstituées à partir du temps de mesures. Plus cette période de mesure est longue plus la qualité des données est grande. Telraam donne un outil de mesure de ce temps de mesure : l’uptime. Dans cette application, nous n'avons conservé que les données d’uptime supérieur à 0.5 (seuil conseillé par Telraam). Toutefois, les capteurs placés récemment (en période d’apprentissage) et les données matinales ou dans la soirée (visibilité réduite à cause de la nuit)  peuvent présenter des uptimes plus faible. 
          De plus la suppression des données à l’uptime trop faible fait qu’on possède moins de données pour les périodes à risque. La qualité des estimations et des tests est moins bonne sur ces périodes.
          Il faut donc être prudent en interprétant ces données."),
          h3("Avertissement relatif aux catégories de mobilités :"),
          p("Les capteurs Telraam utilisés ont des difficultés à différencier les grosses voitures comme les SUV des poids lourds. Le nombre de poids lourds est donc sur-évalué et le nombre de voitures sous-évalué. Toutefois, le total voitures + camions est précis.
          De la même façon, il faut être prudent dans la différenciation entre vélos et piétons."),
          h3("Explicitation des statistiques mis en place (nécessite des compétences en statistique) :"),
          h4("Comparaison de périodes :"),
          p("Méthode pour tracer les courbes :") ,
          p("Selon la sélection de l’utilisateur, on filtre les données pour ne garder que le trafic correspondant aux mobilités, capteur, direction et contraintes de dates sélectionnés. On réalise ensuite une moyenne pour chaque créneau horaire.
          On a rajouté un intervalle de confiance à 95% autour de nos courbes. Pour chaque créneau horaire, on a estimé la variance des données, ce qui nous a permis d’obtenir l’intervalle de confiance (à partir d’une loi de Student)."),
          br(),
          p("Méthode pour la significativité de la différence :"),
          p("On s’appuie sur un test de Wilcoxon Mann Whitney. L’idée est de comparer, pour chaque créneau horaire, la répartition des valeurs de chacune des périodes. Le test consiste à regarder la distance entre les fonctions de répartition empirique, si elles sont éloignées, le test rejette l’hypothèse nulle :  l’égalité des lois. L’option « Significatif » indique une p-value inférieure à 0.05, celle « Entre deux » une  p-value entre 0.05 et 0.1 et celle « Non significatif » une p-value supérieure à 0.1."),
          h4("Seuil d’engorgement :"),
          p("Méthode pour tracer les courbes :"),
          p("On commence par filtrer les données selon les sélections de l’utilisateur. On isole la partie correspondant au pourcentage de conducteur dépassant chaque vitesse. On range les données dans l’ordre croissant du nombre de véhicules (voitures + camions). On a pré-lissé les données à l’aide d’une moyenne glissante d’une amplitude de 50 pour dégager un début tendance (courbe noir du graphique). À partir de cette tendance, on a lissé nos données à l’aide de l’outil geom_smooth de ggplot2. Ces courbes de lissages sont les courbes colorées du graphique."),
          br(),
          p("Méthode pour trouver le seuil :"),
          p("L’objectif est de déterminer un seuil de rupture dans la courbe de lissage. Pour cela, on utilise un test de Darling Erdös (dérivé du test de CUSUM). La fonction est implémentée dans le Package",
            tags$a(href="https://github.com/ntguardian/CPAT","CPAT"), 
            "(Curtis Miller)."),
          h4(" Comparaison de période :"),
          p("Séparation en tendance, cycle et bruit :"),
          p("Après un filtrage des données selon les choix de l’utilisateur, on détermine la période d’activité commune des deux capteurs sélectionnées. 
          Pour trouver la tendance (évolution générale du flux),  pour chaque capteur, on réalise une moyenne glissante sur 14 jours (2 périodes hebdomadaires), c’est le graphique du premier onglet. 
          On soustrait la tendance au flux total pour avoir de données sans tendance. Pour ces données, on fait une moyenne sur tous les lundis, puis les mardis, etc. Cela nous donne les valeurs associées au cycle de la semaine (second onglet). La partie restante après la soustraction du cycle hebdomadaire correspond au bruit statistique (troisième onglet)."),
          br(),
          p("Indicateurs du lien entre les bruits :"),
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
      )
    ))     
)