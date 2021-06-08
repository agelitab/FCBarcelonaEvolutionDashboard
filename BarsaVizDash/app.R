#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Carreguem (i instal·lem si necessari) llibreries
if (!require(ggplot2)){
    install.packages("ggplot2")
}
if (!require(plotly)){
    install.packages("plotly")
}
if (!require(dplyr)){
    install.packages("dplyr")
}
if (!require(reshape2)){
    install.packages("reshape2")
}
if (!require(shiny)){
    install.packages("shiny")
}

library(ggplot2)
library(plotly)
library(dplyr)
library(reshape2)
library(shinydashboard)
library(shiny)


# càrrega dades
season_stats <- read.csv("season_stats.csv")
tactics_formation <- read.csv("tactics_formation.csv")
player_stats <- read.csv("player_stats.csv", quote = "")


# creació estadístiques addicionals
season_stats$`G-xG` <- season_stats$Goals - season_stats$xG
season_stats$`GA-xGA` <- season_stats$Goals_Against - season_stats$xGA


# codi grafic season_stats
season_stats_plot <-  plot_ly(season_stats, x = ~Season) %>%
    add_trace(y = ~Goals, type = 'bar', name = 'G', visible = TRUE) %>%
    add_trace(y = ~xG, name = 'xG', type  ='bar', visible = TRUE) %>%
    add_trace(y = ~`G-xG`, name = 'G-xG', type = 'bar', visible = FALSE) %>%
    add_trace(y = ~Goals_Against, name = 'GA', type = 'bar', visible = FALSE) %>%
    add_trace(y = ~xGA, name = 'xGA', type = 'bar', visible = FALSE) %>%
    add_trace(y = ~`GA-xGA`, name = 'GA-xGA', type = 'bar', visible = FALSE) %>%
    layout(
        title = "Season Stats",
        updatemenus = list(
            list(
                x = -0.1,
                y = 0.9,
                buttons = list(
                    list(method = "restyle",
                         args = list("visible", list(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)),
                         label = "Goals"),
                    list(method = "restyle",
                         args = list("visible", list(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)),
                         label = "Goals Against"),
                    list(method = "restyle",
                         args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)),
                         label = "Differences")
                )
            )
        )
    )


# codi gràfic formacions
formations_plot <- plot_ly() %>%
    add_pie(data = tactics_formation[tactics_formation$Season == "2008/2009" | tactics_formation$Season == "2009/2010" | tactics_formation$Season == "2010/2011" | tactics_formation$Season == "2011/2012", ],
            labels = ~tactics.formation, value = ~count, name = "Guardiola's Era", visible = TRUE) %>%
    add_pie(data = tactics_formation[tactics_formation$Season == "2012/2013", ],
            labels = ~tactics.formation, value = ~count, name = "Tito's  Era", visible = FALSE) %>%
    add_pie(data = tactics_formation[tactics_formation$Season == "2013/2014", ],
            labels = ~tactics.formation, value = ~count, name = "Tata Martino's  Era", visible = FALSE) %>%
    add_pie(data = tactics_formation[tactics_formation$Season == "2014/2015" | tactics_formation$Season == "2015/2016" | tactics_formation$Season == "2016/2017", ],
            labels = ~tactics.formation, value = ~count, name = "Luis Enrique's Era", visible = FALSE) %>%
    add_pie(data = tactics_formation[tactics_formation$Season == "2017/2018" | tactics_formation$Season == "2018/2019", ],
            labels = ~tactics.formation, value = ~count, name = "Valverde's Era", visible = FALSE) %>%
    add_pie(data = tactics_formation[tactics_formation$Season == "2019/2020", ],
            labels = ~tactics.formation, value = ~count, name = "Quique Setien's Era", visible = FALSE) %>%
    layout(
        title = "Tactical formations for coach",
        updatemenus = list(
            list(
                x = -0.1,
                y = 0.9,
                buttons = list(
                    list(method = "restyle",
                         args = list("visible", list(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)),
                         label = "Josep Guardiola"),
                    list(method = "restyle",
                         args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)),
                         label = "Tito Vilanova"),
                    list(method = "restyle",
                         args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)),
                         label = "Tata Martino"),
                    list(method = "restyle",
                         args = list("visible", list(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)),
                         label = "Luis Enrique"),
                    list(method = "restyle",
                         args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)),
                         label = "Ernesto Valverde"),
                    list(method = "restyle",
                         args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)),
                         label = "Quique Setién")
                )
            )
        )
    )


# defineixo les temporades amb què treballaré
seasons <- season_stats$Season


# creo DASHBOARD
url <- a("StatsBomb Open Source Data", href = "https://github.com/statsbomb/open-data")

header = dashboardHeader(title = "EVOLUCIÓ DEL F.C. BARCELONA (2008-2020)", titleWidth = 475)

sidebar = dashboardSidebar(
    sidebarMenu(
        menuItem("Season Stats & Formations", tabName = "season_stats", icon = icon('calendar')),
        menuItem("Player Stats per Season", tabName = "player_stats", icon = icon('user'))
    )
)

body = dashboardBody(
    tabItems(
        tabItem(tabName = "season_stats",
                h1("Evolució de l'equip al llarg les temporades i entrenadors", align = "center"),
                h5("Data:", url, align = "center"),
                fluidRow(
                    column(width = 6,
                           box(width = NULL, plotlyOutput("stats")),
                           box(title = "Èpoques", width = NULL, background = "light-blue",
                               "Guardiola  <-  08/09 - 11/12", br(), "Tito  <-  12/13", br(), "Tata  <-  13/14", br(),
                               "Luis Enrique  <-  14/15 - 16/17", br(), "Valverde  <-  17/18 - 18/19", br(),
                               "Setién  <-  19/20"),
                           box(title = "Conclusions", width = NULL, background = "navy",
                               "- L'etapa més perillosa en quant a qualitat d'atac del Barça va ser amb Luis Enrique i Guardiola
                    (respectivament) amb els xG més elevats en els últims anys, tot i que l'últim any de Guardiola suposa
                    la cimera de qualitat d'ocasions en aquests últims 12 anys   -->   El treball dóna els seus fruits",
                               br(), "- L'etapa de Guardiola no és només la que genera de les millors ocasions, sinó que veiem també
                    que ens trobem davant d'un equip organitzat i sòlid defensivament (xGA més baixos dels últims 12 anys,
                    seguit pels registres del Barça de Luis Enrique)", br(), "- L'etapa de màxima gràcia de definció dels
                    jugadors es produeix la temporada 12/13 amb una diferència G-xG = 35.27", br(), "- L'ús excessiu
                    d'estratègies defensives per part d'Ernesto Valverde no va significar un equip més sòlid defensivament.
                    Tot el contrari: l'equip rebia ocasions de gol molt més perilloses que els altres anys", br(),
                               "- Malgrat la manca defensiva de les temporades de Valverde, veiem el paper providencial de Marc-André
                    Ter Stegen, permetent que l'equip encaixés menys gols dels esperats tant en els dos anys i mig de
                    Valverde com en el mig de Quique Setién", br(), br(), br(), "PODEM CONCLOURE LA PÈRDUA D'IDENTITAT DEL F.C. 
                    BARCELONA AL LLARG DE LES TEMPORADES.", br(), " ENTENDREM, PART DELS MOTIUS, EN LA PÀGINA SEGÜENT;
                    PERÒ ESTÀ CLAR QUE L'APOSTA D'ENTRENADORS QUE NO SÓN 'DE LA CASA' I NO ENTENEN EL CLUB, I LA NO APOSTA DE
                    LA DIRECTIVA ROSSELL-BARTOMEU PER TOT EL QUE REPRESENTA EL BARÇA (DES D'ESTIL DE JOC FINS A POTENCIMENT
                    DE LA MÀSIA) SÓN LES PRINCIPALS CAUSES DE LA DEVALLADA DEL BARÇA EN ELS ÚLTIMS ANYS")
                    ),
                    column(width = 6,
                           box(width = NULL, plotlyOutput("formacions")),
                           box(title = "Ànalisi", width = NULL, background = "light-blue",
                               "- Observem que Guardiola, Tito, Tata i Luis Enrique són els entrenadors més fidels
                         a l'estil Barça; però només Guardiola i Luis Enrique aconsegueixen mantenir un 
                         'GA-xGA' negatiu, mentre que Tito aposta per un joc més ofensiu (major 'G-xG')",
                               br(), "- Per altra banda, Valverde i la mitja temporada de Setién també tenen 
                         'GA-xGA'<0 però a costa de renunciar a l'estil Barça: aposten per formacions
                         més defensives amb un baix ús del clàssic 4-3-3", br(), "- El període més golejador
                         és durant l'etapa de Luis Enrique, amb un dels millors, si no el millor, tridents
                         de la història", br(), "- Observem en l'any de Tito com a entrenador la major 
                         diferència entre G i xG, any de màxim rendiment cara a porta de les últimes  
                         temporades", br(), "- L'any de Tito també és el que més gols es van encaixar, tal 
                         i com hem dit, per la gran aposta ofensiva i de pressió de l'equip (això no es veu 
                         reflexat en aquesta visualització, però); però també podria deure's a la lesió de 
                         Victor Valdés i el fet que el Barça jugués amb Pinto gran part de la temporada", 
                               br(), "- Observem que Luis Enrique aconsegueix, en dues temporades (14/15 i 15/16) 
                         els majors registres defensius (menors GA) des de Guardiola. Valverde també ho 
                         aconsegueix, però a costa de comprometre l'ADN Barça")
                           
                    )
                    
                )
        ),
        tabItem(tabName = "player_stats",
                h1("És Messi un llop solitari, o s'hi ha trobat?", align = "center"),
                h3("Estadístiques per jugador i temporada (amb mínim un atribut de cada)", align = "center"),
                h5("Data:", url, align = "center"),
                fluidRow(
                    column(width = 4,
                           box(title = "Selecciona la temporada", selectInput("seasons", "Temporada:", seasons),
                               width = NULL),
                           box(title = "Anàlisi", width = NULL, background = "light-blue",
                               "- Podem des-seleccionar certs atributs com 'Key Passes' en la temporada 2008/2009 per tal 
                  de veure uns gràfics de barres més òptims", br(), "- 2008/2009: Messi reparteix joc i 
                  marca, però veiem que es troba rodejat d'altres golejadors com Henry i Eto'o, i creadors 
                  de joc com Xavi i Iniesta", br(), "- 2009/2010: Messi assumeix el rol de golejador de 
                  l'equip i segueix creant, però acompanyat de Xavi, Iniesta i, fins i tot, Dani Alves", br(),
                               "- 2010/2011: Messi ja és el màxim creador i golejador del Barça, però segueix tenint bons 
                  jugadors al costat que li complementen la feina en ambdues tasques", br(), "- A partir 
                  d'aquí, la muntanyeta d'estadístiques de Messi ressalta sobre les demés, fins l'arribada 
                  de Cesc la temporada 13/14, on hi tornem a veure als seus gran col·laboradors Xavi i 
                  Iniesta (absents les dues temporades anteriors per abscència de gols", br(), "- En els 3 
                  anys sota la direcció de Luis Enrique, veiem la importància del rol de Messi en l'equip, 
                  tot i que acompanyat d'un Luis Suárez més golejador amb el pas del temps, i un Neymar cada 
                  cop més golejador i creatiu. En la creació de joc també veiem l'important paper de Xavi 
                  (retirat la temporada 14/15) i Iniesta", br(), "- Durant les dues temporades de Valverde 
                  (17/18 i 18/19) veiem com Messi es converteix en l'únic creador perillós i golejador 
                  del Barça (ja que, en el segon any, podem veure una forta deballada de l'instint golejador 
                  de Sárez", br(), "- L'anàlisi culmina la temporada 19/20, on Messi és, amb diferència, 
                  l'únic jugador capaç de crear perillositat en l'equip blaugrana")
                    ),
                    column(width = 8,
                           box(plotlyOutput("players"), width = NULL),
                           box(title = "Conclusions", width = NULL, background = "navy",
                               "Observem que Messi no és pas un jugador individualista: tot i tenir els màxims 
                       registres golejadors en quasi cada temporada estudiada, és també el jugador que 
                       més jugades perilloses és capaç de crear degut als seus passes ('Key Passes' i 
                       'Through Ball'; que no són més que passes que acaben en xut o gol, i passes que 
                       trenquen l'última línia de defensa).", br(), br(),  "Podem comprovar que Messi ha 
                       envellit com el vi negre: cada any que passa els seus números baten els de l'any 
                       anterior.", br(), br(), "Tot i això, vèiem que, en les temporades exitoses del Barça, 
                       malgrat que Messi fós el futbolista clau, hi havia jugadors que li complementaven la 
                       feina, mentre que amb el pas dels anys, ha estat ell qui ha hagut d'assolir els rols 
                       creatius i golejadors de l'equip.", br(), br(), "Així doncs, sí que podríem dir que 
                       Messi és un llop solitari, però no perquè ell vulgui: amb els anys ha perdut la seva 
                       manada i no li han sabut trobar una altra de nova.")
                    )
                    
                )
        )
    )
)


# ----------------------------------------------------

ui <- dashboardPage(
    header,
    sidebar,
    body,
    
)

server <- function(input, output){
    
    output$stats <- renderPlotly({
        season_stats_plot
    })
    
    output$formacions <- renderPlotly({
        formations_plot
    })
    
    output$players <- renderPlotly({
        
        jugadors <- player_stats[player_stats$Season == input$seasons, ]
        
        plot_ly() %>%
            
            add_trace(data = jugadors, x = ~player.name, y = ~Goals, type = 'bar', name = "Goals") %>%
            add_trace(data = jugadors, x = ~player.name, y = ~xG, type = 'bar', name = "xG") %>%
            add_trace(data = jugadors, x = ~player.name, y = ~key_passes, type = 'bar', name = "Key Passes") %>%
            add_trace(data = jugadors, x = ~player.name, y = ~through_ball, type = 'bar', name = "Through Ball",
                      visible = TRUE) %>%
            add_trace(data = jugadors, x = ~player.name, y = ~assists, type = 'bar', name = "Assists") %>%
            layout(
                title = "Players Stats per Season",
                xaxis = list(title = "Player"),
                yaxis = list(title = "Goal/Pass Value")
            )
    })
}


# corro dashboard
shinyApp(ui, server)





