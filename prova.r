library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ECharts2Shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)

#Lendo o arquivo csv
data <- read.csv(file= "Amostra.csv", encoding = "latin1")

#Total de participantes
totalParticipants <- data %>% summarise(quantParticipants = n()) %>% collect()

#Total de participantes por cada estado
stateParticipants <- data %>% group_by(Estado) %>% summarise(quantStateParticipants = n()) %>% collect()

#Calculo da media da idade
averageAges <- data %>% filter(Idade > 0) %>% summarise(mediaIdade = mean(Idade)) %>% collect()
averageAges <- round(averageAges, 2)

#Local dos participantes
local <- "Todo o Brasil"

#Sexo dos participantes
gender <- data %>% group_by(Sexo) %>% summarise(quantSexo = n()) %>% arrange(desc(quantSexo)) %>% collect()
gender$Sexo <- str_replace(gender$Sexo, "M", "Masculino")
gender$Sexo <- str_replace(gender$Sexo, "F", "Feminino")
genderPercentage <- c(rep(as.character(gender[1,1]), as.integer(gender[1,2])),
                      rep(as.character(gender[2,1]), as.integer(gender[2,2]))
                     )

#Conteudo mais e menos consumido total e por sexo
mostViewQuant <- data %>% group_by(Conteudo_mais_consumido) %>% summarise(quantMost = n()) %>% arrange(desc(quantMost))
lessViewQuant <- data %>% group_by(Conteudo_menos_consumido) %>% summarise(quantLess = n()) %>% arrange(desc(quantLess))

mostVideoM <- as.integer(data %>% filter(Conteudo_mais_consumido == "Videos" & Sexo == "M") %>% count() %>% collect())
mostVideoF <- as.integer(data %>% filter(Conteudo_mais_consumido == "Videos" & Sexo == "F") %>% count() %>% collect())

mostMusicsM <- as.integer(data %>% filter(Conteudo_mais_consumido == "Musicas" & Sexo == "M") %>% count() %>% collect())
mostmusicsF <- as.integer(data %>% filter(Conteudo_mais_consumido == "Musicas" & Sexo == "F") %>% count() %>% collect())

mostPodcastsM <- as.integer(data %>% filter(Conteudo_mais_consumido == "Podcasts" & Sexo == "M") %>% count() %>% collect())
mostPodcastsF <- as.integer(data %>% filter(Conteudo_mais_consumido == "Podcasts" & Sexo == "F") %>% count() %>% collect())

mostGamesM <- as.integer(data %>% filter(Conteudo_mais_consumido == "Jogos" & Sexo == "M") %>% count() %>% collect())
mostGamesF <- as.integer(data %>% filter(Conteudo_mais_consumido == "Jogos" & Sexo == "F") %>% count() %>% collect())

mostNetM <- as.integer(data %>% filter(Conteudo_mais_consumido == "Redes Sociais" & Sexo == "M") %>% count() %>% collect())
mostNetF <- as.integer(data %>% filter(Conteudo_mais_consumido == "Redes Sociais" & Sexo == "F") %>% count() %>% collect())

mostNewsM <- as.integer(data %>% filter(Conteudo_mais_consumido == "Noticias" & Sexo == "M") %>% count() %>% collect())
mostNewsF <- as.integer(data %>% filter(Conteudo_mais_consumido == "Noticias" & Sexo == "F") %>% count() %>% collect())

dataMostView <- data.frame(
                   Video = c(mostVideoM, mostVideoF),
                   Musicas = c(mostMusicsM, mostmusicsF),
                   Podcasts = c(mostPodcastsM, mostPodcastsF),
                   Jogos = c(mostGamesM, mostGamesF),
                   Redes_Sociais = c(mostNetM, mostNetF),
                   Noticias = c(mostNewsM, mostNewsF)
                )

dataMostView <- t(dataMostView)
dataMostView <- as.data.frame(dataMostView)
dataMostView <- rbind(0, dataMostView)
names(dataMostView) <- c("Masculino", "Feminino")
row.names(dataMostView) <- c("0","Videos", "Musicas", "Podcasts", "Jogos", "Redes Sociais", "Noticias")

lessVideoM <- as.integer(data %>% filter(Conteudo_menos_consumido == "Videos" & Sexo == "M") %>% count() %>% collect())
lessVideoF <- as.integer(data %>% filter(Conteudo_menos_consumido == "Videos" & Sexo == "F") %>% count() %>% collect())

lessMusicsM <- as.integer(data %>% filter(Conteudo_menos_consumido == "Musicas" & Sexo == "M") %>% count() %>% collect())
lessMusicsF <- as.integer(data %>% filter(Conteudo_menos_consumido == "Musicas" & Sexo == "F") %>% count() %>% collect())

lessPodcastsM <- as.integer(data %>% filter(Conteudo_menos_consumido == "Podcasts" & Sexo == "M") %>% count() %>% collect())
lessPodcastsF <- as.integer(data %>% filter(Conteudo_menos_consumido == "Podcasts" & Sexo == "F") %>% count() %>% collect())

lessGamesM <- as.integer(data %>% filter(Conteudo_menos_consumido == "Jogos" & Sexo == "M") %>% count() %>% collect())
lessGamesF <- as.integer(data %>% filter(Conteudo_menos_consumido == "Jogos" & Sexo == "F") %>% count() %>% collect())

lessNetM <- as.integer(data %>% filter(Conteudo_menos_consumido == "Redes Sociais" & Sexo == "M") %>% count() %>% collect())
lessNetF <- as.integer(data %>% filter(Conteudo_menos_consumido == "Redes Sociais" & Sexo == "F") %>% count() %>% collect())

lessNewsM <- as.integer(data %>% filter(Conteudo_menos_consumido == "Noticias" & Sexo == "M") %>% count() %>% collect())
lessNewsF <- as.integer(data %>% filter(Conteudo_menos_consumido == "Noticias" & Sexo == "F") %>% count() %>% collect())

dataLessView <- data.frame(
  Video = c(lessVideoM, lessVideoF),
  Musicas = c(lessMusicsM, lessMusicsF),
  Podcasts = c(lessPodcastsM, lessPodcastsF),
  Jogos = c(lessGamesM, lessGamesF),
  Redes_Sociais = c(lessNetM, lessNetF),
  Noticias = c(lessNewsM, lessNewsF)
)

dataLessView <- t(dataLessView)
dataLessView <- as.data.frame(dataLessView)
dataLessView <- rbind(0, dataLessView)
names(dataLessView) <- c("Masculino", "Feminino")
row.names(dataLessView) <- c("0","Videos", "Musicas", "Podcasts", "Jogos", "Redes Sociais", "Noticias")

ui1 <- fluidPage(
  ui <- uiOutput("ui"),
  
  tags$div(id = "container",
           tags$h1("Dashboard Pesquisa de Conteudo da Internet", style = "text-align: center; margin-bottom: 48px;"),
           tags$div(style = "display: flex; align-items: center; justify-content: center; margin-bottom: 48px;",
                    tags$img(src="logo.svg", height = 380, width = 380, )
           ),
           tags$p(style = "text-align: center; font-size: 24px; font-weight: bold; margin-bottom: 48px;", "Seja bem vindo ao Dashboard de uma pesquisa que teve por objetivo levantar alguns dados sobre quais sao os conteudos mais consumidos e menos consumidos pelas pessoas na Internet!"),
           tags$p(style = "text-align: center; font-size: 24px; font-weight: bold; color: red;", "Para visualizar os dados basta clicar no botao abaixo:"),
           tags$div(style = "display:flex; justify-content: center;",
                    actionButton("idButton", "Ir para os dados!")
           )
  )
)

ui2 <- dashboardPage(
  dashboardHeader(
    title = "Pesquisa Internet",
    titleWidth = "100%"
  ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '
    ))),
    loadEChartsLibrary(),
    loadEChartsTheme('shine'),
    
    tabsetPanel(
      id = "tabs",
      
      tabPanel(
        title = "Perfil dos Participantes",
        value = "Page1",
        fluidRow(style = "margin-top: 2%;",
          valueBoxOutput("totalParticipants", width = 4),
          valueBoxOutput("averageAges", width = 4),
          valueBoxOutput("local", width = 4)
        ),
        
        fluidRow(
          column(6,
                 style = "margin: 2%; width: 46%;",
                 plotOutput("graphStateParticipants")
          ),
          column(6,
                 style = "margin: 2%; width: 46%;",
                 plotOutput("graphGenderParticipants")
          )
          
        ),
        
        fluidRow(style = "display: flex; align-items:center; justify-content: center; margin: 0 20%;",
                 column(6,
                        tags$p("Sexo dos participantes (percentual):"),
                        tags$div(id="graphGenderPerc", style="height: 460px; "), 
                        deliverChart(div_id = "graphGenderPerc")
                 )
        )
      ),
      
      tabPanel(
        title = "Resultados",
        value = "Page2",
        
        fluidRow(
          column(6,
                 style = "margin: 2%; width: 46%;",
                 plotOutput("graphMostView")
          ),
          column(6,
                 style = "margin: 2%; width: 46%;",
                 plotOutput("graphLessView")
          )
        ),
        
        fluidRow(
          tags$h3(style= "text-align: center", "Conteudos mais consumidos por Sexo")
        ),
        
        fluidRow(
          column(12,
                 tags$div(id="graphMostViewGender", style="width:100%;height:300px; margin-bottom: 48px;"),
                 deliverChart(div_id = "graphMostViewGender")
          )
        ),
        
        fluidRow(
          tags$h3(style= "text-align: center", "Conteudos menos consumidos por Sexo")
        ),
        
        fluidRow(
          column(12,
                 tags$div(id="graphLessViewGender", style="width:100%; height:300px; margin-bottom: 48px;"),
                 deliverChart(div_id = "graphLessViewGender")
          )
        )
      ),
      
      tabPanel(
        title = "Tabela",
        
        fluidRow(
          DT::dataTableOutput("tabela")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$idButton, {
    output$ui <- removeUI(selector = "div#container")
    output$ui <- renderUI({ui2})
  })
  
  #Mostra o total de participantes
  output$totalParticipants <- renderValueBox({
    valueBox(totalParticipants, "Total de Participantes", icon = icon("users"), color="orange")  
  })
  
  #Mostra a media da idade
  output$averageAges <- renderValueBox({
    valueBox(averageAges, "Media de Idade", icon = icon("calendar-alt"), color="green")  
  })
  
  #Mostra o local dos participantes
  output$local <- renderValueBox({
    valueBox(local, "Local dos Participantes", icon = icon("globe"), color="blue")  
  })
  
  #Grafico de participantes por estado
  output$graphStateParticipants <- renderPlot({ggplot(stateParticipants) +
      aes(x = Estado, fill = Estado, weight = quantStateParticipants) +
      geom_bar() +
      scale_fill_hue() +
      labs(x = "Estados", y = "Quantidade", title = "Quantidade de participantes por estado", fill = "Estados") +
      theme_minimal()
  })
  
  #Grafico de participantes por sexo
  output$graphGenderParticipants <- renderPlot({ggplot(gender) +
      aes(x = Sexo, fill = Sexo, weight = quantSexo) +
      geom_bar(width = .50) +
      scale_fill_hue() +
      labs(x = "Sexos", y = "Quantidade", title = "Quantidade de participantes por sexo", fill = "Sexos") +
      theme_minimal()
  })
  
  #Grafico do percentual de participantes por sexo
  renderPieChart(div_id = "graphGenderPerc", data = genderPercentage, theme = "shine", radius = "75%", )
  
  #Grafico dos conteudos mais consumidos
  output$graphMostView <- renderPlot({ggplot(mostViewQuant) +
      aes(x = Conteudo_mais_consumido, fill = Conteudo_mais_consumido, weight = quantMost) +
      geom_bar() +
      scale_fill_hue() +
      labs(x = "Conteudos", y = "Quantidade", title = "Conteudos Mais Consumidos", fill = "Conteudos") +
      theme_minimal()
  })
  
  #Grafico dos conteudos menos consumidos
  output$graphLessView <- renderPlot({ggplot(lessViewQuant) +
      aes(x = Conteudo_menos_consumido, fill = Conteudo_menos_consumido, weight = quantLess) +
      geom_bar() +
      scale_fill_hue() +
      labs(x = "Conteudos", y = "Quantidade", title = "Conteudos Menos Consumidos", fill = "Conteudos") +
      theme_minimal()
  })
  
  #Grafico dos conteudos mais consumidos por sexo
  renderLineChart(div_id = "graphMostViewGender", theme = "shine", data = dataMostView)
  
  #Grafico dos conteudos menos consumidos por sexo
  renderLineChart(div_id = "graphLessViewGender", theme = "shine", data = dataLessView)
  
  #Tabela com os dados
  output$tabela <- DT::renderDataTable({
    DT::datatable(data, options = list(lengthMenu = c(5, 30, 50), pageLength = 15))
  })
  
}

shinyApp(ui1, server)