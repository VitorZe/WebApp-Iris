library(shiny)
library(ggplot2)

setwd("~/1.Progreams/RStudio/Aprendes Shiny/Iris")
Iris <- iris
Dici <- c(
  "Sépala (Comprimento)",
  "Sépala (Largura)",
  "Pétala (Comprimento)",
  "Pétala (Largura)",
  "Espécie"
)
for (x in seq(1:5)){
  colnames(Iris)[x] <- Dici[x]
}

#front end
## O que será mostrado
#fluidPage traz uma pagina com tamanho adaptavel
ui <- navbarPage(title = "Explorando o Dataset iris",
  
  tabPanel(
    title = "Introdução ao site",
    sidebarPanel(
      h4("Este site traz consigo algumas abas, localizadas no canto superior.
       \tcada uma com um titulo que descreve o seu conteudo"),
      h4("Teremos um conjunto de dados que será analisado, pode ir para a
         próxima página para entender um pouco mais!")
    ),
    mainPanel(
      tags$img(src = "AquiO.png")  
    )
    ),               
                                
  tabPanel(
    align = "justify",
    title = "Do que se trata?",
        sidebarPanel(
          h4("Envolve um conjunto de dados abertos sobre uma classe de flores (iris)"),
          tags$a(href = "https://doi.org/10.24432/C56C76", "[Link para o dataset]"),
          h4("Temos dados referentes a especie, tamanho da petala  e tamanho da sépala,
             cada observação do conjunto se refere a uma planta"),
          h5("Quais características são específicas de cada espécie?"),
          h5("Da pra prever qual a classe unicamente com os outros valores?")
          
        ),   
          mainPanel(
            tags$img(src = "iris_flowers.png")
          ),
          
           ),
  
  tabPanel(title = "Banco de dados",
           tableOutput("dados")),
  
  tabPanel(title = "Análises univariadas",
               sidebarLayout(
                     sidebarPanel(
                       h3("Distribuição"),
                       h4("Histograma"),
                       varSelectInput(
                         "idSelect",
                         "Variável",
                         dplyr::select_if(Iris, is.numeric)
                       ),
                       sliderInput("arrasta", "Numero de barras", 
                                   min = 0, max = 100, value = 30, step = 1),           
                       ),
                     mainPanel(
                      titlePanel("Distribuição das variaveis"),
                       plotOutput("graficoDist"),
                       h4("Normalidade"), 
                       textOutput("idNormalidade")
                      )
                 )
           ),
  tabPanel(title = "Analises multivariadas",
           sidebarLayout(
             sidebarPanel(
               h3("Scatter e Regressão"),
               varSelectInput(
                 "idSelect1",
                 "Variável X",
                 dplyr::select_if(Iris, is.numeric)
               ),
               varSelectInput(
                 "idSelect2",
                 "Variável Y",
                 dplyr::select_if(Iris, is.numeric)
               )
             ),
             mainPanel(
               titlePanel("Relação entre variáveis numéricas"),
               plotOutput("graficoCorr")
             )
           )
    
  )
)
  
  
#backend
# O que

#input - dados do usuario | output - saidas processadas pelo bagui
server <- function(input, output){
    
    output$dados <- renderTable({Iris})
    
    output$graficoDist <- renderPlot({ggplot(Iris) + 
                                  geom_histogram(aes(x = !!input$idSelect,
                                                     fill = Espécie),
                                                 bins = input$arrasta,
                                                 alpha = .8,
                                                 color = "black") +
                                  theme_minimal()
    })
    
    output$idNormalidade <- renderText({
      print(Iris$idSelect)
                   })
    
    output$graficoCorr <- renderPlot({ggplot(Iris) +
                                             geom_point(aes(
                                               x = !!input$idSelect1,
                                               y = !!input$idSelect2,
                                               color = Espécie)) +
                                              geom_smooth(aes(
                                                x = !!input$idSelect1,
                                                y = !!input$idSelect2),
                                                method = lm) +
                                              theme_minimal()
      
    })

}


shinyApp(ui, server)

