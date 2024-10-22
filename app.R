#Desenvolvimento do WebApp
library(shiny)
library(bslib)
#Visualização de dados
library(ggplot2)
library(plotly)

library(dplyr)
#Dataset Iris com I maiusculo, para diferenciar
Iris <- iris
#Dicionario com a tradução das colunas
Dici <- c(
  "Sépala (Comprimento)",
  "Sépala (Largura)",
  "Pétala (Comprimento)",
  "Pétala (Largura)",
  "Espécie"
)

#Loop para substituir cada termo antigo pelo termo novo no dicionario
for (x in seq(1:5)){
  colnames(Iris)[x] <- Dici[x]
}

#frontend
ui <- page_navbar(title = "Explorando o dataset Iris",
 navset_card_underline(
  nav_menu("Menu",
  
   nav_panel(
    title = "Introdução ao site",
    sidebarPanel(
      tags$img(src = "Aquio.png")
      ),
    mainPanel(
        h4("Este site traz consigo algumas abas, localizadas no canto superior.
       Cada uma com um titulo que descreve o seu conteudo"),
      h4("Teremos um conjunto de dados que poderá ser analisado por você, pode ir para a
         próxima página para entender um pouco mais!")
    )
    ),               
                                
   nav_panel(
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
            tags$img(src = "iris_flowers.png"),
            tags$img(src = "iris_flowers2.png")
          ),
          
           ),
  
   nav_panel(title = "Banco de dados",
           tableOutput("dados")),
  
   #3. ANALISES UNIVARIADAS
   nav_panel(title = "Análises univariadas",
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
                       radioButtons("idRadioHist", "Separar por grupo?",
                                    c("Sim", "Não"),
                                    selected = "Não")
                       ),
                     mainPanel(
                      titlePanel("Distribuição das variaveis"),
                       h3("Histograma"),
                       plotOutput("graficoDist")
                      )
                 )
           ),
   #4.ANALISES MULTIVARIADAS
   nav_panel(title = "Analises multivariadas",
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
 )
)

  
#Backend  
server <- function(input, output){
    #Saida da aba "Banco de dados
    output$dados <- renderTable({Iris})
    
    #Aba Analise Univariada
    ##Saida do plot histograma
    output$graficoDist <- renderPlot({
      if (input$idRadioHist == "Sim") {
        ggplot(Iris) + 
          geom_histogram(aes(x = !!input$idSelect),
                         bins = input$arrasta,
                         alpha = .8,, 
                         fill = "steelblue",
                         color = "white") +
          facet_wrap(~ Iris$Espécie) +
          theme_minimal(base_size = 22)
        #Caso escolha "não"
      } else {
        ggplot(Iris) + 
          geom_histogram(aes(x = !!input$idSelect),
                         fill = "steelblue",
                         bins = input$arrasta,
                         alpha = .8,
                         color = "white") +
          ylab("Contágem") +
          theme_minimal(base_size = 22)
      }
    })
    
    #Aba Analise Multivariada
    #Grafico de correlação/scatter
    output$graficoCorr <- renderPlot({
      ggplot(Iris) +
        geom_point(aes(
          x = !!input$idSelect1,
          y = !!input$idSelect2,
          color = Espécie),
          size = 2) +
        geom_smooth(aes(
          x = !!input$idSelect1,
          y = !!input$idSelect2),
          method = lm) +
        theme_minimal(base_size = 22)
    })

}


shinyApp(ui, server)

