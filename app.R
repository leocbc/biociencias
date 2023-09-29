# Carregue a biblioteca Shiny
library(shiny)
library(ggplot2)
library(hrbrthemes)

# Defina a UI da aplicação Shiny
ui <- 
  
  fluidPage(
  titlePanel("Projeto: Diagnóstico e caracterização das principais problemáticas que estão envolvidas no ensino da Biociência e da Biotecnologia durante a pandemia do COVID-19"),
  
  # Divida o layout 
  fluidRow(
    column(
      width = 8,
      offset = 2,
      h1("Preditor Neural Artificial para avaliação do ensino remoto de Biociências", align = "center")
    ),
  #   column(
  #     width = 2,
  #     img(src = "img/ufra.jpeg", width = "100%")
  #   )
  ),
  hr(),
  h3("Sobre"),
  fluidRow(
    column(
      width = 6,
      p(strong("Objetivos do estudo"), "Obter uma Rede Neural Artificial (RNA) para a predição das variáveis importantes ao processo de ensino e aprendizagem remotos, Diagnosticar e caracterizar as principais problemáticas que estão envolvidas no ensino da Biociência e da Biotecnologia")
    ),
    column(
      width = 6,
      p(strong("Obtenção dos dados"), "A coleta de dados foi realizada mediante questionário online na plataforma Google Formulários. Todos os participantes foram convidados via carta convite por e-mail, com lista de endereços a ser disponibilizada pelos gestores de cada instituição colaboradora da pesquisa.  Com intuito de obter uma abordagem ampla e representativa, as informações sobre sexo, etnia, cor/raça (de acordo com classificação estabelecida pelo Instituto Brasileiro de Geografia e Estatística - IBGE) e classes/grupos sociais foram obtidas unicamente via formulário de pesquisa, não havendo preterição prévia. Não foram feitas distinções sobre orientação sexual e identidade de gênero dos participantes. A análise comparativa entre classes/grupos sociais considerados vulneráveis e não vulneráveis é importante para alcançar os objetivos da pesquisa, tendo em vista que, a princípio, grupos em situação de vulnerabilidade social tem acesso precarizado ao ensino remoto.")
    )
  ),
  hr(),
  fluidRow(
    column(12, sidebarLayout(
      sidebarPanel(
        selectInput("x_var", "Selecione o Eixo X:", choices = colnames(dados_superior_x)),
        selectInput("y_var", "Selecione o Eixo Y:", choices = colnames(dados_superior_x))
      ),
      mainPanel(
        plotOutput("heatmapPlot")
      )
    ))),
  fluidRow(column(6, plotOutput("plot2")),
          column(6, plotOutput("plot3")))
  )


# Defina o servidor da aplicação Shiny
server <- function(input, output) {
  # Função para criar os gráficos
  output$heatmapPlot <- renderPlot({
    x <- input$x_var
    y <- input$y_var
    tabela = table(dados_superior_x[,x], dados_superior_x[,y])
    g = ggplot(data.frame(tabela), aes(x = Var1,y = Var2, fill = Freq)) +
      geom_tile(color = "white")+
      scale_fill_gradient(low="gray95", high="tomato") +
#      theme_ipsum() +
      theme(axis.text.x = element_text(angle=30, vjust = .9, hjust = .9))
                                       
    g
    
  })
  
  output$plot2 <- renderPlot({
    x <- input$x_var
    tabela2 = data.frame(table(dados_superior_x[,x]))
    
    ggplot(tabela2, aes(x=Var1, y=Freq)) + 
      geom_bar(stat = "identity", color="blue", fill=rgb(0.1,0.4,0.5,0.7) ) +
      coord_flip()
  })
  
  output$plot3 <- renderPlot({
    y <- input$y_var
    tabela3 = data.frame(table(dados_superior_x[,y]))
    
    ggplot(tabela3, aes(x=Var1, y=Freq)) + 
      geom_bar(stat = "identity", color="red", fill=rgb(0.1,0.4,0.5,0.7) ) +
      coord_flip()
  })
  
}

# Crie a aplicação Shiny
shinyApp(ui = ui, server = server)
