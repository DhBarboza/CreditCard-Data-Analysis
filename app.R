library(shiny)
library(plyr)
library(dplyr)
library(shinythemes)

# Set your Environment:
# Before running the code put the path of the folder with the files:
setwd("C:/Users/Douglas Henrique/Desktop/Github/CreditCard-Data-Analysis")

# Load the "'.RData'" object (contains the script history: analysis-creditcard):
load("C:/Users/Douglas Henrique/Desktop/Github/CreditCard-Data-Analysis/.RData")

ui <- fluidPage(theme = shinytheme("cerulean"),
  tagList(
  
  navbarPage(title = "Fraud Detection",
             
    tabPanel("Client",
             
             h2("Consult customer profile"),
             
             hr(),
             
             fluidRow(
               
               column(4, selectInput('dados_corretos',
                                  label="Did the customer fill in all the data correctly?",
                                  choices=c(yes = 0, no = 1), width = NULL)),
               
               column(4, selectInput('cpf_falecida', 
                                     label="Is the CPF of a person already deceased?",
                                     choices=c(yes = 0, no = 1), width = NULL)),
               
               column(4,selectInput('cpf_sujo',
                                    label="Is the CPF dirty in the square?",
                                    choices=c(yes = 0, no = 1), width = NULL))),# </fluidRow>
             
             fluidRow(
               
               column(4,textInput('valor_max', 
                                  label="What is the maximum value of previous purchases?",
                                  width = NULL, placeholder = "0")),
               
               column(4,textInput('dias_ultima',
                                  label="How many days ago was the last purchase?", 
                                  width = NULL, placeholder = "0"))),# </fluidRow>
             
             fluidRow(
               
               column(4,textInput('charge_back_dias',
                                  label="How many chargeback orders in the last year?", 
                                  width = NULL, placeholder = "0" )),
               
               column(4, textInput('valor',
                                   label="What is the purchase price?",
                                   width = NULL, placeholder = "0"))),
             
             textOutput("text")
             
    )# </tabPanel>
  )# </navbarPage>
))


server <- function(input, output, session) {

  data_client <-reactive({
    data.frame(
      
      preencheu_corretamente=as.numeric(input$dados_corretos),
      
      cpf_falecida=as.numeric(input$cpf_falecida),
      
      cpf_sujo=as.numeric(input$cpf_sujo),
      
      valor_max=as.numeric(input$valor_max),
      
      dias_ultima=as.numeric(input$dias_ultima),
      
      charge_back=as.numeric(input$charge_back_dias),
      
      Valor=as.numeric(input$valor),
      
      Class="NA"
    )
  })
  
  
  probability <- reactive({ model_losgistic %>% predict(data_client(), type = "response")})
  
  output$text <- renderText({ paste0("The probability of being a fraud is: ", probability()) })
}

shinyApp(ui = ui, server = server)
