# Data Visualization
# Interactivity 
# Author: Luc??a Delgado

#This code generates an app to visualize data from a Mexican Health Survey on 
#perceptions of Sweetened Beverages Consumption.

#note: save as app.r

library(shiny)

ui <- fluidPage(
  
    tags$h1("Nutritional Perceptions"),
    tags$h2("Consumption of Sweetened Beverages in Mexico"),
    tags$h4("Data from the Mexican National Health Survey ENSANUT 2016"),
                
                # *Input() functions: 
                
                wellPanel(
                sliderInput(inputId = "age",
                           label = "Select age range",
                           value = c(20,60), min = 20, max = 60)),
    
                
                # *Output() functions
                
                tabsetPanel(tabPanel("Consumption Frequency", 
                                     tags$h3("How often do you drink sweetened beverages?"),
                                     plotOutput("plotC")),
                            
                            tabPanel("Capability Perception", 
                                     tags$h3("How capable do you feel about consuming one or less \n
                                             glasses of Sugar Sweetened Beverages per week?"),
                                     plotOutput("plotB")), 
                            tabPanel("Benefits", 
                                     tags$h3("Which of the following benefits of eating 
                                             healthy \ndo you consider more important?"),
                                     plotOutput("plotA"))
                            ))

server <- function(input, output) {
  
  #DATA
  library(haven)
  library(data.table)
  library(ggplot2)
  library(tidyverse)
  library(knitr)
  library(dplyr)
  library(grid)
  
  #setwd("~/Documents/1. Harris/6 Quarter/Data Viz/Interactivity/")
  DT <- read_csv("perc_cvs.csv")
  DT = data.table(DT)
  
  # Plot A
  # Benefits
        DT[, perc113_short := substr(perc113,1,5)]
        DT$perc113 <- recode(DT$perc113_short , 
                             "senti" 
                             = "Mental and fisical wellness",
                             "dismi"
                             = "Reduce medical expenses",
                             "evita"
                             = "Prevent illness",
                             "no sa"
                             = "Do not know",
                             "rendi"
                             = "Improve performance",
                             "no re"
                             = "No answer")
        
          #Filter Age  
          data <- reactive(DT[edad>input$age[1] & edad<input$age[2]])
          #Base Population after filters
          tot_pop=reactive(sum(data()[,"ponde_f"]))
          #Prepare data for Plot A: 
          DT_plot = reactive(data()[,perc:=sum(ponde_f)/tot_pop(), by=perc113])
          DT_plota = reactive(unique(DT_plot()[,.(perc113,perc)]))
        
        #Plot A
        output$plotA <- renderPlot({
          ggplot(DT_plota(), aes(x=perc113)) + 
            geom_bar(stat= "identity", fill="deepskyblue4",aes(y=perc)) +
            labs(
                 #title = "Which of the following benefits of eating healthy \ndo you consider more important?",
                 x = "", 
                 y = "Population Percentage",
                 caption = "") +
            theme(text = element_text(size=20))
          
            })
        
  #Plot B
  #"How capable do you feel about consumption one or less glasses of SSB per week?"
          DT$perc115 <- recode(DT$perc115 ,
                                    "capaz"
                                    = "2. Capable",
                                    "muy capaz"
                                    = "1. Very capable",
                                    "poco capaz"
                                    = "3. A little capable",
                                    "nada capaz"
                                    = "4. Not Capable",
                                    "no sabe"
                                    = "5. Do not know",
                                    "no responde"
                                    = "6. No Answer")
          
          #Filter Age  
          data_b <- reactive(DT[edad>input$age[1] & edad<input$age[2]])
          #Base Population after filters
          tot_pop=reactive(sum(data_b()[,"ponde_f"]))
          #Prepare data for Plot A: 
          DT_plot_b = reactive(data_b()[,perc:=sum(ponde_f)/tot_pop(), by=perc115])
          DT_plotb = reactive(unique(DT_plot_b()[,.(perc115,perc)]))
        
        #Plot
          output$plotB <- renderPlot({
            ggplot(DT_plotb(), aes(x=perc115)) + 
            geom_bar(stat= "identity", fill="deepskyblue4",aes(y=perc)) +
            labs(
                 #title = "How capable do you feel about consuming one or less \n glasses of Sugar Sweetened Beverages per week?",
                 x = "", 
                 y = "Population Percentage",
                 caption = "") +
            theme(text = element_text(size=20))
          })
          
  #Plot C
  #"How often do you drink sweetened beverages?"
          DT$perc131 <- recode(DT$perc131 ,
                                    "varias veces a la semana"
                                    = "2. Several times a week",
                                    "menos de una vez al mes o nunca"
                                    = "4. Less than once a month",
                                    "diariamente"
                                    = "1. Every day",
                                    "una a tres veces por mes"
                                    = "3. One-three per month",
                                    "no sabe"
                                    = "5. Do not know", 
                                    "no responde"
                                    = "6. No Answer")
          #Filter Age  
          data_c <- reactive(DT[edad>input$age[1] & edad<input$age[2]])
          #Base Population after filters
          tot_pop=reactive(sum(data_c()[,"ponde_f"]))
          #Prepare data for Plot A: 
          DT_plot_c = reactive(data_c()[,perc:=sum(ponde_f)/tot_pop(), by=perc131])
          DT_plotc = reactive(unique(DT_plot_c()[,.(perc131,perc)]))
          
          #Plot
          output$plotC <- renderPlot({
            ggplot(DT_plotc(), aes(x=perc131)) + 
              geom_bar(stat= "identity", fill="deepskyblue4",aes(y=perc)) +
              labs(
                   #title = "How often do you drink sweetened beverages?",
                   x = "", 
                   y = "Population Percentage",
                   caption = "") +
              theme(text = element_text(size=20))
          })

}

shinyApp(ui = ui, server = server)


