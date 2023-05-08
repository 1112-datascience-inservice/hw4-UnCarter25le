#ggbiplot is not on CRAN, it is only available on GitHub. It can be installed by following cmmand.

#install.packages('remotes', dependencies = TRUE)
#remotes::install_github("vqv/ggbiplot")





#---------------------------

#Base Task (80 pts)
#[20 pts] Basic information: name、department、student number
#[30 pts] Make a shiny interactive web site to show PCA analysis (as the following picture) for the iris data such that users can specify which component to show (i.e., PC1, PC2, PC3 ...)
#[30 pts] Make a shiny interactive web site to show correspondence analysis (CA) analysis for the iris data


#---------------------------

library(dplyr)    # alternatively, this also loads %>%
library("ggplot2")
library(shiny)
library(ggbiplot)
data(iris)


#-------------------------------------------------

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  #======================(1)Title======================
  # App title
  titlePanel("楊昇豐_111971013_資科專一"),
  # ref: runExample("11_timer")
  h3(textOutput("remaining_days")),
  hr(),


  #======================(2)Content======================
  # ref: https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/   ---- Add control widgets
  fluidRow(
    #column(3,
    #       radioButtons("radio", h3("Radio buttons"),
    #                    choices = list("Choice 1" = 1, "Choice 2" = 2,
    #                                   "Choice 3" = 3),selected = 1)),
    #
    #column(3,
    #       selectInput("select", h3("Select box"), 
    #                   choices = list("Choice 1" = 1, "Choice 2" = 2,
    #                                  "Choice 3" = 3), selected = 1)),
    #
    #column(3, 
    #       sliderInput("slider1", h3("Sliders"),
    #                   min = 0, max = 100, value = 50),
    #       sliderInput("slider2", "",
    #                   min = 0, max = 100, value = c(25, 75))
    #),
    #
    #column(3, 
    #       textInput("text", h3("Text input"), 
    #                 value = "Enter text..."))   
    helpText("Note: help text isn't a true widget,", 
                        "but it provides an easy way to add text to",
                        "accompany other widgets."),    
    numericInput("height", "height", min = 300, max = 700, value = 500)
  ),
  
  
  fluidRow(
      # ref: https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/   ---- basic layout and html tag
      
      sidebarLayout(
        position = "right",
        
        #<part_1>
        #ref: https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/  ---- Display(input or output) reactive output
        sidebarPanel(
         
          h3("choose how many input to do PCA:"),
          sliderInput(inputId = "bins",
                      label = "Number of points:",
                      min = 30,
                      max = 50,
                      value = 30),
          sliderInput(inputId = "bin2",
                      label = "Number of points:",
                      min = 6,
                      max = nrow(iris),
                      value = 100),                      
                      
          h3("choose what you wnt to see on PCA Result : Plot"),
          selectInput(inputId = "select_x", h4("X Variable"), 
                                 choices = list("PC1" = 1, "PC2" = 2,
                                                "PC3" = 3, "PC4" = 4), selected = 1),
          selectInput(inputId = "select_y", h4("Y Variable"), 
                                 choices = list("PC1" = 1, "PC2" = 2,
                                                "PC3" = 3, "PC4" = 4), selected = 2),
                                                
          verbatimTextOutput("select_input_check")
                                                
                                                
          
        ),
        #<part_2>
        mainPanel(
          # Output function	Creates:
          # dataTableOutput	DataTable
          # htmlOutput	raw HTML
          # imageOutput	image
          # plotOutput	plot
          # tableOutput	table
          # textOutput	text
          # uiOutput	raw HTML
          # verbatimTextOutput	text  

          #h1("First level title", align = "center"),
          
          tabsetPanel(type = "tabs",
                      tabPanel("distPlot", plotOutput(outputId = "distPlot")),
                      tabPanel("PCA Result: Plot", plotOutput(outputId = "pcaPlot2")),
                      tabPanel("Table", pre(textOutput(outputId = "pcaPlot3")))
          ),
        

          
          
          
        )
      ),
  ),

  hr(),
  
  # https://shiny.rstudio.com/reference/shiny/latest/textoutput
  textInput("comment", "Welcome to leave the comment to me. Please send to me via eamil, 111971013@nccu.edu.tw."),
  verbatimTextOutput("action_to_comment")
  
  
  
  
)



# ref: https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/ ---- output's lifecyle Use R scripts and data
server <- function(input, output, session) {
  
  # render function	creates:
  # renderDataTable	DataTable
  # renderImage	images (saved as a link to a source file)
  # renderPlot	plots
  # renderPrint	any printed output
  # renderTable	data frame, matrix, other table like structures
  # renderText	character strings
  # renderUI	a Shiny tag object or HTML  
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#E5E5E5", border = "white",  ##75AADB
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
  
  numbers <- reactive({
    validate(
      need(is.numeric(input$height), "Please input a number")
    )
  })  
  
  output$pcaPlot2 <- renderPlot({
    

    # log transform 
    log.ir <- log(iris[1:input$bin2, 1:4])
    ir.species <- iris[1:input$bin2, 5]
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    
    g <- ggbiplot(ir.pca, choices = c(as.numeric(input$select_x), as.numeric(input$select_y)), obs.scale = 1, var.scale = 1, groups = ir.species)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')      
    g
  }, height = function(x) input$height)
  
  output$pcaPlot3 <- renderText({
    

    #summary(iris)
    "654654"
  }) 

  
  
  
  
  output$remaining_days <- renderText({
    #invalidateLater(1000, session)
    paste("Still ", round(difftime("2024-5-25", Sys.time(), units = "days"), 0), "days until I graduate." )
  
      
  })
  
  
  output$action_to_comment <- renderText({
    paste("Hi Carter:", "", sprintf("%s", input$comment)  , "", "Yours Truly", "Yannan He & Jiaming Chang", sep = "\n") 
    
  })
  
  output$select_input_check <- renderText({
    paste(input$select_x, input$select_y, sep="_")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
