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
library(FactoMineR)
library(factoextra)
data(iris)

#res.ca <- ca(obj=iris[1:100, 1:4], nd=4, graph = TRUE)
#res.ca$colnames
# summary(res.ca, scree = TRUE, rows = TRUE, columns = TRUE)


#https://www.clres.com/ca/pdepca01a.html   install.packages(c("FactoMineR", "factoextra"))



#CA(X = iris[1:input$n, 1:4], graph = FALSE) 
#CA(X = iris[1:100, 1:4], graph = FALSE) 
#> res.ca
#**Results of the Correspondence Analysis (CA)**
#The row variable has  100  categories; the column variable has 4 categories
#The chi square of independence between the two variables is equal to 84.55842 (p-value =  1 ).
#*The results are available in the following objects:
#
#   name              description
#1  "$eig"            "eigenvalues"
#2  "$col"            "results for the columns"
#3  "$col$coord"      "coord. for the columns"
#4  "$col$cos2"       "cos2 for the columns"
#5  "$col$contrib"    "contributions of the columns"
#6  "$row"            "results for the rows"
#7  "$row$coord"      "coord. for the rows"
#8  "$row$cos2"       "cos2 for the rows"
#9  "$row$contrib"    "contributions of the rows"
#10 "$call"           "summary called parameters"
#11 "$call$marge.col" "weights of the columns"
#12 "$call$marge.row" "weights of the rows"



# fviz_ca_biplot(res.ca, repel = TRUE)





#-------------------------------------------------

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
    #======================(1)Title======================
    # App title
    fluidRow(
        column(6, titlePanel("楊昇豐_111971013_資科專一")),
        # ref: runExample("11_timer")
        column(6, h3(textOutput("remaining_days")))
    ),
  
  
    hr(),


    #======================(2)Content======================
    # ref: https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/   ---- Add control widgets
    fluidRow(  
        helpText("Note: help text isn't a true widget,", 
                            "but it provides an easy way to add text to",
                            "accompany other widgets."),
                    #id
        radioButtons("height", h3("height"),
                    choices = list("300px" = 300, "500px" = 500,
                                   "800px" = 800), selected = 500)
    ),
  
  
    fluidRow(
        # ref: https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/   ---- basic layout and html tag
        sidebarLayout(
            position = "right",
            #<part_1>
            #ref: https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/  ---- Display(input or output) reactive output
            sidebarPanel(

                h3("choose how many input to do PCA:"),
                #sliderInput(inputId = "bins",
                #            label = "Number of points:",
                #            min = 30,
                #            max = 50,
                #            value = 30),
                sliderInput(inputId = "bins2",
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
                # ============================
                # Output function	Creates:
                # dataTableOutput	DataTable
                # htmlOutput	raw HTML
                # imageOutput	image
                # plotOutput	plot
                # tableOutput	table
                # textOutput	text
                # uiOutput	raw HTML
                # verbatimTextOutput	text  
                # ============================
                #h1("First level title", align = "center"),
              
                tabsetPanel(type = "tabs",
                          #tabPanel("distPlot", plotOutput(outputId = "distPlot")),
                          tabPanel("PCA Result: Plot", plotOutput(outputId = "pca_result_plot")),
                          tabPanel("CA", 
                              
                              plotOutput(outputId = "ca_plot"),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),      
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(), 
                              pre(textOutput(outputId = "ca_summary_info"))
                          ),
                          tabPanel("Iris Dataset", pre(textOutput(outputId = "iris_row_data")))
                )
            )
        )
      


    ),

    hr(),

    # https://shiny.rstudio.com/reference/shiny/latest/textoutput
    textInput("comment", "Welcome to leave the comment to me. Please send to me via eamil, 111971013@nccu.edu.tw."),
    verbatimTextOutput("action_to_comment")

  
  
  
)



# ref: https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/ ---- output's lifecyle Use R scripts and data
server <- function(input, output, session) {
  
        #=============================================
        # render function	creates:
        # renderDataTable	DataTable
        # renderImage	images (saved as a link to a source file)
        # renderPlot	plots
        # renderPrint	any printed output
        # renderTable	data frame, matrix, other table like structures
        # renderText	character strings
        # renderUI	a Shiny tag object or HTML  
        #=============================================

        #output$distPlot <- renderPlot({
        #  
        #  x    <- faithful$waiting
        #  bins <- seq(min(x), max(x), length.out = input$bins + 1)
        #  
        #  hist(x, breaks = bins, col = "#E5E5E5", border = "white",  ##75AADB
        #       xlab = "Waiting time to next eruption (in mins)",
        #       main = "Histogram of waiting times")
        #  
        #})
  

        output$pca_result_plot <- renderPlot({
            # log transform 
            log.ir <- log(iris[1:input$bins2, 1:4])
            ir.species <- iris[1:input$bins2, 5]
            # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
            ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
            
            g <- ggbiplot(ir.pca
                , choices = c(as.numeric(input$select_x), as.numeric(input$select_y))
                , obs.scale = 1, var.scale = 1, groups = ir.species)
            g <- g + scale_color_discrete(name = '')
            g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')      
            g      

        }, height = function(x) as.numeric(input$height))
  
  
  
        output$ca_plot <- renderPlot({

            #library(ca)
            #res.ca <- ca(obj=iris[1:100, 1:4], nd=4, graph = TRUE)
            res.ca <- CA(X = iris[1:input$bins2, 1:4], graph = FALSE) 

            fviz_ca_biplot(res.ca, repel = TRUE)


        }, height = function(x) as.numeric(input$height))
  
  
        output$iris_row_data <- renderText({

        "654654"
        }) 
  
        output$ca_summary_info <- renderPrint({


            res.ca <- CA(X = iris[1:input$bins2, 1:4], graph = FALSE)

            summary(res.ca)


        }) 


  
  
  
  
        output$remaining_days <- renderText({
            invalidateLater(1000, session)

            paste("Still ", round(difftime("2024-5-25", Sys.time(), units='secs'), 0 )
                , "secs(",  round(difftime("2024-5-25", Sys.time(), units = "days"), 0),"days) until I graduate." )

            #round(difftime("2024-5-25", Sys.time(), units = "days"), 0)
            #round(difftime("2024-5-25", Sys.time(), units='secs')), 'secs')

          
        })
  
  
        output$action_to_comment <- renderText({
            paste("Hi Carter:"
                    , ""
                    , sprintf("%s", input$comment)  
                    , ""
                    , "Yours Truly", "Yannan He & Jiaming Chang"
                    , sep = "\n") 

        })
  
  
    output$select_input_check <- renderText({
        paste(input$select_x, input$select_y, input$bins2, sep="_")

    })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)