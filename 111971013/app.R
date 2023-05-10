

# Note from assistant and Teacher:
#【1】
#ggbiplot is not on CRAN, it is only available on GitHub. It can be installed by following cmmand.

#install.packages('remotes', dependencies = TRUE)
#remotes::install_github("vqv/ggbiplot")

#【2】deploy shinyapp error
#Preparing to deploy application...DONE
#Uploading bundle for application: 9081079...DONE
#Deploying bundle: 7200756 for application: 9081079 ...
#Waiting for task: 1299176728
#  building: Parsing manifest
#  building: Building image: 8559090
#  building: Fetching packages
#  building: Building package: MASS
################################# Begin Task Log ################################ 
################################## End Task Log ################################# 
#Error: Unhandled Exception: Child Task 1299176729 failed: Error building image: Error fetching MASS (7.3-58.4) source. <CRANPackageSource repo='http://cran.rstudio.org'> unable to satisfy package: MASS (7.3-58.4)
#Execution halted
#=============================
# correct answer!
#https://cran.r-project.org/web/packages/MASS/index.html
#install.packages("MASS", version='7.3-60')
# neitzon's answer
#devtools::install_version("MASS", "7.3-60")
#=============================


#【3】another example:
#https://yi-hua.shinyapps.io/hw4_110/



#======================(0)Library======================

library(dplyr)    # alternatively, this also loads %>%
library("ggplot2")
library(shiny)
library(ggbiplot)
library(FactoMineR) #https://www.clres.com/ca/pdepca01a.html   install.packages(c("FactoMineR", "factoextra"))
library(factoextra)
library(corrplot)
library(DT)
library(MASS)
data(iris)


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
        
        column(12, h3("Plot Control Panel"))
    ),

    
    fluidRow(  
        column(4, 
            #helpText("Note: help text isn't a true widget,", 
            #                    "but it provides an easy way to add text to",
            #                    "accompany other widgets."),
                        #id
            radioButtons("height_of_plot", h3("height of plot"),
                        choices = list("300px" = 300, "500px" = 500,
                                       "800px" = 800), selected = 500)        
        
        ),
        column(4, 
                        #id
            radioButtons("label_font_size", h3("font size of label"),
                        choices = list("12px" = 12, "16px" = 16,
                                       "22px" = 22), selected = 16)           
        
        ),
        column(4, 
                        #id
            radioButtons("data_point_size", h3("size of point"),
                        choices = list("geom_size_1" = 1, "geom_size_1.5" = 2.5,
                                       "geom_size_2" = 4), selected = 2.5)           
        
        )           

                                   
        #when id element is not consistent between ui and server, then 
        #damn：Error in if: argument is of length zero
    ),
  
  
    fluidRow(
        # ref: https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/   ---- basic layout and html tag
        sidebarLayout(
            position = "right",
            #<part_1>
            #ref: https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/  ---- Display(input or output) reactive output
            sidebarPanel(
                sliderInput(inputId = "bins2",
                          label = "Number of points:",
                          min = 6,
                          max = nrow(iris),
                          value = 100),                      
                          
                h3("Using in PCA"),
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
                    #【tab_1】
                    tabPanel("PCA", 
                        #【tab_1_1】
                        fluidRow(
                            column(12, h3("Summery of PCA for Iris"),
                            pre(textOutput(outputId = "iris_pca_summary")))
                            
                            ),
                        tabsetPanel(
                            #【tab_1_2】
                            tabPanel("PCA ggbiplot", 
                                plotOutput(outputId = "pca_result_plot"),
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
                                        
                            ),
                            #【tab_1_3】
                            tabPanel("PCA Data Table for Iris", 
                            
                                    fluidRow(
                                            column(12, h3("Rotation Table"),
                                            tableOutput("iris_pca_rotation_table")),                                           
                                        
                                        ),
                                    fluidRow(
                                            column(4, h3("center Table"),
                                            tableOutput("iris_pca_center_table")),
                                            column(4, h3("scale Table"),
                                            tableOutput("iris_pca_scale_table")),   
                                            column(4, h3("sdev Table"),
                                            tableOutput("iris_pca_sdev_table"))
                                        ),             
                                    fluidRow(
                                            column(8, h3("x Table"),
                                            dataTableOutput("iris_pca_x_table")),                                             
                                     
                                        ),
                            ),
                                
                        )
                    ),
                    #【tab_2】
                    tabPanel("CA", 
                        #【tab_2_1】
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
                        #【tab_2_2】
                        pre(textOutput(outputId = "ca_summary_info"))
                    ),
                    #【tab_3】
                    tabPanel("Iris Dataset", 
                        #【tab_3_1】
                        fluidRow(
                            column(12, h3("Summery of Iris Dataset"),
                            pre(textOutput(outputId = "iris_row_data")))
                            
                            ),
                          tabsetPanel(
                            #【tab_3_2】
                            tabPanel("Correlations plot of Iris"
                                , plotOutput("correlation_plot"),
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
                            ),
                            #【tab_3_3】
                            tabPanel("Iris Data Table", dataTableOutput("iris_data_table"))
                          )                                     
                        
                    )
                )
            )
        )
    ),

    hr(),
    #【emailToCarter】
    # https://shiny.rstudio.com/reference/shiny/latest/textoutput
    fluidRow(
        column(6, textInput("comment"
                            , "Welcome to leave the comment to me. Please send to me via eamil :\n 【111971013@nccu.edu.tw】"),
                            verbatimTextOutput("action_to_comment"))
                            
    )
  
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

        generate_ir_pca <- reactive({
            # log transform 
            log.ir <- log(iris[1:input$bins2, 1:4])
            ir.species <- iris[1:input$bins2, 5]
            # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
            ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
            ir.pca
        })  
        generate_ir_ca <- reactive({
            res.ca <- CA(X = iris[1:input$bins2, 1:4], graph = FALSE) 
            res.ca
        })  
                
        
        
        

        # 【final countdown】
        output$remaining_days <- renderText({
            invalidateLater(1000, session)

            paste("Still ", round(difftime("2024-5-25", Sys.time(), units='secs'), 0 )
                , "secs(",  round(difftime("2024-5-25", Sys.time(), units = "days"), 0)
                ,"days) until I graduate on 2024/5/25." )

            #round(difftime("2024-5-25", Sys.time(), units = "days"), 0)
            #round(difftime("2024-5-25", Sys.time(), units='secs')), 'secs')

          
        })
          
        
        #【tab_1_PCA】
        #【tab_1_1】
        output$iris_pca_summary <- renderPrint({

            summary(generate_ir_pca())
        })       
        # 【select x y】
        output$select_input_check <- renderText({
            paste(input$select_x, input$select_y, input$bins2, sep="_")

        })        
        #【tab_1_2】
        output$pca_result_plot <- renderPlot({
            # log transform 
            log.ir <- log(iris[1:input$bins2, 1:4])
            ir.species <- iris[1:input$bins2, 5]
            # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
            ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
            
            #tmp <- generate_ir_pca()
            g <- ggbiplot(ir.pca
                , choices = c(as.numeric(input$select_x), as.numeric(input$select_y))
                , obs.scale = 1, var.scale = 1, groups = ir.species
                , circle = TRUE) + geom_point(aes(colour=ir.species), size = as.numeric(input$data_point_size))
            g <- g + scale_color_discrete(name = '')
            g <- g + theme(legend.direction = 'horizontal', 
                        legend.position = 'top',
                        legend.text = element_text(size=input$label_font_size),
                        legend.title = element_text(size=input$label_font_size,face="bold"),
                        #legend.key.size = unit(5, 'cm'),
                        axis.text=element_text(size=input$label_font_size),
                        axis.title=element_text(size=input$label_font_size,face="bold")) 
            g      

        }, height = function(x) as.numeric(input$height_of_plot))
  
  
        #【tab_1_3】
      
        output$iris_pca_rotation_table <-  renderTable(
        

            data.frame(generate_ir_pca()[2]$rotation)
            
            ,rownames = TRUE
        
        )
        output$iris_pca_center_table <-  renderTable(
        
            data.frame(data = generate_ir_pca()[3]$center),rownames = TRUE
        
        )        
        output$iris_pca_sdev_table <-  renderTable(
        
            data.frame(data = generate_ir_pca()[1]$sdev),rownames = TRUE
        
        )        
        output$iris_pca_scale_table <-  renderTable(
        
            data.frame(data = generate_ir_pca()[4]$scale),rownames = TRUE
        
        )    
        output$iris_pca_x_table <-  renderDataTable(
        
            data.frame(generate_ir_pca()[5]$x),rownames = TRUE
        
        )         
        
            
        #【tab_2_CA】
        #【tab_2_1】
        output$ca_plot <- renderPlot({

            #(1)
            #library(ca)
            #res.ca <- ca(obj=iris[1:100, 1:4], nd=4, graph = TRUE)
            #summary(res.ca, scree = TRUE, rows = TRUE, columns = TRUE)
            
            #(2)#https://www.clres.com/ca/pdepca01a.html   
            tmp <- generate_ir_ca()

            fviz_ca_biplot(tmp
                , labelsize = (as.numeric(input$label_font_size)/2)
                , pointsize = (as.numeric(input$data_point_size)*1.1)
                , invisible ="col", repel = TRUE)#+ theme_minimal() #+ geom_point(size = as.numeric(input$data_point_size))
            #
            #+ geom_point(aes(colour=ir.species), size = as.numeric(input$data_point_size))

        }, height = function(x) as.numeric(input$height_of_plot))
  
        #【tab_2_2】
        output$ca_summary_info <- renderPrint({
            summary(generate_ir_ca())
        })   
  
        #【tab_3_Iris Dataset】
        output$iris_row_data <- renderPrint({
            summary(iris[1:input$bins2, -5])
        }) 
        #【tab_3_2】
        output$correlation_plot <- renderPlot({

            #library(corrplot)
            corrplot(cor(iris[1:input$bins2, -5], method = "pearson"), number.cex = .9, method = "square", 
                     hclust.method = "ward", order = "FPC",
                     type = "full"
                     , cl.cex = (as.numeric(input$data_point_size)*0.6)
                     , tl.cex=(as.numeric(input$data_point_size)*0.6) ,tl.col = "black")                     

        }, height = function(x) as.numeric(input$height_of_plot))        
        
        #【tab_3_3】
        output$iris_data_table <-  renderDataTable(
            iris[1:input$bins2, -5]
        
        )


        #【emailToCarter】
        output$action_to_comment <- renderText({
            paste("Hi Carter:"
                    , ""
                    , sprintf("  %s", input$comment)
                    , ""
                    , "Yours Truly", "Yannan He & Jiaming Chang"
                    , sep = "\n") 

        })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)