library(shiny)
library(rhandsontable)
library(MASS)

shinyUI(fluidPage(titlePanel("Online Laboratory Data Plotter"),
                  
                  tabsetPanel(
                    
                    tabPanel(h3("Scatter"),
                             sidebarLayout(position = "right",
                                           
                                           sidebarPanel(h4("Settings"),
                                                        h4("Graph"),
                                                        fluidRow(
                                                          column(
                                                            textInput("xTitle", h5("x title"), value = "x", width = 150),
                                                            numericInput("xMin", h5("min x"), value = 0, width = 100),
                                                            numericInput("xMax", h5("max x"), value = NA, width = 100),
                                                            width = 6),
                                                          column(
                                                            textInput("yTitle", h5("y title"), value = "y", width = 150),
                                                            numericInput("yMin", h5("min y"), value = 0, width = 100),
                                                            numericInput("yMax", h5("max y"), value = NA, width = 100),
                                                            width = 6)
                                                        ),#end fluidRow
                                                        h4("Table"),
                                                        fluidRow(
                                                          column(
                                                            numericInput("numDecPlaces", h5("decimal places"), value = 1, min = 1, max = 5, width = 100),
                                                            width = 6),
                                                          column(
                                                            checkboxInput("fitting", "linear regression", value = FALSE),
                                                            h5("y = ax + b"),
                                                            tableOutput("fitTable"), 
                                                          width = 6)
                                                        ),#end fluidRow
                                                        hr(),
                                                        checkboxInput("loglog", "show log-log", value = FALSE),
                                                        uiOutput("loglogControl"),
                                                        width = 3),#end sidebarPAnel
                                           mainPanel(
                                             fluidRow(
                                               column(
                                                 rHandsontableOutput('table'),
                                                 downloadButton("downloadData", "Download"),
                                                 width = 4),
                                               column(
                                                 plotOutput("plotAuto", width = 650),#, height = 300),
                                                 uiOutput("loglogPlot"),
                                                 #plotOutput("plotAutoLog", width = 650),#, height = 300),
                                                 width = 8)
                                             ),#end fluidRow
                                             
                                             width = 9)#end mainPanel
                                           
                             )#end sidebarLayout
                             
                    ),#end tabPanel Graph
                    tabPanel(h3("Histogram"),
                             sidebarLayout(position = "right",
                                           
                                           sidebarPanel(h4("Settings"),
                                                        h4("Table"),
                                                        fluidRow(
                                                          column(
                                                            numericInput("numDecPlacesHist", h5("decimal places"), value = 1, min = 1, max = 5, width = 100),
                                                            width = 6),
                                                          column(
                                                            numericInput("numCol", h5("number of columns (max 30)"), value = 1, min = 1, width = 100),
                                                            width = 6)
                                                        ),
                                                        h4("Histogram"),
                                                        uiOutput("select"),
                                                        fluidRow(
                                                          column(
                                                            numericInput("binSize", h5("Bin size"), value = 1, min = 0, width = 100),
                                                            textInput("xTitleH", h5("x title"), value = "x", width = 150),
                                                            numericInput("xMinH", h5("min x"), value = NA, width = 100),
                                                            numericInput("xMaxH", h5("max x"), value = NA, width = 100),
                                                            width = 6),
                                                          column(
                                                            numericInput("dataBound", h5("Bin boundary"), value = 0.5, width = 100),
                                                            textInput("yTitleH", h5("y title"), value = "frequency", width = 150),
                                                            numericInput("yMinH", h5("min y"), value = NA, width = 100),
                                                            numericInput("yMaxH", h5("max y"), value = NA, width = 100),
                                                            width = 6)
                                                        ),#end fluidRow
                                                        hr(),
                                                        checkboxInput("normal", "show normal curve", value = FALSE),
                                                        width = 3),#end sidebarPAnel
                                           
                                           mainPanel(
                                             fluidRow(
                                               column(
                                                 rHandsontableOutput('tableHist'),
                                                 downloadButton("downloadDataHist", "Download"),
                                                 width = 4),
                                               column(
                                                 tableOutput("statTable"),
                                                 tags$a(href="/Data/DataRes.csv","Download results", download= "Results.csv"),
                                                 plotOutput("plotHist", width = 500, height = 500),
                                                 width = 8)
                                             ),#end fluidRow
                                             
                                             width = 9)#end mainPanel
                                           
                             )#end sidebarLayout
                    ),#end TabPanel Histogram
                    tabPanel(h3("Notes"),
                             fluidRow(column(includeHTML("Notes.html"), width = 8))       
                    )#end tabPanel Notes
                  )#end tabset Panel
                  
))#end shinyUI fluidPage