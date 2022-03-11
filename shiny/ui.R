#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# https://stackoverflow.com/questions/38616790/how-to-display-many-points-from-plotly-click-in-r-shiny
# https://stackoverflow.com/questions/48887731/highcharter-click-event-to-filter-data-from-graph
# https://plotly-r.com/linking-views-with-shiny.html#shiny-plotly-inputs
# https://talks.cpsievert.me/20191115/#7

library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(shinyBS)
library(DT)
library(plotly)


ui <- navbarPage("Top2000",
                 id = 'tab',
                 theme = shinythemes::shinytheme("flatly"),
                 tabPanel(
                   "Dashboard", id = "main",
                   wellPanel(style = "background-color: #f4f4f4;",
                   #tags$style('#main {
                  #           background-color: #007BA7;}'
                  # ),

                 #tags$head(tags$style(HTML('.navbar-static-top {background-color: #b22000;}',
                 #                           '.navbar-default .navbar-nav>.active>a {background-color: #b22000;}'))),
                 fluidRow(
                   column(width = 3,style = "background-color: #E0E0E0;",
                          
                          #div(style="display:inline-block",
                          actionButton("info", label = "?", style = "float:right; padding:10px;font-size:70%;margin-top:5px;"),
                          tags$p(strong("Filters Section", style =  "font-size:20px;color:#585a58")),
                                 #HTML("Filters Section <a id=\"button\" href=\"#\" class=\"action-button\"><i class=\"fa fa-info-circle\"></i></a>")),
                          chooseSliderSkin("Flat"),
                          setSliderColor(c("SteelBlue","SteelBlue"),c(1,2)),
                          div(style = "margin-top: -5px"),
                          tags$p(strong("Years:", style = "font-size:18px;color:#585a58")),
                          div(style = "margin-top: -13px"),
                          tagList(
                            tags$style(type = 'text/css', '#years_slider .irs-grid-text, .irs-from, .irs-to, .irs-min, .irs-max {font-size: 15px; color:#585a58}'),
                                       #years_slider .irs-from, .irs-to {background:#585a58}'), 
                            div(id = 'years_slider',
                            sliderInput("years", NULL, sep= "",
                                        min = 1999, max = 2021, value = c(1999,2021), width = "100%", step = 1, ticks = FALSE)
                            )
                          ),
                          div(style = "margin-top: -5px"),
                          tags$p(strong("Position:", style = "font-size:18px;color:#585a58")),
                          div(style = "margin-top: -13px"),
                          tagList(
                            tags$style(type = 'text/css', '#top_slider .irs-grid-text {font-size: 15px; color:#585a58}",
                                       #top_slider .irs-grid-pol {background:#585a58}'), 
                            div(id = 'top_slider',
                            sliderInput("top", NULL, sep= "",
                                        min = 1, max = 2000, value = c(1,2000), width = "100%", step = 1, ticks = FALSE)
                            ),
                          ),
                          div(style = "margin-top: 20px"),
                          tags$p(strong("Songs by Times of Occurrence", style = "font-size:18px;color:#585a58")),
                          div(style = "margin-top: -12px"),
                          plotlyOutput("artist_ocurrance_plot", height = 280),
                          br(),
                          tags$p(strong("Songs by Released Decade", style = "font-size:18px;color:#585a58")),
                          div(style = "margin-top: -12px"),
                          plotlyOutput("released_decade",height = 280),
                          br(),
                          ),
                   column(width = 9, offset = 0.8,
                          tabsetPanel(type = "tabs",
                                      tabPanel("Non Audio Features", 
                                               fluidRow(
                                                 column(width = 6,
                                                        tags$p(strong("Songs by Country and Continent", style =  "font-size:18px;color:#585a58")),
                                                        div(style = "margin-top: -12px"),
                                                        plotOutput("country", height = 755),
                                                   
                                                 ),
                                                 column(width = 5,
                                                        tags$p(strong("Songs by Gender", style =  "font-size:18px;color:#585a58")),
                                                        div(style = "margin-top: -12px"),
                                                        plotOutput("gender", height = 150, width = "100%"),
                                                        br(),
                                                        tags$p(strong("Songs by Genre", style = "font-size:18px;color:#585a58")),
                                                        div(style = "margin-top: -10px"),
                                                        plotOutput("genre",  height = 560)
                                                 )
                                               ),

                                      ),
                                      tabPanel("Audio Features", 
                                               div(style = "margin-top: 10px"),
                                               tags$p(strong("Histograms", style =  "font-size:18px;color:#585a58")),
                                               div(style = "margin-top: -10px"),
                                               plotOutput("audioplot", height = 360),
                                               div(style = "margin-top: 5px"),
                                               #br(),
                                               fluidRow(
                                                 column(width = 6,
                                                        tags$p(strong("Correlation Matrix", style =  "font-size:18px;color:#585a58")),
                                                        div(style = "margin-top: -10px"),
                                                        plotOutput("audiocorr", height = 350),
                                                        ),
                                                 column(width = 6,
                                                        tags$head(tags$style("#features{font-size:18px;color:#585a58;font-weight: bold;}")
                                                                  ),
                                                        uiOutput("features"),
                                                        #tags$p(strong(uiOutput("features"), style =  "font-size:18px;color:#585a58")),
                                                        #div(style = "margin-top: 0px"),
                                                        plotOutput("corr_scatter", height = 350)
                                                 )
                                               )

                                               # fluidRow(
                                               #   column(width = 5,
                                               #          plotOutput("audioplot", height = 800)),
                                               #   column(width = 6,
                                               #          plotOutput("audiocorr")
                                               #   )
                                               # )
                                      ),

                          )
                   )
                 )
                 )
                 )
)
           
