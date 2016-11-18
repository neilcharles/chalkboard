
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinysky)

shinyUI(fluidPage(
  
  dashboardPage(
    dashboardHeader(disable=TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
  
    # Show a plot of the generated distribution
  fluidRow(
    column(3,
           tabBox(width=NULL,
              tabPanel("Draw",
                box(width=NULL,            
                   tabBox(width=NULL,
                     tabPanel("Add",
                       textInput("uiPlayerName", "Label (Player Name)", value=""),
                       uiOutput("selectEventType"),
                       shiny::actionButton("uiDrawEvent", "Add to Pitch", icon("plus-square")),
                       radioButtons("uiAddEdit", "", c("Add Points"  = "add", "Move Points" = "edit"))
                     ),
                     tabPanel("Animate",
                              uiOutput("selectFrame"),
                              shiny::actionButton("uiAddFrame", "Add Frame", icon("copy")),
                              br(),
                              br(),
                              shiny::actionButton("uiRenderAnimation", "Render Animation", icon("video-camera"))),
                     
                     
                     tabPanel("Lookup",
                              uiOutput("selectTeams"),
                              uiOutput("selectPlayers"),
                              shiny::actionButton("uiAddPlayers", "Add players to pitch", icon("group"))
                     )
                     
                     
                   )),
                shiny::actionButton("uiDeleteFrame", "Delete Final Frame", icon("minus-square"), 
                             style="color: #0d0d0d; background-color: #ff3333; border-color: #0d0d0d"),
                
                shiny::actionButton("uiClear", "Clear Pitch", icon("trash"), 
                              style="color: #0d0d0d; background-color: #ff3333; border-color: #0d0d0d")
               ),
              
              tabPanel("File",
                 box(width=NULL,title="Save and Load",
                   shiny::downloadButton("downloadPitch", "Save Picture"),
                   br(),
                   br(),
                   shiny::downloadButton("downloadData", "Save Data as csv"),
                   br(),
                   br(),
                   shiny::downloadButton("downloadAnimation", "Save Animation"),
                   hr(),
                   fileInput('file1', 'Load data from csv',
                             accept=c('text/csv', 
                                      'text/comma-separated-values,text/plain', 
                                      '.csv'))
                   
                 )
              ),
              
              tabPanel("Format",
                box(width=NULL,       
                   textInput("uiTitle", "Title", value=""),
                   textInput("uiSubtitle", "Subtitle", value="")
                ),
                box(width=NULL,
                   colourInput("colOnBall", "On ball events", "white", palette = "limited", allowedCols = c("white","#ffff66","#6699ff", "#99ff99", "#ff8080")),
                   colourInput("colPlayers", "Own Players", "white", palette = "limited", allowedCols = c("white","#ffff66","#6699ff", "#99ff99", "#ff8080")),
                   colourInput("colOppoPlayers", "Opposition Players", "#6699ff", palette = "limited", allowedCols = c("white","#ffff66","#6699ff", "#99ff99", "#ff8080")),
                   hr(),
                   radioButtons("uiPitchType", "Pitch Type", choices=list("Minimal" = "minimal", "Full" = "full", "Juego de Posicion" = "juego"), selected="Full")
                )
              ),
              
              tabPanel("Help",
               box(title="Instructions",width=NULL,
                   p("Click two points on the pitch to draw a temporary arrow, then specify event details and click 'Add to Pitch'. Your temporary line will be replaced with a chalk one."),
                   p("You can add as many lines as you like."),
                   p("'Player Position' and 'Opponent Position' are represented by single points drawn at the line's origin."),
                   p("Delete a line by double clicking on its origin."),
                   tags$a(href="http://www.hilltop-analytics.com/football/chalkboard-intro/", "Click for more detailed instructions.")
               ),
               box(title="Hints and Tips",width=NULL,
                   p("You don't have to label every mark. If the board is getting cluttered, you can use un-labelled points for opposition players etc."),
                   p("The chalkboard works on mobile too! (Although you might not be able to double click to delete points). Why not try it live from a game?")
               )
                
              )
           )),
    column(8,offset=1,
           tabBox(width = NULL,
             tabPanel(title = "Chalkboard",
                div(style = 'overflow-x: scroll', align = "center",plotOutput("pitchPlot", height=600, width=800, click = "pitch_click",  dblclick = dblclickOpts(id = "pitch_dblclick"))),
                textOutput("chalkboardCaption")
             ),
             tabPanel(title = "Animation",
                      busyIndicator(wait = 1000),
                      
                      div(style = 'overflow-x: scroll', align = "center",imageOutput("renderedPitch", height=600, width=800))      
             )
           )
        )
    )
  )
)
))