library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(ECharts2Shiny)
library(r2d3)
css <- "
#large .selectize-input { line-height: 40px; }
#large .selectize-dropdown { line-height: 30px; }"

overviewPage <- function()tagList(
  fluidRow(
    div(id = "overview_controls",
        column(3, selectInput("site", "Select Site:", choices = c("All", as.list(unique(cleantable$Location))), multiple = F)),
        column(2, dateRangeInput("dateRange", "Select Date Range", #start =  min(dates), end = max(dates), min = min(dates),max = max(dates),
                                 startview = "year", separator = " to ", autoclose = TRUE) 
        )),
        column(1, br(),br(),
                shinyjs::useShinyjs(), 
                actionBttn('reset_overview',"Reset", style = "jelly", size="xs",color = "primary")
        )
   
  ), 
  fluidRow(
    
    tags$style("#n_crosses .small-box, #n_totalseeds .small-box, #n_rescued .small-box, #n_germination .small-box, #n_openfield_plantlets .small-box {cursor: pointer;}"),
    
    valueBoxOutput("n_crosses", width = 2),tags$style("#n_crosses"), # tags$style("#n_crosses {width:220px;}")
    bsModal("mod_crosses",tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Banana Crosses"),"btn",size = "large",
            column(12, offset = 10, 
                   downloadBttn("download_crosses",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_crosses"), br(), br()
    ),
    
    valueBoxOutput("n_bunches", width = 2), tags$style("#n_bunches"),
    bsModal("modal_bunches", tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Banana Bunches Harvested"),"btn", size = "large",
            column(12, offset = 10,
                   downloadBttn("download_bunches",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_bunches"), br(), br()
    ),
    
    valueBoxOutput("n_totalseeds", width = 2), tags$style("#n_totalseeds"),
    bsModal("modal_totalseeds", tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Seed Extraction"), "btn", size = "large",
            column(12, offset = 10,
                   downloadBttn("download_totalseeds",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_totalseeds"), br(), br()
    ),
    
    valueBoxOutput("n_rescued", width = 2), tags$style("#n_rescued"), 
    bsModal("modal_rescued", tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Embryo Rescued"), "btn",size = "large",
            column(12, offset = 10,
                   downloadBttn("download_rescued",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_rescued"), br(), br()
    ),
    
    valueBoxOutput("n_germination", width = 2), tags$style("#n_germination"),
    bsModal("modal_germination", tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Embryo Germination"), "btn",size = "large",
            column(12, offset = 10,
                   downloadBttn("download_germination",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_germination"), br(), br()
    ),
    
    valueBoxOutput("n_openfield_plantlets", width = 2), tags$style("#n_openfield_plantlets"),
    bsModal("modal_openfield", tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Plantlets in Openfield"), "btn",size = "large",
            column(12, offset = 10,
                   downloadBttn("download_openfield",style = "jelly",  size = "sm")),
            DT::dataTableOutput("list_openfield"), br(), br()
    )
    
  ), br(),br(),
  fluidRow(
    ######### CROSSES BAR PLOT
    column(width = 6,
           tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Total number of crosses"),
           tags$div(align = 'right',
                    uiOutput("totalcrosses")
           ),
           uiOutput("crossesOUT"), br()
          
          ),
     # column(5, offset = 1,
     #        loadEChartsLibrary(),
     #        verbatimTextOutput('text1'),
     #        tags$div(id="test", style="width:100%;height:400px;"),  # Specify the div for the chart. Can also be considered as a space holder
     #        deliverChart(div_id = "test")  # Deliver the plotting
     #        )
     
      column(width = 3,
             tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Unique females used in pollination"),br(),
             tags$div(align = 'right',
                      uiOutput("totalFemales")
             ),
             uiOutput("motherOUT"), br()
             
      ),
      column(width = 3,
             tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Unique males used in pollination"), br(),
             tags$div(align = 'right',
                      uiOutput("totalMales")
             ),
             uiOutput("fatherOUT"), br()
             
      )
  )
)