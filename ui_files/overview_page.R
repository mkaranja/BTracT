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
      column(3, selectInput("site", "Select Site:", c("All","Arusha","Sendusu"), selected = "All", multiple = F)),
      column(3, dateRangeInput("dateRange", "Select Date Range", min = datemin, max=datemax, start=datemin, end=datemax)),
      column(1,
             conditionalPanel(condition = "input.overview_site != 'All'",
               shinyjs::useShinyjs(), br(),br(),
               actionBttn('reset_overview',"Reset", style = "jelly", size="xs",color = "primary")
             )
      )
    )
  ), br(),
  fluidRow(
    
    tags$style("#n_crosses .small-box, #n_totalseeds .small-box, #n_rescued .small-box, #n_8weeks .small-box, #n_openfield_plantlets .small-box {cursor: pointer;}"),
    
    valueBoxOutput("n_crosses", width = 2),tags$style("#n_crosses"), # tags$style("#n_crosses {width:220px;}")
    bsModal("mod_crosses",tags$p(style = "color: #FF8C00; font-size: 18px; text-align: center;","Banana crosses"),"btn",size = "large",
            column(12, offset = 10, 
                   downloadBttn("download_crosses",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_crosses"), br(), br()
    ),
    
    valueBoxOutput("n_bunches", width = 2), tags$style("#n_bunches"),
    bsModal("modal_bunches", tags$p(style = "color: #FF8C00; font-size: 18px; text-align: center;","Banana bunches"),"btn", size = "large",
            column(12, offset = 10,
                   downloadBttn("download_bunches",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_bunches"), br(), br()
    ),
    
    valueBoxOutput("n_totalseeds", width = 2), tags$style("#n_totalseeds"),
    bsModal("modal_totalseeds", tags$p(style = "color: #FF8C00; font-size: 18px; text-align: center;","Total Number of seeds"), "btn", size = "large",
            column(12, offset = 10,
                   downloadBttn("download_totalseeds",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_totalseeds"), br(), br()
    ),
    
    valueBoxOutput("n_rescued", width = 2), tags$style("#n_rescued"), 
    bsModal("modal_rescued", tags$p(style = "color: #FF8C00; font-size: 18px; text-align: center;","Number of embryo rescued"), "btn",size = "large",
            column(12, offset = 10,
                   downloadBttn("download_rescued",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_rescued"), br(), br()
    ),
    
    valueBoxOutput("n_8weeks", width = 2), tags$style("#n_8weeks"),
    bsModal("modal_8weeks", tags$p(style = "color: #FF8C00; font-size: 18px; text-align: center;","Active after 8 weeks"), "btn",size = "large",
            column(12, offset = 10,
                   downloadBttn("download_8weeks",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_8weeks"), br(), br()
    ),
    
    valueBoxOutput("n_openfield_plantlets", width = 2), tags$style("#n_openfield_plantlets"),
    bsModal("modal_openfield", tags$p(style = "color: #FF8C00; font-size: 18px; text-align: center;","Plantlets in the openfield"), "btn",size = "large",
            column(12, offset = 10,
                   downloadBttn("download_openfield",style = "jelly",  size = "sm")),
            DT::dataTableOutput("list_openfield"), br(), br()
    )
    
  ),
  fluidRow(
    ######### CROSSES BAR PLOT
    column(width = 5,
           tags$p(style = "color: #FF8C00; font-size: 18px; text-align: center;","Total number of crosses"),
           tags$div(align = 'right',
                    uiOutput("totalcrosses")
           ),
           uiOutput("crossesOUT")
          
    ),
    # column(5, offset = 1,
    #        # column(4, offset = 3,
    #        # shinyWidgets::prettyRadioButtons(inputId = "parent",label = "Parents:", choices = c("Female", "Male"),
    #        #                                  icon = icon("check"), bigger = F,status = "info", inline = T)
    #        # ),
    #        loadEChartsLibrary(),
    #        tags$div(id="test", style="width:100%;height:500px;"),  # Specify the div for the chart. Can also be considered as a space holder
    #        deliverChart(div_id = "parents")  # Deliver the plotting
    #        )
    #   )
    # 
     column(width = 3, offset = 1,
            tags$p(style = "color: #FF8C00; font-size: 18px; text-align: center;","Unique females used in pollination"),br(),
            tags$div(align = 'right',
                     uiOutput("totalFemales")
            ),
            highchartOutput("mother", height = 500)
     ),
     column(width = 3,
            tags$p(style = "color: #FF8C00; font-size: 18px; text-align: center;","Unique males used in pollination"), br(),
            tags$div(align = 'right',
                     uiOutput("totalMales")
            ),
            highchartOutput("father", height = 500)
     )
  )
)