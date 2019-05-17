
dataPage <- function()tagList(
  div(
    column(2, br(),
           #selectizeInput("dt_site",label = "Site:",choices = c(unique(as.character(cleantable$Location))), multiple = TRUE), br(),
           uiOutput("sidebarControls")
           
    ),
    
    column(9,
           tabsetPanel(
             id = "datatabs",
             
             tabPanel("Data Table",
                      wellPanel(
                        fluidRow(
                          column(12,offset = 11, 
                                 downloadBttn("downloadTbl", size = "xs")), br(), br(),
                          column(12,
                                 div(style = 'overflow-x: scroll',
                                     DT::DTOutput("viewdt")
                                 )
                          )
                        ), br(), hr(), br()
                      )   
             ),
             tabPanel("Summary Table",
                      br(),br(),
                      wellPanel(
                        fluidRow(
                          #tags$p(style = "color: #FF8C00; font-size: 18px; text-align: center;","Table displays total number of accessions grouped by Location, Mother and Father"), br(),
                          column(12, offset = 11,
                                 downloadBttn("downloadSummary","Download", size = "xs")), br(),br(),
                          
                           DT::dataTableOutput("summaryDT")
                        )
                      )
                      
             ),
             tabPanel("Structure",
                      uiOutput("structureOUT")
             )
           )
    )
  )
)

