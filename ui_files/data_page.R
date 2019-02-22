
dataPage <- function()tagList(
  div(
    column(2, br(),
           selectInput("dataset","Select dataset:", c("Flowering","Crosses","Plantlets","Status","Contamination"), selected = "Crosses"),
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
                                    DT::dataTableOutput("viewdt")
                                ))
                       ), br(), hr(), br()
                  )   
              ),
             tabPanel("Structure",
                      uiOutput("structureOUT")
             )
             #,
              # tabPanel("Visualize",
              #          fluidRow(
              #            highchartOutput("hvisualize",height = "700px")
              #          )
              #   )
             )
    )
  )
)

