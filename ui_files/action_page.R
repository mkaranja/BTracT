

actionNeeded <- function()tagList(
  
  dashboardPage(
    dashboardHeader(title="World Population Drill Down",titleWidth =400),
    
    dashboardSidebar(width = 240,
                     br(),
                     br(),
                     br(),
                     selectizeInput("region", 
                                    label = em("Select Region",style="text-align:center;color:#FFA319;font-size:150%"),
                                    c("Arusha","Sendusu"))
    ),
    
    
    dashboardBody(   
      conditionalPanel(
        condition = "output.condition1 == 1",
        
        tags$h1("World Population by Region in 2015",style="text-align:center;color:blue;font-size:200%"),
        
        tags$p("Click On Any Region To Get The Treemap Of That Region",style="text-align:center;color:purple"),
        plotOutput("threemap_population",height="600px",
                   click="click_treemap")
      ),
      
      conditionalPanel(
        condition = "output.condition1 == 0",
        br(),
        br(),
        tableOutput("population_time_series"),
        uiOutput("zoomout")
      )
    )
  )
)