source("tissue_culture/ui_page.R")

tissue_culture_server <- function(env_serv) with(env_serv, local({
  output$newWindowContent <- renderUI({
    ui_page()
    })
  
})
)