library(shinyBS)
library(DT)
library(data.table)


searchbox <- function(env_serv) with(env_serv, local({
  
  # SEARCHBOX
  
  observeEvent(input$go_search,{
    session$sendCustomMessage("new-page",list(
      HTML(
        
        div(
          title = tags$p(style = "color: orange; font-size: 25px; text-align: center;",input$search),
          DT::dataTableOutput("search_results"),
          downloadBttn("download_search_results")
        )
      )
    )
    )
  })
  
  # observeEvent(input$go_search, {
  #   showModal(modalDialog(width="600px",
  #     title = tags$p(style = "color: orange; font-size: 25px; text-align: center;",input$search),
  #     DT::dataTableOutput("search_results"),
  #     downloadBttn("download_search_results"),
  #     size = "l",
  #     easyClose = TRUE,
  #     footer = NULL
  #   ))
  # })
  
  # searchInput <- reactive({
  #   bb[Location==input$search | Activity==input$search | Accession==input$search | Mother==input$search | Father==input$search | lubridate::year(Date)==input$search]
  # })
  
  output$search_results <- DT::renderDataTable({
    result = bb[Location==input$search | Activity==input$search | Accession==input$search | Mother==input$search | Father==input$search | lubridate::year(Date)==input$search]
    result = janitor::remove_empty(result, "cols")
    DT::datatable(result,
                  options = list(searchHighlight = TRUE))
  })


  output$download_search_results <- downloadHandler(
    filename = function(){
      sprintf("%s.csv",input$search)
    },
    content = function(file) {
      fwrite(searchInput(), file, row.names = FALSE)
    }
  )
}))