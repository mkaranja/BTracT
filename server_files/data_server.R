library(shinyWidgets)
library(summarytools)
library(jsonlite)
library(shinyjs)

dataserver <- function(env_serv) with(env_serv, local({
  
  datasetInput <- reactive({
    req(input$dataset)
    
    switch(input$dataset, 
           "Flowering" = flowering,
           "Crosses" = bananadata,
           "Plantlets" = plantlets,
           "Status" = status,
           "Contamination" = contamination)
  })
  
  output$sidebarControls <- renderUI({
      if(input$datatabs == "Structure"){
          shinyWidgets::prettyRadioButtons(inputId = "display",label = "Display:", choices = c("str", "summary"),
                                           icon = icon("check"), bigger = F,status = "info")
        } else if(input$datatabs =="Data Table"){
          selectInput("showVars", "Select variables to show:",names(datasetInput()), multiple = TRUE)
          
        }
      
    
    # else if(input$datatabs =="Visualize"){
    #     div(
    #       br(),
    #       selectInput("visVars","Select variable", c(names(full_cleantable)[-c(1:3,12)]), size = "7%", selectize = F), br(),
    #       dateRangeInput("visdateRange","Date Range:", min = datemin, max=datemax, start=datemin, end=datemax), br(),
    #       prettySwitch(inputId = "grpBySite", label = "Group by Site", status = "success", value = T), br(),
    #       prettyRadioButtons(inputId = "type",label = "Plot Type:", choices = c("line", "column", "bar", "spline"),icon = icon("check"), bigger = F,status = "info"), br(),
    #       prettyRadioButtons(inputId = "stacked",label = "Stacked:", choices = c(FALSE, "normal", "percent"), icon = icon("check"), bigger = F,status = "info")
    #   )
    #   }
    
  })
  
  
  output$structureOUT <- renderUI({
    columns = names(datasetInput())
    if(!is.null(input$showVars)){
      columns = input$showVars
    }
    result = datasetInput()[,..columns, drop=FALSE]
    
    div(
      conditionalPanel(
        condition = "input.display=='str'",
          div(
              fluidRow(br(),
               tags$p(style = "color: #FF8C00; font-size: 18px;","Data structure"),
               verbatimTextOutput("dataStr"), br()
              ),
              fluidRow(
                conditionalPanel(condition = "input.dataset=='Flowering'", includeMarkdown('www/variables/flowering.md')),
                conditionalPanel(condition = "input.dataset=='Crosses'", includeMarkdown('www/variables/bananadata.md')),
                conditionalPanel(condition = "input.dataset=='Plantlets'", includeMarkdown('www/variables/plantlets.md')),
                conditionalPanel(condition = "input.dataset=='Status'", includeMarkdown('www/variables/status.md')),
                conditionalPanel(condition = "input.dataset=='Contamination'", includeMarkdown('www/variables/contamination.md')),br(),hr(), br()
              ))
      ),
    conditionalPanel(
      condition = "input.display=='summary'",
      fluidRow(br(),
         tags$p(style = "color: #FF8C00; font-size: 18px;","Data summary"), 
         print(dfSummary(result, graph.magnif = 1.0), 
               method = 'render',
               omit.headings = TRUE,
               bootstrap.css = FALSE),
               br(),hr(), br()
          )
       )
    )
  })
  
  output$dataStr <- renderPrint({
    columns = names(datasetInput())
    if(!is.null(input$showVars)){
      columns = input$showVars
    }
    result = datasetInput()[,..columns, drop=FALSE]
    str(result)
    })
  
  ##################################################
  # Data Table TAB
  ##################################################
  
  output$viewdt <- DT::renderDataTable({
    columns = names(datasetInput())
    if(!is.null(input$showVars)){
      columns = input$showVars
    }
    result = datasetInput()[,..columns, drop=FALSE]
    
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
   })
   
  
  downloadView <- reactive({
    columns = names(datasetInput())
    if(!is.null(input$showVars)){
      columns = input$showVars
    }
    result = datasetInput()[,..columns, drop=FALSE]
    
    if(!is.null(input$viewdt_rows_selected)){
      result = result[input$viewdt_rows_selected,]
    }
    return(result)
  })
  output$downloadTbl <- downloadHandler(
    filename = function(){paste(input$dataset,'-', Sys.time(), '.csv')},
    content = function(file) {
      write.csv(downloadView(), file, row.names = FALSE) #datasetInput
    }
  )
  
  
  ##################################################
  # PIVOT TAB
  ##################################################
  
  ##################################################
  # VISUALIZE TAB
  ##################################################
  
  output$hvisualize <- renderHighchart({
    
    result = full_cleantable %>%
      dplyr::mutate(year = lubridate::year(Date), month = lubridate::month(Date), day = lubridate::day(Date))
    if((input$visdateRange[2] - input$visdateRange[1]) < 31){
      result = result[Date %between% c(input$visdateRange[1], input$visdateRange[2])]
      result = result %>%
        dplyr::group_by(day) %>% 
        dplyr::tally()
      grp = result$day
      
    } else if((lubridate::year(input$visdateRange[2]) == lubridate::year(input$visdateRange[1]))){
      result <- setDT(result)[lubridate::year(Date) == lubridate::year(input$visdateRange[1]) &
                                lubridate::month(Date) %between% c(lubridate::month(input$visdateRange[1]),lubridate::month(input$visdateRange[2]))]
      result = result[Date %between% c(input$visdateRange[1], input$visdateRange[2])]
      result = result %>%
        dplyr::group_by(month) %>% 
        dplyr::tally()
      grp = result$month
      
    }  else if((lubridate::year(input$visdateRange[2]) != lubridate::year(input$visdateRange[1]))){ 
      result <- setDT(result)[lubridate::year(Date) %between% c(lubridate::year(input$visdateRange[1]), lubridate::year(input$visdateRange[2]))]
      result = result[Date %between% c(input$visdateRange[1], input$visdateRange[2])]
      result = result %>%
        dplyr::group_by(year) %>% 
        dplyr::tally()
      grp = result$year
    }
    
    hc <- highchart() %>%
      hc_chart(type = input$type) %>%
      hc_xAxis(categories = grp) %>%
      hc_add_series(name = input$visVars, data = result$n) %>%
      hc_exporting(enabled = TRUE)
    
    if (input$stacked != FALSE)
      hc <- hc %>% hc_plotOptions(series = list(stacking = input$stacked))
    hc
    
  })
  
  ##################################################
  # EXPLORE TAB
  ##################################################
}))
