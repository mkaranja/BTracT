library(shinyWidgets)
library(summarytools)
library(jsonlite)
library(shinyjs)
library(rpivotTable)

css <- "
#large .selectize-input { line-height: 40px; }
#large .selectize-dropdown { line-height: 30px; }"

dataserver <- function(env_serv) with(env_serv, local({
  
  
  
  output$sidebarControls <- renderUI({
    div(
      conditionalPanel(
        condition = "input.datatabs=='Data Table' || input.datatabs=='Structure'",
        selectInput("dataset","Select dataset:", c("Flowering","Crosses","Plantlets","Status","Contamination"), selected = 'Crosses'),
        verbatimTextOutput('text'),
        conditionalPanel(
          condition = "input.datatabs=='Structure'",
          shinyWidgets::prettyRadioButtons(inputId = "display",label = "Display:", choices = c("str", "summary"),
                                           icon = icon("check"), bigger = F,status = "info")
        )#,
        #uiOutput('showVarsOut')
      ),
      conditionalPanel(
        condition = "input.datatabs=='Summary Table'", br(),br(),
        tags$style(type='text/css', css),
        div(id = "large",
          selectInput('groupby', 'Group by:', c('Location','Crossnumber','Mother','Father', 'FemalePloidyLevel','MalePloidyLevel','Year_of_Pollination','Month_of_Pollination'), 
                    multiple=T, width="100%")
        )
        # 
        # prettyRadioButtons('summaryShow','View', c('Table','Corr'))
        
        # selectInput("activity","Activity:", c("All", unique(cleantable$Activity)), multiple = T),
        # dateRangeInput("summaryDateRange","Pollination Date Range:", format = "yyyy-mm-dd", startview = "year",
        #                start = min(anytime::anydate(na.omit(bananadata$`First Pollination Date`))), 
        #                end = max(anytime::anydate(na.omit(bananadata$`First Pollination Date`))),
        #                min = min(anytime::anydate(na.omit(bananadata$`First Pollination Date`))),
        #                max = max(anytime::anydate(na.omit(bananadata$`First Pollination Date`)))), br(),br()
      )
    )
  })
  
  # output$summaryUI <- renderUI({
  #   div(
  #     if(input$summaryShow=='Table'){
  #       DT::dataTableOutput("summaryDT")
  #     } else if(input$summaryShow=='Corr'){
  #       plotOutput('summaryCorr')
  #     }
  #   )
  # })
  datasetInput <- reactive({
    req(input$dataset)
    
    switch(input$dataset, 
           "Flowering" = flowering,
           "Crosses" = bananadata,
           "Plantlets" = plantlets[,-c("Crossnumber", 'Mother','Father')],
           "Status" = status,
           "Contamination" = contamination)
  })
  
  # output$showVarsOut <- renderUI({
  #   selectInput("showVars", "Select variables to show:",c(names(datasetInput())), multiple = TRUE)
  # })
  
  
  output$structureOUT <- renderUI({
    result = datasetInput()
    # columns = names(result)
    # if(!is.null(input$showVars)){
    #   columns = input$showVars
    # }
    # 
    # if(nrow(result)>0){
    #   result = result[,columns, drop=FALSE]  
    # } else {
    #   result = result[,..columns, drop=FALSE]
    # }
    
    
    #result = datasetInput()[,columns, drop=FALSE] # [,..columns, drop=FALSE]
    # if(!is.null(input$dt_site)){
    #   result = result[Location %in% input$dt_site]
    # }
    result = janitor::remove_empty(result, "cols")
    
    
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
    result = datasetInput()
    #columns = names(result)
    # if(!is.null(input$showVars)){
    #   columns = input$showVars
    # }
    # 
    # if(nrow(result)>0){
    #   result = result[,columns, drop=FALSE]  
    # } else {
    #   result = result[,..columns, drop=FALSE]
    # }
    
    #result = datasetInput()[,columns, drop=FALSE] #[,..columns, drop=FALSE]
    str(result)
  })
  
  ##################################################
  # Data Table TAB
  ##################################################
  viewInput <- reactive({
    result = data.frame(datasetInput())
    columns = colnames(result)
    columns = input$showVars
    if(input$dataset=='Crosses'){
       if(!is.null(input$female_bar_clicked)){
         result = result %>% dplyr::filter(Mother %in% input$female_bar_clicked[1])
       } 
       if(!is.null(input$male_bar_clicked)){
         result = result %>% dplyr::filter(Father %in% input$male_bar_clicked[1])
       }
     }
    
    result = janitor::remove_empty(result, "cols")
    colnames(result) = gsub("[.]"," ", names(result))
    return(result)
  })
  output$viewdt <- DT::renderDT({
    
    DT::datatable(viewInput(), filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })
  
  output$text <- renderPrint({
    input$viewdt_cell_clicked$value
  })
  
  
 
    
  
  downloadView <- reactive({
    result = viewInput()
    # if(!is.null(input$dt_site)){
    #   result = result[Location %in% input$dt_site]
    # }
    # columns = names(result)
    # if(!is.null(input$showVars)){
    #   columns = input$showVars
    # }
    # result = result[,columns, drop=FALSE]
    result = result[input[["viewdt_rows_all"]],]
    
    if(!is.null(input$viewdt_rows_selected)){
      result = result[input$viewdt_rows_selected,]
    }
    
    result = janitor::remove_empty(result, "cols")
    result = result[complete.cases(result$Crossnumber),]
    return(result)
  })
  output$downloadTbl <- downloadHandler(
    filename = function(){paste(input$dataset,'-', Sys.time(), '.csv')},
    content = function(file) {
      write.csv(downloadView(), file, row.names = FALSE) #datasetInput
    }
  )
  ##################################################
  # Summaries
  
  summaryIn <- reactive({
    result = bananadata %>%
      dplyr::select(Location, Crossnumber,Mother, Father, `FemalePloidyLevel`,`MalePloidyLevel`,`First Pollination Date`,`Total Seeds`, `Good Seeds`,
                    `Number of Embryo Rescued`,`Number of Embryo Germinating`) %>%
      dplyr::filter(Mother !='' & Father !='')
    
    result$`Year_of_Pollination` = lubridate::year(result$`First Pollination Date`)
    result$`Month_of_Pollination` = month.abb[lubridate::month(result$`First Pollination Date`)]
    
    if(length(input$groupby)>0){
      result = result %>% 
        dplyr::group_by(.dots = input$groupby)
    } else{
      result = result %>% 
        dplyr::group_by(Crossnumber)
    }
    result = result %>% 
      dplyr::summarise(`Number of Crosses`=n(), `Total Seeds`=sum(na.omit(as.integer(`Total Seeds`))), Good_Seeds = sum(na.omit(as.integer(`Good Seeds`))),
                       `Number of Embryo Rescued`= sum(na.omit(as.integer(`Number of Embryo Rescued`))),
                       `Number of Embryo Germinating`= sum(na.omit(as.integer(`Number of Embryo Germinating`))))
  })
  
  output$summaryDT <- renderDataTable({
    
    DT::datatable(summaryIn(), filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
    
  })
  
  
  summaryDownload <- reactive({
    result = summaryIn()
    result = result[input[["summaryDT_rows_all"]],]
    
    if(!is.null(input$summaryDT_rows_selected)){
      result = result[input$summaryDT_rows_selected,]
    }
    
    result = janitor::remove_empty(result, "cols")
    result = result[complete.cases(result[,1]),]
    return(result)
    
  })
  
  output$downloadSummary <- downloadHandler(
    filename = function(){paste0('BTracT Summary-',input$summaryDateRange[1], '-',input$summaryDateRange[2],'.csv')},
    content = function(file) {
      write.csv(summaryDownload(), file, row.names = FALSE) #datasetInput
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
