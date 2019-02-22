library(shiny)
library(magrittr)
library(collapsibleTree)
library(data.table)
library(shinyWidgets)

statusserver <- function(env_serv) with(env_serv, local({
  
  
  output$status_sidebar <- renderUI({
    if(input$overview_status == "Current details"){
      fluidRow(br(), br(), 
        p("This section shows the current status of accession in the breeding program.",br(), br(),
               "Click on the plot bars to view the detailed information in the data table below", br(),br(), 
                     "To download the plot, right click on it. Select save as picture."), br(), br(), br(),br(), br(), br(),br(), br(), br()
      )
      } else if(input$overview_status == "Data flow"){
        fluidRow(
          br(),
          div(id = "dataflow_filters",
              p("This plot shows movement of accessions from one activity to the next in the breeding program. ", br(),br(), 
                     "Click on any node to see detailed information in the data table below.", br(),br(), 
                     "To download the plot, right click on it. Select save picture as"), br(),br(), 
              
              selectInput("df_site","Select site",c("All",unique(cleantable$Location))), 
              dateRangeInput("df_dateRange","Select date range:",startview = "year",start = datemin, end = datemax, min = datemin, max = datemax),
              conditionalPanel("input.df_site !='All'",
                                  shinyjs::useShinyjs(), br(),
                            actionBttn('reset_dataflow',"Reset", style = "bordered",color = "primary")    
              ), br(), br(), br()
          
              )
        )
      } 
     else if(input$overview_status == "Lost information"){
         div(
           shinyWidgets::prettyRadioButtons(inputId = "lostType",label = "Display: ", choices = c("Crosses","Plantlets"), selected = "Crosses",icon = icon("check"), 
                                            bigger = F,status = "info"),
           conditionalPanel(
             condition = "input.lostType == 'Crosses'",
             p("List of crosses whose statuses have been reported")
           ),
           conditionalPanel(
             condition = "input.lostType == 'Plantlets'",
             p("Plantlets that have been contaminated or died")
           )
         )
       }
  })
  
  
  output$current_Activities <- renderHighchart({
    result = data.table(bbDF)
    result = result[lubridate::year(result$Date) >= 2018 & result$Activity !='Status'] # recent records
    
    
    v = data.table(table(result$Location,result$Activity)) %>% filter(N >0)
    y = data.frame(rowSums(table(v$V1,v$V2)))
    colnames(y) = "Number"
    z = tibble::rownames_to_column(y, var="Location")
    z = data_frame(name = z$Location,y = z$Number, drilldown = tolower(name))
    
    data = result
    z1 = data[data$Location == paste0("",z$name[1],""),]
    z2 = data[data$Location == paste0("",z$name[2],""),]
    z3 = data[data$Location == paste0("",z$name[3],""),]
    
    z21 = data.table(table(z1$Activity)) %>% filter(N >0)
    z22 = data.table(table(z2$Activity)) %>% filter(N >0)
    #z23 = data.table(table(z3$Activity)) %>% filter(N >0)
    
    z31 <- arrange(data_frame(name = z21$V1,value = z21$N),desc(value))
    z32 <- arrange(data_frame(name = z22$V1,value = z22$N),desc(value))
    #z33 <- arrange(data_frame(name = z23$V1,value = z23$N),desc(value))
    
    ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.name);}")
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(type = "category") %>%
      hc_legend(enabled = FALSE) %>%
      hc_yAxis(title = list(text = "Number"), gridLineWidth = 0, tickInterval = 5) %>%
      hc_plotOptions(series = list(column = list(stacking = "normal"), 
                                   borderWidth=0,
                                   dataLabels = list(enabled = TRUE),
                                   events = list(click = ClickFunction)
      )
      ) %>%
      hc_add_series(data=z,name="Number", colorByPoint = TRUE,colors = c("blue","green","orange")) %>% 
      
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = list(
          list(id = paste0(tolower(z$name[1])), data = list_parse2(z31), name="Number"),
          list(id = paste0(tolower(z$name[2])), data = list_parse2(z32), name="Number")#,
          #list(id = paste0(tolower(z$name[3])), data = list_parse2(z33), name="Number")
          
        )
      )
  })
  
  makeReactiveBinding("outputText")
  
  observeEvent(input$Clicked, {
    outputText <<- paste0(input$Clicked)
  })
  
  
  # current details title
  output$currentTitle <- renderUI({
    if(is.null(input$Clicked)){
      div(
        tags$p(style = "color: orange; font-size: 25px; text-align: center;",
               "This plot show the current number of active activities per site"), br()
      )
    } else {
      div(
        tags$p(style = "color: orange; font-size: 25px; text-align: center;",
               paste("This plot show the current number of accessions in every stage in", input$Clicked)), br()
      )
    }
    
  })
  
  current_details <- reactive({
    #result = bbDF %>% dplyr::filter(Date >= 2018)
    result = data.table(bbDF)
    result = result[lubridate::year(result$Date) >= 2018 & result$Activity !='Status'] # recent records
    
    if(is.null(input$Clicked)){
      v = data.table(table(result$Location,result$Activity)) %>% filter(N >0)
      y = data.frame(rowSums(table(v$V1,v$V2)))
      colnames(y) = "Number"
      z = tibble::rownames_to_column(y, var="Location")
    } else {
      temp <- result
      rowcheck <- temp[temp$Location == input$Clicked,]
      
      if (nrow(rowcheck)!=0) {
        temp <- temp[temp$Location == input$Clicked,]
        Lvl1Click <<- input$Clicked
      } else {
        temp <- temp[temp$Location == Lvl1Click,]
        temp <- temp[temp$Activity == input$Clicked,]
      }
      
      if(input$Clicked=='Seed extraction'){
        temp <- temp[,c("Activity","Accession","Date","Total Seeds")]
      } else if(input$Clicked == 'Germination after 8 weeks'){
        temp <- temp[,c("Activity", "Accession","Date","Actively germinating after 8 Weeks")]
      } else if(input$Clicked == 'Embryo rescue'){
        temp = temp[,c("Activity","Accession","Date","Good Seeds","Bad Seeds","Number of Embryo Rescued")]
      } else {
        temp = dplyr::select(temp,"Activity","Accession","Date")
      }
      
      if(nrow(table(temp$Activity))>1){
        temp = temp %>%
          dplyr::group_by(Activity) %>%
          dplyr::summarise(`Number of accessions` = n()) %>%
          dplyr::arrange(`Number of accessions`)
      }
      return (temp)
    }
  })
  
  # Current details Table
  output$current_Table <- DT::renderDataTable({
    current_details()
  })
  
  output$download_current_details <- downloadHandler(filename = function(){
    #result = filter(bbDF, lubridate::year(Date) >= 2018)
    result = data.table(bbDF)
    result = result[lubridate::year(result$Date) >= 2018 & result$Activity !='Status'] # recent records
    
    if(is.null(input$Clicked)){
      paste0("Number of active activities per site",'-',Sys.time(), '.csv')
    } else if(!is.null(input$Clicked)){
      temp <- result
      rowcheck <- temp[temp$Location == input$Clicked,]
      if (nrow(rowcheck)!=0) {
        Lvl1Click <<- input$Clicked
        paste0(input$Clicked,"- active activities",'-',Sys.time(), '.csv')
        
      } else {
        paste0(Lvl1Click, "_",input$Clicked,'-',Sys.time(), '.csv')
      }
      
    }
  },
  content = function(file) {
    write.csv(current_details(), file, row.names = FALSE)
  }
  )
   
################################################################################################### 
#--------------------------Data flow tab  
################################################################################################## 
 
    # collapsible tree
  observeEvent(input$reset_dataflow, {
    reset("dataflow_filters")
  })
  
  output$overviewPlot <- renderCollapsibleTree({
    req(input$df_dateRange)
    result = cleantable %>%
      dplyr::filter(!Activity %in% c('Flowering', 'Status', 'Repeat pollination') & 
                      Date %between% list(input$df_dateRange[1],input$df_dateRange[2]))
    result$Activity = gsub('First pollination','Pollination', result$Activity)
    
    if(input$df_site!="All"){
      result = result %>% 
        dplyr::filter(Location %in% input$df_site)
    }
    
    result = result[,c("Location", "Activity", "Accession")]
    data = result %>% 
      dplyr::group_by(Location, Activity)
    data = dplyr::summarise(data, Accessions = n())
    data$col = data$Location
    levels(data$col) <- colorspace::rainbow_hcl(11)
    
    collapsibleTreeSummary(
      data,
      inputId = "node",
      c("Location", "Activity", "Accessions"),
      attribute = "Accessions",
      maxPercent = 50,
      nodeSize = "Accessions",
      collapsed = F
    )
  })
  

  data_flow <- reactive({
    req(input$df_dateRange)
    result = cleantable %>%
      dplyr::filter(!Activity %in% c('Flowering', 'Status', 'Repeat pollination') & 
                      Date %between% list(input$df_dateRange[1],input$df_dateRange[2]))
    result$Activity = gsub('First pollination','Pollination', result$Activity)
    
    if(input$df_site!="All"){
      result = result %>% 
        dplyr::filter(Location %in% input$df_site)
    }
    
    if(is.null(input$node)){
      result = result %>% 
        dplyr::group_by(Location, Activity) %>%
        dplyr::summarise(Accessions = n())
    } else if(length(input$node)==1){ #!is.null(input$node) && 
      result %>%
        dplyr::filter(Location==input$node[1])
    } else if(length(input$node)==2){ #!is.null(input$node) && 
          
          if(input$node[1]=="Seed extraction"){
            dt = bananadata %>%
              dplyr::rename(Accession = Crossnumber) %>%
              dplyr::select(Location, Accession, `Total Seeds`)
            result = dplyr::left_join(result,dt, by = c("Location","Accession"))
          } else if(input$node[1]=="Embryo Rescue"){
            dt = bananadata %>%
              dplyr::rename(Accession = Crossnumber) %>%
              dplyr::select(Location, Accession, `Good Seeds`,`Number of Embryo Rescued`)
            result = dplyr::left_join(result,dt, by = c("Location","Accession"))
          } else if(input$node[1]=="Germination after 2 weeks"){
            dt = bananadata %>%
              dplyr::rename(Accession = Crossnumber) %>%
              dplyr::select(Location, Accession, `Active after 2 Weeks`)
            result = dplyr::left_join(result,dt, by = c("Location","Accession"))
          } else if(input$node[1]=="Germination after 8 weeks"){
            dt = bananadata %>%
              dplyr::rename(Accession = Crossnumber) %>%
              dplyr::select(Location, Accession, `Active after 8 Weeks`)
            result = dplyr::left_join(result,dt, by = c("Location","Accession"))
          }
      
      result = result %>% dplyr::filter(Location==input$node[2], Activity==input$node[1]) %>%
        dplyr::select(Location, Activity, Accession, Date, everything(),-c(Mother, Father))
      
    }
    return(result)
  })
  
  
  # Data flow Table
  output$collapsed_table <- DT::renderDataTable({
   
   DT::datatable(data_flow(), 
                  style = 'bootstrap', rownames = FALSE, 
                  filter = list(position = 'top'),
                  extensions = c('Buttons','FixedColumns','FixedHeader'),
                  options = list(pageLength = 5, 
                                 lengthMenu = c(5, 10, 20, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE,
                                 rowCallback = JS( 'function(row, data) { $("td:eq(5)", row).css("text-align", "center"); }'),
                                 columnDefs = list(list(className = "dt-head-center dt-center", targets = "_all"))
                  )
    )
  })
  
  # Download data flow data
  output$download_data_flow <- downloadHandler(filename = function(){
    result = cleantable %>%
      filter(Activity != 'Flowering' & Activity !='Status' & Activity !='Repeat pollination')  %>%
      filter(Date >= input$df_dateRange[1], Date <= input$df_dateRange[2])
    
    if(input$df_site !="All"){ # && (input$df_dateRange[2] - input$df_dateRange[1]) < 31
      result <- result %>%
        dplyr::filter(Location %in% input$df_site,
                      Date >= input$df_dateRange[1],
                      Date <= input$df_dateRange[2])
      
    }
    
    result = result %>% dplyr::select(Location, Activity, Accession)
    #colnames(result) = gsub("_"," ", names(result))
    if(is.null(input$node)){
      paste0("Banana data - ", Sys.time(),".csv")
    } else if(length(input$node)==1){
      paste0(input$node[1],"-", Sys.time(),".csv")
    } else if(length(input$node)==2){
      paste0(input$node[2],"_",input$node[1],"-", Sys.time(),".csv")
    }
  },
  content = function(file) {
    fwrite(data_flow(), file, row.names = FALSE)
  }
  )
  

  # LOST INFORMATION
  lostInput <- reactive({
    req(input$lostType)
    switch(input$lostType,
           "Crosses" = lostInfo,
           "Plantlets" = contamination)
  })
  output$lostTbl <- DT::renderDT({
    
    DT::datatable(lostInput(), filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
   
  })
  
  output$downloadLost <- downloadHandler(
  filename = function(){paste0(input$lostType,"-",Sys.time(),".csv")},
  content = function(file) {
    fwrite(lostdtIn(), file, row.names = FALSE)
  }
  )
})
)