

source("helpers/scheduler.R")
### Handle cliks on a treemap
tmLocate <-
  function(coor, tmSave) {
    tm <- tmSave$tm
    
    # retrieve selected rectangle
    rectInd <- which(tm$x0 < coor[1] &
                       (tm$x0 + tm$w) > coor[1] &
                       tm$y0 < coor[2] &
                       (tm$y0 + tm$h) > coor[2])
    
    return(tm[rectInd[1], ])
    
  }
#######

statusserver <- function(env_serv) with(env_serv, local({
  
  
  output$status_sidebar <- renderUI({
    if(input$overview_status == "Current details"){
     # div(position="fixed",
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
       else if(input$overview_status == "Schedule"){
           div(
             selectizeInput("schedule_site",label = "Site:",choices = c('',unique(as.character(cleantable$Location))))
             )
       }
     # )
  })
  
  
  output$current_Activities <- renderHighchart({
    result = result = bbDF %>% dplyr::filter(lubridate::year(Date) >= 2018) %>%
      setDT()
    data = result[result$Activity !='Status'] # recent records
  if(nrow(data)>0){  
    v = data.table(table(data$Location,data$Activity)) %>% filter(N >0)
    y = data.frame(rowSums(table(v$V1,v$V2)))
    colnames(y) = "Number"
    z = tibble::rownames_to_column(y, var="Location")
    z = tibble(name = z$Location,y = z$Number, drilldown = tolower(name))
    
    for(i in 1:nrow(z)){
      p = data[data$Location == paste0("",z$name[i],""),]
      pp = data.table(table(p$Activity)) %>% filter(N >0)
      ppp <- arrange(tibble(name = pp$V1,value = pp$N),desc(value))
      
      assign(paste0("z",i),p)
      assign(paste0("z2",i),pp)
      assign(paste0("z3",i),ppp)
    }
    
    if(nrow(z)==1){
      series = list(
        list(id = paste0(tolower(z$name[1])), data = list_parse2(z31), name="Number")
      )
    } else if(nrow(z)==2){
      series = list(
        list(id = paste0(tolower(z$name[1])), data = list_parse2(z31), name="Number"),
        list(id = paste0(tolower(z$name[2])), data = list_parse2(z32), name="Number")
      )
    } else if(nrow(z)==3){
      series = list(
        list(id = paste0(tolower(z$name[1])), data = list_parse2(z31), name="Number"),
        list(id = paste0(tolower(z$name[2])), data = list_parse2(z32), name="Number"),
        list(id = paste0(tolower(z$name[3])), data = list_parse2(z33), name="Number")
      )
    } else if(nrow(z)==4){
      series = list(
        list(id = paste0(tolower(z$name[1])), data = list_parse2(z31), name="Number"),
        list(id = paste0(tolower(z$name[2])), data = list_parse2(z32), name="Number"),
        list(id = paste0(tolower(z$name[3])), data = list_parse2(z33), name="Number"),
        list(id = paste0(tolower(z$name[4])), data = list_parse2(z34), name="Number")
      )
    } else if(nrow(z)==5){
      series = list(
        list(id = paste0(tolower(z$name[1])), data = list_parse2(z31), name="Number"),
        list(id = paste0(tolower(z$name[2])), data = list_parse2(z32), name="Number"),
        list(id = paste0(tolower(z$name[3])), data = list_parse2(z33), name="Number"),
        list(id = paste0(tolower(z$name[4])), data = list_parse2(z34), name="Number"),
        list(id = paste0(tolower(z$name[5])), data = list_parse2(z35), name="Number")
      )
    }
    
    
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
      hc_add_series(data=z,name="Number", colorByPoint = TRUE,colors = c("maroon","blue","yellow","green","orange")) %>% 
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = series
      )
  } else {
    return(NULL)
  }
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
    result = bbDF[lubridate::year(Date) >= 2018]
    result = result[result$Activity !='Status'] # recent records
  if(nrow(result)>0){  
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
        temp <- temp[,c("Activity","Accession","Date","Total Seeds")][`Total Seeds`>0]
      } else if(input$Clicked == 'Germination'){
        temp <- temp[,c("Activity", "Accession","Date","Number of Embryo Germinating")][`Number of Embryo Germinating`>0]
      } else if(input$Clicked == 'Embryo rescue'){
        temp = temp[,c("Activity","Accession","Date","Good Seeds","Bad Seeds","Number of Embryo Rescued")][`Good Seeds`>0]
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
  } else {
    return(NULL)
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
    } else if(length(input$node)==2){ 
      
          if(input$node[1]=="Pollination"){
            result = result[,1:6]
          } else if(input$node[1]=="Harvested bunches"){
            result = result[,1:4]
          }
          if(input$node[1]=="Seed extraction"){
            result = result[,c(1:4,7)]
          } else if(input$node[1]=="Embryo Rescue"){
            result = result[,c(1:4,7:9)]
          } else if(input$node[1]=="Germination"){
            result = result[,c(1:4,7:10)]
          } else if(input$node[1]=='Subculturing'){
            result = result[,c(1:4,11)]
          }  else if(input$node[1]=='Rooting'){
            result = result[,c(1:4,11:12)]
          }   else if(input$node[1]=='Weaning 1/ Sent out'){
            result = result[,c(1:4,11:13)]
          }   else if(input$node[1]=='Weaning 2'){
            result = result[,c(1:4,11:14)]
          }   else if(input$node[1]=='Screen house'){
            result = result[,c(1:4,11:15)]
          }   else if(input$node[1]=='Hardening'){
            result = result[,c(1:4,11:16)]
          }   else if(input$node[1]=='Open field'){
            result = result[,c(1:4,11:17)]
          } 
      
      result = result %>% dplyr::filter(Location==input$node[2], Activity==input$node[1]) %>%
        dplyr::select(Location, Activity, Accession, Date, everything())
      
    }
    return(result)
  })
  
  
  # Data flow Table
  output$collapsed_table <- DT::renderDataTable({
    DT::datatable(data_flow(), filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
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
  # file name
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
  

  # Schedule INFORMATION
  
  overdue <- reactive({
    result = scheduler %>%
      dplyr::filter(status=='Overdue')
    
    if(input$schedule_site !=''){
      result = result %>%
        dplyr::filter(Location==input$schedule_site)
    }
    result
  })
  
  ready <- reactive({
    result = scheduler %>%
      dplyr::filter(status=='Ready')
    
    if(input$schedule_site !=''){
      result = result %>%
        dplyr::filter(Location==input$schedule_site)
    }
    result
  })
  
  approaching <- reactive({
    result = scheduler %>%
      dplyr::filter(status=='Approaching')
    
    if(input$schedule_site !=''){
      result = result %>%
        dplyr::filter(Location==input$schedule_site)
    }
    result
  })
  
  output$schedulerOut <- renderUI({
    div(
      fluidRow(
        box(width=12, collapsible = TRUE, 
            tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;", paste(input$schedule_site), " Overdue Accessions"), br(),
            highchartOutput("overdue_summary") %>% withSpinner(color="#0dc5c1"),
            p(style = "color: #8B0000;", "Click for details"),br(), br(),
            uiOutput('overdueTbl')
        )),
      fluidRow(
      
      box(width=12, solidHeader = T, collapsible = TRUE, 
            tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;", paste(input$schedule_site), " Accessions Ready for Recording"), br(),
            highchartOutput("ready_summary") %>% withSpinner(color="#0dc5c1"),
            p(style = "color: #006400;", "Click for details"),br(), br(),
          
          uiOutput('readyTbl')
        )),
      fluidRow(
      box(width=12, collapsible = TRUE, 
            tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;", paste(input$schedule_site), " Accession Approaching Time of Recording"), br(),
            highchartOutput("approaching_summary") %>% withSpinner(color="#0dc5c1"),
            p(style = "color: #00CED1;", "Click for details"),br(), br(), 
            uiOutput('approachingTbl')
        )), br()
    )
  })
  
  
  js_overdue_bar_clicked <- JS("function(event) {Shiny.onInputChange('overdue_bar_clicked', [event.point.category]);}")
  output$overdue_summary <- renderHighchart({
    result <- overdue() %>%
        group_by(NextActivity) %>%
        dplyr::tally() %>%
        arrange(desc(n)) %>%
        dplyr::collect() 
      
      # plot title
      ptitle = if(input$schedule_site !=""){
        paste(input$schedule_site, " accession late for recording as of ", Sys.Date())
      } else if(input$site ==""){
        paste("Accession late for recording as of ", Sys.Date())
      }
      
      highchart() %>%
        hc_add_series(data = result$n, type = "column", color = '#8B0000', name = paste("Overdue accessions"),  events = list(click = js_overdue_bar_clicked)) %>%
        hc_xAxis(categories = result$NextActivity,tickmarkPlacement="on") %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 2) %>%
        hc_title(text=ptitle) %>%
        hc_add_theme(hc_theme_elementary())
    
  })
  
   # display the subsetted data
  output$overdueTbl <- renderUI({
    if(!is.null(input$overdue_bar_clicked)){
      div(
        column(1, offset = 11, downloadBttn('downloadOverdue',size="sm", style = 'jelly')),br(),
          dataTableOutput("overdue_drilldown"))
    }
  })
  
  overdue_drill <- reactive({
    result = overdue()
    if(!is.null(input$overdue_bar_clicked)){
      result = result %>% dplyr::filter(NextActivity %in% input$overdue_bar_clicked[1])
    } 
    result = result %>%
      dplyr::arrange(desc(`Days Elapsed`))
    result[,-6]
  })
   output$overdue_drilldown <- DT::renderDataTable({
     overdue_drill()
   })
  
   # Download
   output$downloadOverdue <- downloadHandler(
     filename = function(){paste0(input$schedule_site,"-",input$overdue_bar_clicked[1],".csv")},
     content = function(file) {
       fwrite(overdue_drill(), file, row.names = FALSE)
     }
   )
   
  #################### READY
   js_ready_bar_clicked <- JS("function(event) {Shiny.onInputChange('ready_bar_clicked', [event.point.category]);}")
   output$ready_summary <- renderHighchart({
     result <- ready() %>%
       group_by(NextActivity) %>%
       dplyr::tally() %>%
       arrange(desc(n)) %>%
       dplyr::collect() 
     
     # plot title
     ptitle = if(input$schedule_site !=""){
       paste(input$schedule_site, " accessions ready for recording as of ", Sys.Date())
     } else if(input$site ==""){
       paste("Accessions ready for recording as of ", Sys.Date())
     }
     
     highchart() %>%
       hc_add_series(data = result$n, type = "column", color = '#006400', name = paste("Ready accessions"),  events = list(click = js_ready_bar_clicked)) %>%
       hc_xAxis(categories = result$NextActivity,tickmarkPlacement="on") %>%
       hc_exporting(enabled = TRUE) %>% 
       hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                  shared = TRUE, borderWidth = 2) %>%
       hc_title(text=ptitle) %>%
       hc_add_theme(hc_theme_elementary())
     
   })
   
   # display the subsetted data
   output$readyTbl <- renderUI({
     if(!is.null(input$ready_bar_clicked)){
       div(
         column(1, offset = 11, downloadBttn('downloadReady',size="sm", style = 'jelly')),br(),
         dataTableOutput("ready_drilldown")
       )
     }
   })
   ready_drill <- reactive({
     result = ready()
     if(!is.null(input$ready_bar_clicked)){
       result = result %>% dplyr::filter(NextActivity %in% input$ready_bar_clicked[1])
     } 
     result = result %>%
       dplyr::arrange(desc(`Days Elapsed`))
     result[,-6]
   })
   output$ready_drilldown <- DT::renderDataTable({
     ready_drill()
   })
   
   # Download
   output$downloadReady <- downloadHandler(
     filename = function(){paste0(input$schedule_site,"-",input$ready_bar_clicked[1],".csv")},
     content = function(file) {
       fwrite(ready_drill(), file, row.names = FALSE)
     }
   )
   #################### APPROACHING
   js_approaching_bar_clicked <- JS("function(event) {Shiny.onInputChange('approaching_bar_clicked', [event.point.category]);}")
   output$approaching_summary <- renderHighchart({
     result <- approaching() %>%
       group_by(NextActivity) %>%
       dplyr::tally() %>%
       arrange(desc(n)) %>%
       dplyr::collect() 
     
     # plot title
     ptitle = if(input$schedule_site !=""){
       paste(input$schedule_site, " accessions almost ready for recording as of ", Sys.Date())
     } else if(input$site ==""){
       paste("Accessions almost ready for recording as of ", Sys.Date())
     }
     result = setDT(result)
     highchart() %>%
       hc_add_series(data = result[["n"]], type = "column", color = '#00CED1', name = paste("Approaching accessions"),  events = list(click = js_approaching_bar_clicked)) %>%
       hc_xAxis(categories = result[["NextActivity"]],tickmarkPlacement="on") %>%
       hc_exporting(enabled = TRUE) %>% 
       hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                  shared = TRUE, borderWidth = 2) %>%
       #hc_title(text=ptitle) %>%
       hc_add_theme(hc_theme_elementary())
     
   })
   
   # display the subsetted data
   
   output$approachingTbl <- renderUI({
     
     if(!is.null(input$approaching_bar_clicked)){
       div(
         verbatimTextOutput('txt3'),
         column(1, offset = 11, downloadBttn('downloadApproaching', size="sm", style = 'jelly')),br(),
         dataTableOutput("approaching_drilldown")
       )
     }
   })
   output$txt3 <- renderPrint({
     input$approaching_bar_clicked
   })
   approaching_drill <- reactive({
     result = approaching()
     if(!is.null(input$approaching_bar_clicked)){
       result = result %>% dplyr::filter(NextActivity %in% input$approaching_bar_clicked[1])
     } 
     result = result %>%
       dplyr::arrange(desc(`Days Elapsed`))
     result[,-6]
   })
   
   # Data table
   output$approaching_drilldown <- DT::renderDataTable({
     approaching_drill()
   })
  
   # Download
   output$downloadApproaching <- downloadHandler(
    filename = function(){paste0(input$schedule_site,"-",input$approaching_bar_clicked[1],".csv")},
    content = function(file) {
      fwrite(approaching_drill(), file, row.names = FALSE)
    }
  )
})
)


