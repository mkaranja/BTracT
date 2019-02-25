
library(shinyBS)
library(shinydashboardPlus)

library(magrittr)
library(data.table)

library(ggplot2)
library(highcharter)
library(lubridate)

overviewserver <- function(env_serv) with(env_serv, local({
  
  # RESET
  
  observeEvent(input$reset_overview, {
    reset("overview_controls")
  })

    ####################### VALUEBOXES #########################
  crossesBox <- reactive({
    result <- setDT(bananadata)[,c("Location","Crossnumber","First Pollination Date")]
    result = result[anytime::anydate(`First Pollination Date`) %between% c(input$dateRange[1],input$dateRange[2])]
    
    if(input$site != "All"){ 
      result = result[Location == input$site & anytime::anydate(`First Pollination Date`) %between% c(input$dateRange[1],input$dateRange[2])]
    } else{
      result = result
    }
    
  })
  bunchesBox <- reactive({
    result <- setDT(bananadata)[,c("Location", "Crossnumber", "Bunch Harvest Date","Days to Maturity", "Mother", "Father")] %>%
      .[complete.cases(.),]
    result = result[anytime::anydate(`Bunch Harvest Date`) %between% c(input$dateRange[1],input$dateRange[2])]
    
    if(input$site != "All"){ 
      result <- result[Location == input$site & anytime::anydate(`Bunch Harvest Date`) %between% c(input$dateRange[1],input$dateRange[2])]
    } else{ 
      result = result
    }
  })
  seedsBox <- reactive({
    result <- setDT(bananadata)[,c("Location", "Crossnumber", "Seed Extraction Date","Total Seeds")] %>%
      .[complete.cases(.),]
    result = result[anytime::anydate(`Seed Extraction Date`) %between%  c(input$dateRange[1],input$dateRange[2])]
    if(input$site != "All"){ 
      result <- result[Location == input$site & anytime::anydate(`Seed Extraction Date`) %between% c(input$dateRange[1],input$dateRange[2])]
    } else{ 
      result = result
    }
  })
  embryoBox <- reactive({
    result <- setDT(bananadata)[,c("Location", "Crossnumber", "Embryo Rescue Date","Number of Embryo Rescued")] %>%
      .[complete.cases(.),]
    result = result[anytime::anydate(`Embryo Rescue Date`) %between% c(input$dateRange[1], input$dateRange[2])]
    if(input$site != "All"){ 
      result <- result[Location == input$site & anytime::anydate(`Embryo Rescue Date`) %between% c(input$dateRange[1], input$dateRange[2])]
    } else{ 
      result = result
    }
  })
  active8weeksBox <- reactive({
     result <- setDT(seeds_data)[,c("Location", "SeedID","Germination after 8 Weeks Date")] %>%
       .[complete.cases(.),]
     result = result[anytime::anydate(as.character(`Germination after 8 Weeks Date`)) %between% c(input$dateRange[1],input$dateRange[2])]
     if(input$site != "All"){ 
       result <- result[Location == input$site & anytime::anydate(as.character(`Germination after 8 Weeks Date`)) %between% c(input$dateRange[1],input$dateRange[2])]
     } else{ 
       result = result
      }
     
   })
  
  openfieldBox <- reactive({
    result <- setDT(plantlets)[,c("Location", "PlantletID", "Openfield Transfer Date")] %>%
      .[complete.cases(.),]
    result = result[anytime::anydate(as.character(`Openfield Transfer Date`)) %between% c(input$dateRange[1], input$dateRange[2])]
    if(input$site != "All"){ 
      result <- result[Location == input$site & anytime::anydate(as.character(`Openfield Transfer Date`)) %between% c(input$dateRange[1], input$dateRange[2])]
    }else {
      result = result
    }
    
  })
  
  
  
  output$n_crosses <- renderValueBox({
    result <- crossesBox() %>%
      dplyr::tally() %>%
      dplyr::pull() %>% 
      as.integer()
    box1<-valueBox(value=result,color = "teal",href="#",subtitle=HTML("<b>Total crosses</b>")
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_crosses"
    return(box1)
  })
  
  observeEvent(input$button_n_crosses, {
    toggleModal(session,"mod_crosses","open")
    
    output$list_crosses <- DT::renderDataTable({
      result <- crossesBox()
      
      DT::datatable(result, 
                    style = 'bootstrap', rownames = FALSE, 
                    filter = list(position = 'top'),
                    extensions = c('Buttons','FixedColumns','FixedHeader'),
                    options = list(pageLength = 10, 
                                   lengthMenu = c(5, 10, 20, 50, 100, 500,1000),
                                   autoWidth = T, searchHighlight=T, stateSave = TRUE,
                                   rowCallback = JS( 'function(row, data) { $("td:eq(5)", row).css("text-align", "center"); }'),
                                   scrollX = T,
                                   columnDefs = list(list(className = 'dt-center', target = "_all")),
                                   fixedColumns = list(leftColumns=1, rightColumns=0)
                    )
      )
    })
  })
  # download all or selected rows
  
  downloadCrosses <- reactive({ 
    result <- crossesBox()
    
    if(!is.null(input$list_crosses_rows_selected)){
      result <- result[input$list_crosses_rows_selected,]
    }
  })
  
  
  output$download_crosses <- downloadHandler(
    filename = function(){
      sprintf("Crosses_%s.csv",Sys.time())
    },
    content = function(file) {
      write.csv(downloadCrosses(), file, row.names = FALSE)
    }
  )
  
  
  output$n_bunches <- renderValueBox({
    
    result <- bunchesBox() %>%
      dplyr::tally() %>%
      dplyr::pull() %>% 
      as.integer()
    box1<-valueBox(value=result,
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Banana bunches</b>")
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_bunches"
    return(box1)
   
  })
  
  observeEvent(input$button_n_bunches, {
    toggleModal(session, "modal_bunches", "open")
  })
  output$list_bunches <- DT::renderDataTable({
    result = bunchesBox()
    
    DT::datatable(result, 
                  style = 'bootstrap', rownames = FALSE, 
                  filter = list(position = 'top'),
                  extensions = c('Buttons','FixedColumns','FixedHeader'),
                  options = list(pageLength = 10, 
                                 lengthMenu = c(5, 10, 20, 50, 100, 500,1000),
                                 autoWidth = T, searchHighlight=T, stateSave = TRUE,
                                 rowCallback = JS( 'function(row, data) { $("td:eq(5)", row).css("text-align", "center"); }'),
                                 scrollX = T,
                                 columnDefs = list(list(className = 'dt-center', target = "_all")),
                                 fixedColumns = list(leftColumns=1, rightColumns=0)
                  )
    )
  })
  
  downloadBunches <- reactive({ 
    result <- bunchesBox()
    
    if(!is.null(input$list_bunches_rows_selected)){
      result <- result[input$list_bunches_rows_selected,]
    } else {
      result <- result
    }
  })
  
  
  output$download_bunches <- downloadHandler(
    filename = function(){sprintf("harvesting_%s.csv",Sys.time())},
    content = function(file) {
      write.csv(bunchesBox(), file, row.names = FALSE)
    }
  )
  #===================================================================================================================================Total seeds
  output$n_totalseeds <- renderValueBox({
    result <- seedsBox()
    result <- sum(result$`Total Seeds`)
    box1<-valueBox(value=result,
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Total seeds</b>")
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_seeds"
    return(box1)
 
  })
  
  observeEvent(input$button_n_seeds, {
    toggleModal(session, "modal_totalseeds", "open")
  })
  output$list_totalseeds <- DT::renderDataTable({
    result <- seedsBox()
    DT::datatable(result, 
                  style = 'bootstrap', rownames = FALSE, 
                  filter = list(position = 'top'),
                  extensions = c('Buttons','FixedColumns','FixedHeader'),
                  options = list(pageLength = 10, 
                                 lengthMenu = c(5, 10, 20, 50, 100, 500,1000),
                                 autoWidth = T, searchHighlight=T, stateSave = TRUE,
                                 rowCallback = JS( 'function(row, data) { $("td:eq(5)", row).css("text-align", "center"); }'),
                                 scrollX = T,
                                 columnDefs = list(list(className = 'dt-center', target = "_all")),
                                 fixedColumns = list(leftColumns=1, rightColumns=0)
                  )
    )
  })
  
  downloadTotalSeeds <- reactive({ 
    result <- seedsBox()
    result <- dplyr::select(result, "Crossnumber","Seed Extraction Date","Total Number of Seeds")
    
    
    if(!is.null(input$list_totalseeds)){
      result <- result[input$list_totalseeds_rows_selected,]
    } else {
      result <- result
    }
  })
  
  
  output$download_totalseeds <- downloadHandler(
    filename = function(){sprintf("Seed_extraction_%s.csv",Sys.time())},
    content = function(file) {
      write.csv(seedsBox(), file, row.names = FALSE)
    }
  )
  #======================================================================================= ========================================= Rescued
  output$n_rescued <- renderValueBox({
    result <- embryoBox()
    result <- sum(as.integer(result$`Number of Embryo Rescued`))
    
    box1<-valueBox(value=result,
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Embryo rescue</b>")
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_rescued"
    return(box1)
  })
  
  observeEvent(input$button_n_rescued, {
    toggleModal(session, "modal_rescued", "open")
  })
  output$list_rescued <- DT::renderDataTable({
    result <- embryoBox()
    
    result <- result %>% dplyr::select("Crossnumber",'Number of Embryo Rescued','Embryo Rescue Date')
    
    DT::datatable(result, 
                  style = 'bootstrap', rownames = FALSE, 
                  filter = list(position = 'top'),
                  extensions = c('Buttons','FixedColumns','FixedHeader'),
                  options = list(pageLength = 10, 
                                 lengthMenu = c(5, 10, 20, 50, 100, 500,1000),
                                 autoWidth = T, searchHighlight=T, stateSave = TRUE,
                                 rowCallback = JS( 'function(row, data) { $("td:eq(5)", row).css("text-align", "center"); }'),
                                 scrollX = T,
                                 columnDefs = list(list(className = 'dt-center', target = "_all")),
                                 fixedColumns = list(leftColumns=1, rightColumns=0)
                  )
    )
  })
  
  downloadRescued <- reactive({ 
    result <- embryoBox
    
    if(!is.null(input$list_rescued)){
      result <- result[input$list_rescued_rows_selected,]
    } else {
      result <- result
    }
  })
  
  output$download_rescued <- downloadHandler(
    filename = function(){sprintf("embryorescue_%s.csv",Sys.time())},
    content = function(file) {
      write.csv(embryoBox(), file, row.names = FALSE)
    }
  )
 
  #================================================================================================================================ germinating after 8 weeks  
  output$n_8weeks <- renderValueBox({
    result <- active8weeksBox() %>%
        dplyr::tally() %>%
        dplyr::pull() %>% 
        as.integer()
    
      
    box1<-valueBox(value=result,
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Active after 8 weeks</b>")
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_8weeks"
    return(box1)
  })
  
  observeEvent(input$button_n_8weeks, {
    toggleModal(session, "modal_8weeks", "open")
  })
  output$list_8weeks <- DT::renderDataTable({
    result <- active8weeksBox()
    DT::datatable(result, 
                  style = 'bootstrap', rownames = FALSE, 
                  filter = list(position = 'top'),
                  extensions = c('Buttons','FixedColumns','FixedHeader'),
                  options = list(pageLength = 10, 
                                 lengthMenu = c(5, 10, 20, 50, 100, 500,1000),
                                 autoWidth = T, searchHighlight=T, stateSave = TRUE,
                                 rowCallback = JS( 'function(row, data) { $("td:eq(5)", row).css("text-align", "center"); }'),
                                 scrollX = T,
                                 columnDefs = list(list(className = 'dt-center', target = "_all")),
                                 fixedColumns = list(leftColumns=1, rightColumns=0)
                  )
    )
  })
  
  output$download_8weeks <- downloadHandler(
    filename = function(){sprintf("germination_after_8_weeks_%s.csv",Sys.time())},
    content = function(file) {
      write.csv(active8weeksBox(), file, row.names = FALSE)
    }
  )
  
  #==========================================================================================================================openfield
  
  output$n_openfield_plantlets <- renderValueBox({
    result <- openfieldBox() %>%
      dplyr::tally() %>%
      dplyr::pull() %>% 
      as.integer()
    
    box1<-valueBox(value=result,
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Plants in openfield</b>")
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_openfield"
    return(box1)
  })
  
  observeEvent(input$button_n_openfield, {
    toggleModal(session, "modal_openfield", "open")
  })
  output$list_openfield <- DT::renderDataTable({
    result <- openfieldBox()
    
    DT::datatable(result, 
                  style = 'bootstrap', rownames = FALSE, 
                  filter = list(position = 'top'),
                  extensions = c('Buttons','FixedColumns','FixedHeader'),
                  options = list(pageLength = 10, 
                                 lengthMenu = c(5, 10, 20, 50, 100, 500,1000),
                                 autoWidth = T, searchHighlight=T, stateSave = TRUE,
                                 rowCallback = JS( 'function(row, data) { $("td:eq(5)", row).css("text-align", "center"); }'),
                                 scrollX = T,
                                 columnDefs = list(list(className = 'dt-center', target = "_all")),
                                 fixedColumns = list(leftColumns=1, rightColumns=0)
                  )
    )
  })
  
  downloadOpenfield <- reactive({ 
    result <- Openfield
    if(input$site != "All"){ 
      result <- dplyr::filter(plantlets, Location %in% input$site, Openfield_Transfer_Date %between% c(input$dateRange[1], input$dateRange[2]))
    }
    
    if(!is.null(input$list_openfield)){
      result <- result[input$list_openfield_rows_selected,]
    } else {
      result <- result
    }
  
  })
  
  
  output$download_openfield <- downloadHandler(
    filename = function(){sprintf("Plantlets_in_openfield_%s.csv",Sys.time())},
    content = function(file) {
      write.csv(openfieldBox(), file, row.names = FALSE)
    }
  )
  
  ################ END OF VALUE INFO ############### 
  js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
  
  output$crossesOUT <- renderUI({
    div(
      if(input$site=="All"){
        highcharter::highchartOutput("totals_site", height = 550)
      } else {
        highcharter::highchartOutput("totals", height = 550)
      }
    )
  })
  
  plotTitle <- reactive({
    if(input$site !="All"){
     paste(input$site,"Total Crosses (",input$dateRange[1],"-",input$dateRange[2],")")
      
    } else if(input$site =="All"){
      paste("Overall Total Crosses (",input$dateRange[1],"-",input$dateRange[2],")")
      
    }
  })
  
  output$totals_site <- renderHighchart({
    bananadata$`First Pollination Date`=as.Date(bananadata$`First Pollination Date`)
    result <- setDT(bananadata)[,c("Location","Crossnumber","Mother", "Father","First Pollination Date")] 
    result = result[,Yearly := format(`First Pollination Date`,"%Y")]
    result = result[,Monthly := format(`First Pollination Date`,"%m")]
    result = result[,Daily := format(`First Pollination Date`,"%d")]
    result$Yearly = as.numeric(result$Yearly); result$Monthly = as.numeric(result$Monthly); result$Daily = as.numeric(result$Daily)
    
    result = result[`First Pollination Date` %between% c(input$dateRange[1], input$dateRange[2])]
    
    if((input$dateRange[2] - input$dateRange[1]) < 31){
      result = result[`First Pollination Date` %between% c(input$dateRange[1], input$dateRange[2])]
      result = result[,.N,by=.(Location, Daily)][order(Daily)]
      result$Time = result$Daily
      group_name = "Daily"
    } else if((lubridate::year(input$dateRange[2]) == lubridate::year(input$dateRange[1]))){
      result <- setDT(result)[lubridate::year(`First Pollination Date`) == lubridate::year(input$dateRange[1]) &
                                    lubridate::month(`First Pollination Date`) %between% c(lubridate::month(input$dateRange[1]),lubridate::month(input$dateRange[2]))]
      result = result[,.N, by = .(Location, Monthly)][order(Monthly)]
      result$Time = result$Monthly
      #result$Time = month.abb[result$month]
      group_name = "Monthly"
    }  else if((lubridate::year(input$dateRange[2]) != lubridate::year(input$dateRange[1]))){ 
      result <- setDT(result)[lubridate::year(`First Pollination Date`) %between% c(lubridate::year(input$dateRange[1]), lubridate::year(input$dateRange[2]))]
      result = result[,.N, by=.(Location, Yearly)][order(Yearly)]
      result$Time = result$Yearly
      group_name = "Yearly"
    }
    
    
    highcharter::hchart(result, "column", hcaes(x = Time, y = N, group = Location)) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text=plotTitle()) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
  
  output$totals <- renderHighchart({
    result <- bananadata %>%
      dplyr::select(Location,Crossnumber,Mother, Father,`First Pollination Date`) %>%
      dplyr::mutate(day = lubridate::day(`First Pollination Date`),
             month = lubridate::month(`First Pollination Date`),
             year = lubridate::year(`First Pollination Date`)) %>%
      .[complete.cases(.),]
    result = result %>% dplyr::filter(`First Pollination Date` >= input$dateRange[1], `First Pollination Date` <= input$dateRange[2])
    
    if((input$dateRange[2] - input$dateRange[1]) < 31){
      result = result %>%
        dplyr::filter(Location %in% input$site,
               `First Pollination Date` >= input$dateRange[1],
               `First Pollination Date` <= input$dateRange[2]) %>%
        dplyr::group_by(day) %>% 
        dplyr::tally()
      grp = result$day
      group_name = "Daily"
    } else if((lubridate::year(input$dateRange[2]) == lubridate::year(input$dateRange[1]))){
      result <- dplyr::filter(result, Location %in% input$site,
                       lubridate::year(`First Pollination Date`) == lubridate::year(input$dateRange[1]),
                       lubridate::month(`First Pollination Date`) %between% c(lubridate::month(input$dateRange[1]),lubridate::month(input$dateRange[2]))) %>% 
        dplyr::group_by(month) %>% 
        dplyr::tally() 
      grp = month.abb[result$month]
      group_name = "Monthly"
    }  else if((lubridate::year(input$dateRange[2]) != lubridate::year(input$dateRange[1]))){ 
      result <- dplyr::filter(result, Location %in% input$site, 
                       lubridate::year(`First Pollination Date`) %between% c(lubridate::year(input$dateRange[1]), lubridate::year(input$dateRange[2])))  %>% 
        dplyr::group_by(year) %>% dplyr::tally()
      
      grp = result$year
      group_name = "Yearly"
    }
    
   
    # PLOT POLLINATION
    
    highchart() %>%
      hc_add_series(data = result$n, type = "column",name = paste(group_name, " total crosses"),events = list(click = js_click_line)) %>%
      hc_xAxis( categories = grp,tickmarkPlacement="on") %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text=plotTitle()) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  
  output$totalcrosses <- renderUI({
    result <- bananadata %>%
      dplyr::select(Location,Crossnumber,Mother, Father,`First Pollination Date`) %>% 
      .[complete.cases(.),]
    result = result %>% 
      dplyr::filter(`First Pollination Date` %between% c(input$dateRange[1],input$dateRange[2]))
    
    if(input$site !="All" && (input$dateRange[2] - input$dateRange[1]) < 31){
      result <- result %>%
        dplyr::filter(Location %in% input$site,
               `First Pollination Date` >= input$dateRange[1],
               `First Pollination Date` <= input$dateRange[2])
      
    } else if(input$site !="All" && (lubridate::year(input$dateRange[2]) == lubridate::year(input$dateRange[1]))){
      result <- dplyr::filter(result, Location %in% input$site,
                       lubridate::year(`First Pollination Date`) == lubridate::year(input$dateRange[1]),
                       lubridate::month(`First Pollination Date`) %between% c(lubridate::month(input$dateRange[1]), lubridate::month(input$dateRange[2])))
      
    }  else if(input$site != "All" && (lubridate::year(input$dateRange[2]) != lubridate::year(input$dateRange[1]))){ 
      result <- dplyr::filter(result, Location %in% input$site,
                       lubridate::year(`First Pollination Date`) %between% c(lubridate::year(input$dateRange[1]), lubridate::year(input$dateRange[2])))
      
    }
    else if(input$site == "All"){
      result <- result 
    } 
    
    result <- result %>% 
      dplyr::summarize(crosses = n())
    paste0("N = ", result)
    
  })
  
  
  # parents
  parentsInput <- reactive({
    result <- bananadata %>%
      dplyr::select(Location,Crossnumber,Mother, Father,`First Pollination Date`) %>%
      dplyr::filter(`First Pollination Date` %between% c(input$dateRange[1], input$dateRange[2]))
    
    if((input$dateRange[2] - input$dateRange[1]) < 31){
      result = result %>%
        dplyr::filter(`First Pollination Date` >= input$dateRange[1],
                      `First Pollination Date` <= input$dateRange[2])
    } else if((lubridate::year(input$dateRange[2]) == lubridate::year(input$dateRange[1]))){
      result <- dplyr::filter(result,
                              lubridate::year(`First Pollination Date`) == lubridate::year(input$dateRange[1]),
                              lubridate::month(`First Pollination Date`) %between% c(lubridate::month(input$dateRange[1]),lubridate::month(input$dateRange[2])))
    }  else if((lubridate::year(input$dateRange[2]) != lubridate::year(input$dateRange[1]))){ 
      result <- dplyr::filter(result,
                              lubridate::year(`First Pollination Date`) %between% c(lubridate::year(input$dateRange[1]), lubridate::year(input$dateRange[2])))
    }
    
    if(input$site !="All"){
      result = result %>%
        dplyr::filter(Location == input$site)
    }
    result
  })
  
   output$mother <- renderHighchart({
    
     result = parentsInput() %>%
       dplyr::group_by(Mother) %>%
       dplyr::tally() %>%
       dplyr::arrange(desc(n)) %>%
       dplyr::collect()
     
     # plot title
     ptitle = if(input$site !="All"){
       paste(input$site,"Female plants (",input$dateRange[1],"-",input$dateRange[2],")")
     } else if(input$site =="All"){
       paste("Female plants (",input$dateRange[1],"-",input$dateRange[2],")")
     }  
     
     highchart() %>%
       hc_add_series(data = result$n,type = "bar",name = paste("Female plants")) %>%
       hc_xAxis(categories = result$Mother,tickmarkPlacement="on") %>%
       hc_exporting(enabled = TRUE) %>% 
       hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                  shared = TRUE, borderWidth = 2) %>%
       hc_title(text=ptitle) %>%
       hc_add_theme(hc_theme_elementary())
   })
   
   output$totalFemales <- renderUI({
     
     result <- parentsInput() %>% 
       dplyr::summarize(dplyr::n_distinct(Mother))
     paste0("N = ", result)
   })
   #.....................................................................................MALES 
   output$father <- renderHighchart({
     
     result <- parentsInput() %>%
       group_by(Father) %>%
       dplyr::tally() %>%
       arrange(desc(n)) %>%
       dplyr::collect() 
       
     # plot title
     ptitle = if(input$site !="All"){
       paste(input$site,"Male plants (",input$dateRange[1],"-",input$dateRange[2],")")
     } else if(input$site =="All"){
       paste("Male plants (",input$dateRange[1],"-",input$dateRange[2],")")
     }
     
     highchart() %>%
       hc_add_series(data = result$n, type = "bar",name = paste("Male plants")
       ) %>%
       hc_xAxis(categories = result$Father,tickmarkPlacement="on") %>%
       hc_exporting(enabled = TRUE) %>% 
       hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                  shared = TRUE, borderWidth = 2) %>%
       hc_title(text=ptitle) %>%
       hc_add_theme(hc_theme_elementary())
   })
   
   output$totalMales <- renderUI({
     
     result <- parentsInput() %>% 
       dplyr::summarize(dplyr::n_distinct(Father))
     paste0("N = ", result)
   })
   
  })
)