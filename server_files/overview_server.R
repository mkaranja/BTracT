
library(shinyBS)
library(shinydashboardPlus)

library(magrittr)
library(data.table)

library(ggplot2)
library(highcharter)
library(lubridate)


overviewserver <- function(env_serv) with(env_serv, local({

  observe({
    dt = setDT(cleantable)
    if(input$site=="All"){
      dt = cleantable
    } else{
      dt = dt[Location==input$site]
    }
    dates = lubridate::ymd(na.omit(dt$Date))
    updateDateRangeInput(session, "dateRange", start =  min(dates), end = max(dates), min = min(dates),max = max(dates)
    )
  })
  
  # RESET
  observeEvent(input$reset_overview, {
    dt = setDT(cleantable)
    if(input$site=="All"){
      dt = cleantable
    } else{
      dt = dt[Location==input$site]
    }
    dates = lubridate::ymd(na.omit(dt$Date))
    
    updateSelectInput(session, 'site',choices = c("All", as.list(unique(bananadata$Location))))
    updateDateRangeInput(
      session,"dateRange", start =  min(dates), end = max(dates), min = min(dates),max = max(dates)
    )
  })
  
  # VALUEBOXES 
  ######################################################################################## 
  # CROSSES
  ######################################################################################### 
  crossesBox <- reactive({
    req(input$site)
    req(input$dateRange)
    result <- setDT(bananadata)[,c("Location","Crossnumber","First Pollination Date")]
    result = result[anytime::anydate(`First Pollination Date`) %between% c(input$dateRange[1],input$dateRange[2])]
    
    if(input$site != "All"){ 
      result = result[Location == input$site & anytime::anydate(`First Pollination Date`) %between% c(input$dateRange[1],input$dateRange[2])]
    } 
    result
    
  })
  output$n_crosses <- renderValueBox({
    result <- crossesBox() %>%
      dplyr::tally() %>%
      dplyr::pull() %>% 
      as.integer()
    box1<-valueBox(value=result,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Total crosses</b><br><br>")
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_crosses"
    return(box1)
  })
  
  observeEvent(input$button_n_crosses, {
    toggleModal(session,"mod_crosses","open")
    
    output$list_crosses <- DT::renderDT({
      result <- crossesBox()
      
      DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                    options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                   searchHighlight=T, stateSave = TRUE))
    })
  })
  # download all or selected rows
  
  downloadCrosses <- reactive({ 
    result <- crossesBox()
    result = result[input[["list_crosses_rows_all"]],]
    if(!is.null(input$list_crosses_rows_selected)){
      result <- result[input$list_crosses_rows_selected,]
    }
    result = result[complete.cases(result$Crossnumber),]
    
    result = janitor::remove_empty(result, "cols")
    
    return(result)
  })
  
  
  output$download_crosses <- downloadHandler(
    filename = function(){
      paste0(input$site,"-","Crosses",Sys.time(),".csv")
    },
    content = function(file) {
      write.csv(downloadCrosses(), file, row.names = FALSE)
    }
  )
  
  # HARVESTED BUNCHES
  bunchesBox <- reactive({
    req(input$site)
    req(input$dateRange)
    result <- setDT(bananadata)[,c("Location", "Crossnumber", "Bunch Harvest Date","Days to Maturity", "Mother", "Father")] %>%
      .[complete.cases(.),]
    result = result[anytime::anydate(`Bunch Harvest Date`) %between% c(input$dateRange[1],input$dateRange[2])]
    
    if(input$site != "All"){ 
      result <- result[Location == input$site & anytime::anydate(`Bunch Harvest Date`) %between% c(input$dateRange[1],input$dateRange[2])]
    }
    result
  })
  output$n_bunches <- renderValueBox({
    result <- bunchesBox() %>%
      dplyr::tally() %>%
      dplyr::pull() %>% 
      as.integer()
    box1<-valueBox(value=result,
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Banana bunches</b><br><br>")
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
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })
  
  downloadBunches <- reactive({ 
    result <- bunchesBox()
    result = result[input[["list_bunches_rows_all"]],]
    
    if(!is.null(input$list_bunches_rows_selected)){
      result <- result[input$list_bunches_rows_selected,]
    }
    result = result[complete.cases(result$Crossnumber),]
    result = janitor::remove_empty(result, "cols")
      
      return(result)
  })
  
  
  output$download_bunches <- downloadHandler(
    filename = function(){paste0(input$site,"-","Bunch Harvested",Sys.time(),".csv")},
    content = function(file) {
      write.csv(downloadBunches(), file, row.names = FALSE)
    }
  )
  ######################################################################################## 
  # SEED EXTRACTION
  ######################################################################################### 
  
  seedsBox <- reactive({
    req(input$site)
    req(input$dateRange)
    result <- setDT(bananadata)[,c("Location", "Crossnumber", "Seed Extraction Date","Total Seeds")] %>%
      .[complete.cases(.),]
    result = result[anytime::anydate(`Seed Extraction Date`) %between%  c(input$dateRange[1],input$dateRange[2])]
    if(input$site != "All"){ 
      result <- result[Location == input$site & anytime::anydate(`Seed Extraction Date`) %between% c(input$dateRange[1],input$dateRange[2])]
    }
    result$`Total Seeds` = as.integer(result$`Total Seeds`)
    result 
  })

  output$n_totalseeds <- renderValueBox({
    result <- seedsBox()
    result <- sum(as.integer(na.omit(result$`Total Seeds`)))
    box1<-valueBox(value=result,
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Total seeds</b><br>", nrow(seedsBox())," Unique crosses")
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
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })
  
  downloadTotalSeeds <- reactive({ 
    result <- seedsBox()
    result <- dplyr::select(result,"Location", "Crossnumber","Seed Extraction Date","Total Seeds")
    
    result = result[input[["list_totalseeds_rows_all"]],]
    if(!is.null(input$list_totalseeds_rows_selected)){
      result <- result[input$list_totalseeds_rows_selected,]
    }
      result = result[complete.cases(result$Crossnumber),]
      result = janitor::remove_empty(result, "cols")
      
      return(result)

  })
  
  
  output$download_totalseeds <- downloadHandler(
    filename = function(){paste0(input$site,"-","Seed Extraction",Sys.time(),".csv")},
    content = function(file) {
      write.csv(downloadTotalSeeds(), file, row.names = FALSE)
    }
  )
  
  ######################################################################################## 
  # EMBRYO RESCUE
  ######################################################################################### 
  
  embryoBox <- reactive({
    req(input$site)
    req(input$dateRange)
    result <- setDT(bananadata)[,c("Location", "Crossnumber", "Embryo Rescue Date","Number of Embryo Rescued")] %>%
      .[complete.cases(.),]
    result = result[anytime::anydate(`Embryo Rescue Date`) %between% c(input$dateRange[1], input$dateRange[2])]
    if(input$site != "All"){ 
      result <- result[Location == input$site & anytime::anydate(`Embryo Rescue Date`) %between% c(input$dateRange[1], input$dateRange[2])]
    } 
    result$`Number of Embryo Rescued` = as.integer(result$`Number of Embryo Rescued`)
    result
  })
  
  output$n_rescued <- renderValueBox({
    result <- embryoBox()
    result <- sum(as.integer(na.omit(result$`Number of Embryo Rescued`)))
    
    box1<-valueBox(value=result,
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Embryo rescue</b><br>",nrow(embryoBox())," Unique crosses")
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
    
    result <- result %>% dplyr::select("Location","Crossnumber",'Number of Embryo Rescued','Embryo Rescue Date')
    
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })
  
  downloadRescued <- reactive({ 
    result <- embryoBox()
    result = result[input[["list_rescued_rows_all"]],]
    
    if(!is.null(input$list_rescued_rows_selected)){
      result <- result[input$list_rescued_rows_selected,]
    }
      
    result = result[complete.cases(result$Crossnumber),]
    result = janitor::remove_empty(result, "cols")
      
    return(result)
  })
  
  output$download_rescued <- downloadHandler(
    filename = function(){paste0(input$site,"-","Embryo rescued",Sys.time(),".csv")},
    content = function(file) {
      write.csv(downloadRescued(), file, row.names = FALSE)
    }
  )
  
  ######################################################################################## 
  # GERMINATION
  ######################################################################################### 
  
  germinationBox <- reactive({
    req(input$site)
    req(input$dateRange)
    result <- setDT(bananadata)[,c("Location", "Crossnumber","Embryo Rescue Date","Germination Date","Number of Embryo Germinating")] %>%
      .[complete.cases(.),]
    result = result[anytime::anydate(as.character(`Germination Date`)) %between% c(input$dateRange[1],input$dateRange[2])]
    if(input$site != "All"){ 
      result <- result[Location == input$site & anytime::anydate(as.character(`Germination Date`)) %between% c(input$dateRange[1],input$dateRange[2])]
    } 
    result$`Number of Embryo Germinating` = as.integer(result$`Number of Embryo Germinating`)
    
    result
    
  })
  
  output$n_germination <- renderValueBox({
    result <- germinationBox() 
    result <- sum(as.integer(na.omit(result$`Number of Embryo Germinating`)))

    box1<-valueBox(value=result,
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Number of Embryo Germinating</b><br>",nrow(germinationBox())," Unique crosses" )
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_germination"
    return(box1)
  })
  
  observeEvent(input$button_n_germination, {
    toggleModal(session, "modal_germination", "open")
  })
  output$list_germination <- DT::renderDataTable({
    result <- germinationBox()
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })
  downloadGermination <- reactive({ 
    result <- germinationBox()
    
    result = result[input[["list_germination_rows_all"]],]
    if(!is.null(input$list_germination_rows_selected)){
      result <- result[input$list_germination_rows_selected,]
    }
    
    result = result[complete.cases(result$Crossnumber),]
    result = janitor::remove_empty(result, "cols")
    
    return(result)
  })
  output$download_germination <- downloadHandler(
    filename = function(){paste0(input$site,"-","Germination",Sys.time(),".csv")},
    content = function(file) {
      write.csv(germinationBox(), file, row.names = FALSE)
    }
  )
  
  ######################################################################################## 
  # OPEN FIELD
  ######################################################################################### 
  
  openfieldBox <- reactive({
    req(input$site)
    req(input$dateRange)
    result <- setDT(plantlets)[,c("Location", "PlantletID", "Openfield Transfer Date", "Number in Openfield")] %>%
      .[complete.cases(.),]
    result = result[anytime::anydate(as.character(`Openfield Transfer Date`)) %between% c(input$dateRange[1], input$dateRange[2])]
    if(input$site != "All"){ 
      result <- result[Location == input$site & anytime::anydate(as.character(`Openfield Transfer Date`)) %between% c(input$dateRange[1], input$dateRange[2])]
    }
    result$`Number in Openfield` = as.integer(result$`Number in Openfield`)
    
    result
    
  })
  output$n_openfield_plantlets <- renderValueBox({
    result <- openfieldBox() 
    result <- sum(as.integer(na.omit(result$`#Openfield`)))
    
    box1<-valueBox(value=result,
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Plants in openfield</b><br>", nrow(openfieldBox()), " Unique plantlets")
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
    
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })
  
  downloadOpenfield <- reactive({ 
    result <- Openfield
    if(input$site != "All"){ 
      result <- dplyr::filter(plantlets, Location %in% input$site, Openfield_Transfer_Date %between% c(input$dateRange[1], input$dateRange[2]))
    }
    
    result = result[input[["list_openfield_rows_all"]],]
   
    if(!is.null(input$list_openfield_rows_selected)){
      result <- result[input$list_openfield_rows_selected,]
    } 
    result = result[complete.cases(result$PlantletID),]
    result = janitor::remove_empty(result, "cols")
    
    return(result)
    })
  
  
  output$download_openfield <- downloadHandler(
    filename = function(){paste0(input$site,"-","Plantlets_in_openfield",Sys.time(),".csv")},
    content = function(file) {
      write.csv(downloadOpenfield(), file, row.names = FALSE)
    }
  )
  

  ######################################################################################## 
  # GRAPHS
  #########################################################################################
  
  output$crossesOUT <- renderUI({
    bananadata$`First Pollination Date`=as.Date(bananadata$`First Pollination Date`)
    result <- setDT(bananadata)[,c("Location","Crossnumber","Mother", "Father","First Pollination Date")] 
    result = result[,Yearly := format(`First Pollination Date`,"%Y")]
    result = result[,Monthly := format(`First Pollination Date`,"%m")]
    result = result[,Daily := format(`First Pollination Date`,"%d")]
    result$Yearly = as.numeric(result$Yearly); result$Monthly = as.numeric(result$Monthly); result$Daily = as.numeric(result$Daily)
    
    result = result[`First Pollination Date` %between% c(input$dateRange[1], input$dateRange[2])]
    
    div(
      
      if(input$site=="All"){
        highcharter::highchartOutput("totals_site", height = 520)  %>% withSpinner(color="#0dc5c1")
       } else {
         highcharter::highchartOutput("totals", height = 520) #%>% withSpinner(color="#0dc5c1")
       }
    #}
    )
  })
  
  plotTitle <- reactive({
    if(input$site !="All"){
         paste(input$site,"Total Crosses (",input$dateRange[1],"-",input$dateRange[2],")")
          
        } else if(input$site =="All"){
          paste("Overall Total Crosses (",input$dateRange[1],"-",input$dateRange[2],")")
      }
    })
  
  js_bar_clicked <- JS("function(event) {Shiny.onInputChange('bar_clicked', [event.point.category]);}")
  
  output$totals_site <- renderHighchart({
    bananadata$`First Pollination Date`=as.Date(bananadata$`First Pollination Date`)
    result <- setDT(bananadata)[,c("Location","Crossnumber","Mother", "Father","First Pollination Date")] 
    result = result[,Yearly := format(`First Pollination Date`,"%Y")]
    result = result[,Monthly := format(`First Pollination Date`,"%m")]
    result = result[,Daily := format(`First Pollination Date`,"%d")]
    result$Yearly = as.numeric(result$Yearly); result$Monthly = as.numeric(result$Monthly); result$Daily = as.numeric(result$Daily)
    
    result = result[`First Pollination Date` %between% c(input$dateRange[1], input$dateRange[2])]
    
    if(nrow(result)==0)return(NULL)
    
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
    
    colnames(result)[3]="Number of crosses"
    
   hc = highcharter::hchart(result, "column", hcaes(x = `Time`, y = `Number of crosses`, group = Location), events = list(click = js_bar_clicked))
    
   hc %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", valueDecimals=0,
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text=plotTitle()) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  
  # Tracks the JavaScript event created by `js_bar_line`
  observe({
    dt = setDT(cleantable)
    if(input$site=="All"){
      dt = cleantable
    } else{
      dt = dt[Location==input$site]
    }
    dates = lubridate::ymd(na.omit(dt$Date))
    updateDateRangeInput(session, "dateRange", start =  min(dates), end = max(dates), min = min(dates),max = max(dates)
    )
  })
  
  
  output$totals <- renderHighchart({
    req(input$site)
    result <- bananadata %>%
      dplyr::select(Location,Crossnumber,Mother, Father,`First Pollination Date`) %>%
      dplyr::mutate(day = lubridate::day(`First Pollination Date`),
                    month = lubridate::month(`First Pollination Date`),
                    year = lubridate::year(`First Pollination Date`)) %>%
      .[complete.cases(.),]
    
    result = result %>% 
      dplyr::filter(Location %in% input$site, 
                    `First Pollination Date`  %between% c(input$dateRange[1],input$dateRange[2]))
    
     if((input$dateRange[2] - input$dateRange[1]) < 31){
       result = result %>%
         dplyr::group_by(day) %>% 
         dplyr::tally()  %>%
         collect()
       
       grp = result$day
       group_name = "Daily"
     } else if((lubridate::year(input$dateRange[2]) == lubridate::year(input$dateRange[1])) & (input$dateRange[2] - input$dateRange[1]) > 31){
       result = result %>% 
         dplyr::group_by(month) %>% 
         dplyr::tally()  %>%
         collect()
       
       grp = month.abb[result$month]
       group_name = "Monthly"
     }  else if((lubridate::year(input$dateRange[2]) != lubridate::year(input$dateRange[1]))){ 
       result  = result %>% 
         dplyr::group_by(year) %>% 
         dplyr::tally() %>%
         collect()
       
       grp = result$year
       group_name = "Yearly"
     }
    
    colnames(result)[2]="Number of crosses"
    
    # PLOT POLLINATION
    
    highchart() %>%
      hc_add_series(
        data = result$`Number of crosses`, 
        type = "column",
        name = group_name,
        events = list(click = js_bar_clicked)) %>%
      hc_xAxis(
        categories = grp,
        tickmarkPlacement="on")
   
    
  })
  
  
  output$totalcrosses <- renderUI({
    result <- bananadata %>%
      dplyr::select(Location,Crossnumber,Mother, Father,`First Pollination Date`) %>% 
      .[complete.cases(.),]
    result = result %>% 
      dplyr::filter(`First Pollination Date` %between% c(input$dateRange[1],input$dateRange[2]))
    
    if(nrow(result)==0)return(NULL)
    
    if(input$site !="All" && (input$dateRange[2] - input$dateRange[1]) < 31){
      result <- result %>%
        dplyr::filter(Location %in% input$site,
               `First Pollination Date`  %between% c(input$dateRange[1], input$dateRange[2]))
      
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

    if(input$site !="All"){
      result = result %>%
        dplyr::filter(Location == input$site)
    }
      
    result
  })
  
  output$motherOUT<- renderUI(
    div(
      if(is.null(parentsInput())){
        div(br(), br(), br(),br(), br(), br(),
            tags$p(style = "color: green; font-size: 28px; text-align: center;","No records to display")
        )
      } else {
        div(
          highchartOutput("mother", height = 520) %>% withSpinner(color="#0dc5c1")
        )
      }
    )
  )

  js_female_bar_clicked <- JS("function(event) {Shiny.onInputChange('female_bar_clicked', [event.point.category]);}")
  
   output$mother <- renderHighchart({
     
     if(is.null(parentsInput())){return(NULL)}
     else {
     result <- parentsInput() %>%
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
         
     # paste0("<a href='",result$FemaleGermplasmDbId,"'>",'photo',"</a>")
     highchart() %>%
       hc_add_series(data = result$n,type = "bar", name = paste("Female plants"), events = list(click = js_female_bar_clicked)) %>%
       hc_xAxis(categories = result$Mother) %>%
       hc_exporting(enabled = TRUE) %>% 
       hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", valueDecimals=0,
                  shared = TRUE, borderWidth = 2) %>%
       hc_title(text=ptitle) %>%
       hc_add_theme(hc_theme_elementary())
     }
   })
   
   
   output$totalFemales <- renderUI({
     if(is.null(parentsInput())){return(NULL)}
     else {
     result <- parentsInput() %>% 
       dplyr::summarize(dplyr::n_distinct(Mother))
     paste0("N = ", result)
     }
   })
   
   
   # on female click, go to 'Data' tab
   observeEvent(input$female_bar_clicked, {
     updateTabsetPanel(session, "inTabset",
                       selected = "Data")
   })
   
   
   #.....................................................................................MALES 
   
   output$fatherOUT <- renderUI({
     div(
       if(is.null(parentsInput())){
         div(br(), br(), br(),br(), br(), br(),
             tags$p(style = "color: green; font-size: 28px; text-align: center;","No records to display")
         )
       } else {
         highchartOutput("father", height = 520) %>% withSpinner(color="#0dc5c1")
       }
     )
   })
   
   js_male_bar_clicked <- JS("function(event) {Shiny.onInputChange('male_bar_clicked', [event.point.category]);}")
   
   output$father <- renderHighchart({
     
     if(is.null(parentsInput())){return(NULL)}
     else {
     result <- parentsInput()%>%
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
       hc_add_series(data = result$n, type = "bar",name = paste("Male plants"),  events = list(click = js_male_bar_clicked)) %>%
       hc_xAxis(categories = result$Father,tickmarkPlacement="on") %>%
       hc_exporting(enabled = TRUE) %>% 
       hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                  shared = TRUE, borderWidth = 2) %>%
       hc_title(text=ptitle) %>%
       hc_add_theme(hc_theme_elementary())
     }
   })
   
  
   output$totalMales <- renderUI({
     if(is.null(parentsInput())){return(NULL)}
     else {
     result <- parentsInput() %>% 
       dplyr::summarize(dplyr::n_distinct(Father))
     paste0("N = ", result)
     }
   })
   
   # on male click, go to 'Data' tab
   observeEvent(input$male_bar_clicked, {
     updateTabsetPanel(session, "inTabset",
                       selected = "Data")
   })
   
  })
)
