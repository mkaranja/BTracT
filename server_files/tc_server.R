library(WriteXLS)
library(dplyr)
library(DT)
library(bsplus)
# source("ui_files/tc_page.R")

tc_server <- function(env_serv) with(env_serv, local({
  output$user <- reactive({session$user})

  # controls
  sitesIn <- reactive({
      if(input$tc_tabs=="Crosses"){
        if(exists("banana_labels")){
        as.list(unique(banana_labels$Location))
        }
        
      } else if(input$tc_tabs=="Embryo Germinating"){
        if(exists("embryo_labels")){
        as.list(unique(embryo_labels$Location))
        }
      } else if(input$tc_tabs=="Subcultures"){
        if(exists("plantlet_labels")){
        as.list(unique(plantlet_labels$Location))
        }
      }
  })
  
  output$tc_controls <- renderUI({
    if(input$tc_tabs!="How to do it"){
      div(
          tags$head(tags$style(
            HTML('
                 #controls {background-color: rgba(0,0,255,0.2);;}
                 #controls {background-color: rgba(0,0,255,1);}')
            )),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      width = 330, height = "auto", br(),
                      h4("TC Label Management"),
                      selectInput(inputId = 'tc_site',label = 'Select site:',choices = c("None",sitesIn())),
                      if(input$tc_tabs=="Crosses"){
                        prettySwitch(inputId = "three_per_tube",label = "3-Embryos/ Test Tube", status = "success", fill = TRUE)
                      },
                      uiOutput('ncopiesOut'), br(),
                      downloadBttn("downloadInfo","Download", size = "sm",style = "unite"),
                      br(),
                      p("To subset records to download, select specific data table rows")
        )
      )
    }
  })
  
  output$ncopiesOut <- renderUI({
    if(input$three_per_tube==FALSE){
      numericInput(inputId='n', label='Copies', value=1, max = 10)
    }
  })
  
  output$crossesdt <- DT::renderDataTable({
    req(input$tc_site)
    req(input$n)
    
    result = banana_labels %>%
      dplyr::filter(Location == input$tc_site)
    result = result %>% dplyr::select(-c(Prefix,Suffix))
    
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })
  
  
  
  output$embryodt <- DT::renderDataTable({
    req(input$tc_site)
    req(input$n)
    
    if(exists("embryo_labels")){
    result <- embryo_labels  %>%
      dplyr::filter(Location == input$tc_site, sum(!is.na(`Germination Date`))>0) %>%
      dplyr::select(PlantletID, `Germination Date`)
    #result = purrr::map_df(seq_len(input$n), ~result)
    
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
    } else {
      return(NULL)
    }
  })
  
  
  output$subculresdt <- DT::renderDataTable({
    req(input$tc_site)
    req(input$n)
    if(exists("plantlet_labels")){
    result = plantlet_labels %>%
      filter(Location == input$tc_site)
    result = result %>%
      dplyr::select(Location, PlantletID,`Subculture Date`,Copies)
    #result = purrr::map_df(seq_len(input$n), ~result)
    
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
    } else {
      return(NULL)
    }
  })
  
  downloadInput <- reactive({
    req(input$tc_site)
    req(input$n)
    if(input$tc_tabs=="Crosses"){
      result = banana_labels  %>%
        dplyr::filter(Location == input$tc_site, sum(!is.na(`Embryo Rescue Date`))>0) %>%
        dplyr::select(Crossnumber, Prefix, Suffix, `Number of Embryo Rescued`)
      
      s = input$crossesdt_rows_selected
      if(length(s)){
        result <- result[s,]
      }
      
    }
    
    if(input$tc_tabs=="Embryo Germinating"){
      result <- embryo_labels  %>%
        dplyr::filter(Location == input$tc_site, sum(!is.na(`Germination Date`))>0) %>%
        dplyr::select(PlantletID, Prefix, Suffix, EmbryoNo)
      s = input$embryodt_rows_selected
      if(length(s)){
        result <- result[s,]
      }
    }
    
    if(input$tc_tabs=="Subcultures"){
      result <- plantlet_labels  %>% 
        dplyr::select(PlantletID, Prefix, Suffix, EmbryoNo)
      
      s = input$subculresdt_rows_selected
      if(length(s)){
        result <- result[s,]
      }
    }
    
    if(input$three_per_tube==FALSE){
    result = purrr::map_df(seq_len(input$n), ~result) %>% # repricate
      .[order(.[,1]),]
    } else {
      result$`Number of Embryo Rescued` = as.integer(result$`Number of Embryo Rescued`)
      
      for(i in 1:nrow(result)){
        if(result$`Number of Embryo Rescued`[i]>3){
          result$n[i] = (result$`Number of Embryo Rescued`[i])/3
        } else{
          result$n[i] = 1
        }
        
      }
      
      result$n = ceiling(result$n) 
      result = result[rep(row.names(result), result$n),1:4]
    }
    # return
    return(result[,c('Crossnumber', 'Prefix', 'Suffix')])
    
  })
  
  fileName <- reactive({
    if(input$tc_tabs=="Crosses"){
      name = paste0(input$tc_site,"-crosses",Sys.time(), ".xls", sep="")
    }
    if(input$tc_tabs=="Embryo Germinating"){
      name = paste0(input$tc_site,"-germinating-embryo",Sys.time(), ".xls", sep="")
    }
    if(input$tc_tabs=="Subcultures"){
      name = paste0(input$tc_site,"-subcultures",Sys.time(), ".xls", sep="")
    }
    return(name)
  })
  
  # DOWNLOAD
  output$downloadInfo <- downloadHandler(
    filename = function(){
      fileName()
    },
    content = function(file) {
      WriteXLS::WriteXLS(downloadInput(), ExcelFileName = file)
    }
  )
  
  
  # REPLACE BARCODE
  output$scannedLabel <- DT::renderDataTable({
    filter(accessionDT, Accession == input$scanBarcode)
  })

  output$downloadscannedLabel <- downloadHandler(
    filename = function(){
      paste0("barcode_data",Sys.time(), ".xls", sep="")
    },
    content = function(file) {
      WriteXLS::WriteXLS(accessionDT, ExcelFileName = file)
    }
  )

  # search label
  output$typedLabel <- DT::renderDataTable({
    row.names(accessionDT) = NULL
    accessionDT
  })
}))
