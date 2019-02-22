library(WriteXLS)
library(dplyr)
library(DT)
source("ui_files/tc_page.R")

tc_server <- function(env_serv) with(env_serv, local({
  output$user <- reactive({session$user})

  output$newWindowContent <- renderUI({
    tc_page()
  })
  
  # SAMPLE BARCODE
  output$barcodeImage <- renderPlot({
    #req(input$barcodeSet)

    library(qrencoder)
    par(mar=c(4,4,4,4), oma=c(.5,.5,.5,.5))
    image(qrencode_raster(as.character(banana_labels[1,1])),main=as.character(banana_labels[1,1]),
          asp=1, col=c("white", "black"), axes=FALSE,
          xlab="", ylab="",cex.main=1.8)
  })
#-----------------------------CROSSES---------------------------------------------------------------
  # VIEW crossnumber
  output$crosses <- DT::renderDataTable({
    result = banana_labels %>%
      dplyr::filter(Location == input$crosses_site)
    result = result %>% dplyr::select(-c(Prefix,Suffix))
    DT::datatable(result,
                    style = 'bootstrap', rownames = FALSE,
                    filter = list(position = 'top'),
                    options = list(pageLength = 10, dom = 'Bfrtip', autoWidth = T, searchHighlight=T, stateSave = TRUE,
                                   rowCallback = JS( 'function(row, data) { $("td:eq(5)", row).css("text-align", "center"); }'),
                                   buttons = list(I('colvis')
                                   ))
                    )
  })

  # DOWNLOAD crossid labels
  crossIDSelected <- reactive({
    result = banana_labels  %>%
       dplyr::filter(Location == input$crosses_site, sum(!is.na(`Embryo Rescue Date`))>0)

    if(!is.null(input$crossIDTable_rows_selected)){
      result <- result[input$crosses_rows_selected,]
    } else {
      result <- result[input$crosses_rows_all,]
    }
    result %>%
      dplyr::select(Crossnumber, Prefix, Suffix)
  })

  output$downloadcrossID <- downloadHandler(
    filename = function() {
      paste0(input$crosses_site,"-crosses",Sys.time(), ".xls", sep="")
    },
    content = function(file) {
      WriteXLS::WriteXLS(crossIDSelected(), ExcelFileName = file)
    }
  )

#-------------------------------------SEEDS--------------------------------------------------------------
  # VIEW seeds
  output$seedsIDTable <- DT::renderDataTable({
    if(!exists("seeds_labels")){
      return(NULL)
    }
    result = seeds_labels %>%
      filter(Location == input$seeds_site)
    resut = result %>%
      dplyr::select(-c(Prefix, Suffix))

    DT::datatable(result,
                  style = 'bootstrap', rownames = FALSE,
                  filter = list(position = 'top'),
                  options = list(pageLength = 10, dom = 'Bfrtip', autoWidth = T, searchHighlight=T, stateSave = TRUE,
                                 rowCallback = JS( 'function(row, data) { $("td:eq(5)", row).css("text-align", "center"); }'),
                                 buttons = list(I('colvis')
                                 ))
    )
  })
  seedsIDSelected <- reactive({
    result <- seeds_labels
    if(!is.null(input$seedsIDTable_rows_selected)){
      result <- seeds_labels[input$seedsIDTable_rows_selected,]
    }
    result %>%
      dplyr::select(`Seed Id`, Prefix, Suffix, SeedNo)
  })
  # DOWNLOAD seeds labels
  output$downloadseedsID <- downloadHandler(

    filename = function(){
      paste0(input$seeds_site,"-seeds-germinating-after-8-weeks",Sys.time(), ".xls", sep="")
    },
    content = function(file) {
      WriteXLS::WriteXLS(seedsIDSelected(), ExcelFileName = file)
    }
  )

# ------------------------PLANTLETS------------------------------------------------
  # VIEW subcultures

  output$subculresIDTable <- DT::renderDataTable({
    if(!exists("plantlet_labels")){
      return(NULL)
    }
    result = plantlet_labels %>%
      filter(Location == input$subs_site)
    result = result %>%
      dplyr::select(-c(Prefix, Suffix, SubNo))

    DT::datatable(result,
                  style = 'bootstrap', rownames = FALSE,
                  filter = list(position = 'top'),
                  options = list(pageLength = 10, dom = 'Bfrtip', autoWidth = T, searchHighlight=T, stateSave = TRUE,
                                 rowCallback = JS( 'function(row, data) { $("td:eq(5)", row).css("text-align", "center"); }'),
                                 buttons = list(I('colvis')
                                 ))
    )
  })

  subculturesIDSelected <- reactive({
    if(!is.null(input$subculresIDTable_rows_selected)){
      result <- plantlet_labels[input$subculresIDTable_rows_selected,]
    } else {
      result <- plantlet_labels
    }
    result %>%
      dplyr::select(Plantletid, Prefix, Suffix, SubNo)
  })

  # DOWNLOAD
  output$downloadsubculresID <- downloadHandler(
    filename = function(){
      paste0(input$subs_site,"-plantlets",Sys.time(), ".xls", sep="")
    },
    content = function(file) {
     WriteXLS::WriteXLS(subculturesIDSelected(), ExcelFileName = file)
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
