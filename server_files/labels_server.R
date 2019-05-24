
labels <- function(env_serv) with(env_serv, local({
  
  # only today's data should be displayed
  
  labelsInput <- reactive({
    bananadata[,c("Location","Crossnumber","FemalePlotName","Mother","MalePlotName","Father","First Pollination Date")]
  })
  
  output$labelsDT <- DT::renderDataTable({
    DT::datatable(labelsInput(), filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })
  
  
  
    output$barcodeImage <- renderPlot({
         
         par(mar=c(1,1,1,1), oma=c(.5,.5,.5,.5))
         image(qrencode_raster(as.character(labelsInput()[1,2])),
               asp=1, col=c("white", "black"), axes=F, 
               xlab="", ylab="")
        })
         
    
    downloadlabelsIn <- reactive({
      result = labelsInput()
      if(!is.null(input$labelsDT_rows_selected)){
        result = result[input$labelsDT_rows_selected,]
      }
      result
    })
    
    output$labelformat <- renderUI({
      if(input$format=='Pre-defined'){
        div(
        shinyWidgets::prettyRadioButtons(inputId = "preformat",label = "By site:", choices = c("Arusha", "Sendusu"),
                                         icon = icon("check"), bigger = F, status = "warning"), br()
        )
      } else if(input$format=='Custom'){
        div(
          selectInput('pagesize','Paper size:', c('A4','US','Letter')),br(),
          p("Width and height of the graphics region in inches. Default is 7"),
          column(6,numericInput('pheight',"Page Height", value = 5)), 
          column(6,numericInput('pwidth',"Page Width", value=5)), br(),
          column(6,numericInput('rows',"Rows", value = 1)), 
          column(6,numericInput('cols',"Columns", value=1)), br(),br()
            )
      }
    })
    
    
    # page size
    
    pgsize <- reactive({
      switch(input$pagesize,
             'A4'='a4',
             'Letter'='letter',
             'US'='us'
             )
    })
    paperIn <- reactive({
      if(input$format=='Pre-defined'){
        if(input$preformat=='Arusha'){pgsize = NULL}
        else if(input$preformat=='Sendusu'){pgsize = 'letter'}
      }else if(input$format=='Custom'){
        pgsize = pgsize()
      }
      return(pgsize)
    })
    
    # pgsize
    pgheight <- reactive({
      if(input$format=='Pre-defined'){
        if(input$preformat=='Arusha'){pgheight = 5}
        else if(input$preformat=='Sendusu'){pgheight = 10.5}
      }else if(input$format=='Custom'){
        pgheight = input$pheight
      }
      return(pgheight)
    })
    
    # page width
    pgwidth <- reactive({
      if(input$format=='Pre-defined'){
        if(input$preformat=='Arusha'){pgwidth = 5}
        else if(input$preformat=='Sendusu'){pgwidth = 8}
      }else if(input$format=='Custom'){
        pgwidth = input$pwidth
      }
      return(pgwidth)
    })
    
    # columns
    pgcols <- reactive({
      if(input$format=='Pre-defined'){
        if(input$preformat=='Arusha'){cols=1}
        else if(input$preformat=='Sendusu'){cols=2}
      }else if(input$format=='Custom'){
        cols = input$cols
      }
      return(cols)
    })
    
    # number of rows
    pgrows <- reactive({
      if(input$format=='Pre-defined'){
        if(input$preformat=='Arusha'){rows=1}
        else if(input$preformat=='Sendusu'){rows=5}
      }else if(input$format=='Custom'){
        rows=input$rows
      }
      return(rows)
    })
    
    # outer margins
    label_oma <- reactive({
      if(input$format=='Pre-defined'){
        if(input$preformat=='Arusha'){oma=c(1,1,1,1)}
        else if(input$preformat=='Sendusu'){oma=c(.1,0.2,0.1,0.2)}
      }else if(input$format=='Custom'){
        oma=c(1,1,1,1)
      }
      return(oma)
    })
    
    # margins
    label_mar <- reactive({
      if(input$format=='Pre-defined'){
        if(input$preformat=='Arusha'){mar=c(5,5,5,5)}
        else if(input$preformat=='Sendusu'){mar=c(2,2,2,2)}
      }else if(input$format=='Custom'){
        mar=c(2,2,2,2)
      }
      return(mar)
    })
    
    # title size
    
    
    output$downloadlabels <- downloadHandler(
       filename = "barcodelabels.pdf",
       
         content = function(file) {
           pdf(file, width=pgwidth(), height = pgheight(), paper = paperIn(), pagecentre=F)
           par(mfrow=c(pgrows(),pgcols()),mar=label_mar(), oma=label_oma()) # margins: mar=c(b,l,t,r); oma=c(b,l,t,r)
           for(i in 1:(nrow(downloadlabelsIn()))){
               image(qrencode_raster(as.character(downloadlabelsIn()[i,input$label_value])), 
                     main = as.character(downloadlabelsIn()[i,input$label_value]), sub = as.character(downloadlabelsIn()[i,input$label_label]),
                           cex.main = 1.5, cex.sub = 1, asp=1, col=c("white", "black"), axes=F, 
                     xlab="", ylab="")
             }
           dev.off()
         }
       )
})
)