
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

shinyServer(function(input, output) {
  
  data_selected_region<-reactive({
    cleantable[cleantable$Location%in%input$region,]
  })
  
  output$threemap_population <- renderPlot({ 
    
    par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
    plot(c(0,1), c(0,1),axes=F, col="white")
    vps <- baseViewports()
    
    temp=data_selected_region() %>%
      dplyr::group_by(Activity) %>%
      dplyr::tally() %>%
      dplyr::collect()
    
    .tm <<- treemap(temp, 
                    index="Activity", 
                    vSize="n", 
                    vColor="n",
                    type="value",
                    title = "",
                    palette="Blues",
                    border.col ="white",
                    position.legend="right",
                    fontsize.labels = 16,
                    title.legend="")
  })
  
  
  treemap_clicked <- reactiveValues(
    center = NULL,
    for_condition=NULL
  )
  
  # Handle clicks on treemap by country
  observeEvent(input$click_treemap, {
    x <- input$click_treemap$x
    y <- input$click_treemap$y
    treemap_clicked$center <- c(x,y)
    
    if(is.null(treemap_clicked$for_condition)){
      treemap_clicked$for_condition=c(x,y)
    }
    else{treemap_clicked$for_condition=NULL}
  })
  
  getRecord_population <- reactive({
    x <- treemap_clicked$center[1]
    y <- treemap_clicked$center[2]
    
    x <- (x - .tm$vpCoorX[1]) / (.tm$vpCoorX[2] - .tm$vpCoorX[1])
    y <- (y - .tm$vpCoorY[1]) / (.tm$vpCoorY[2] - .tm$vpCoorY[1])
    
    
    l <- tmLocate(list(x=x, y=y), .tm)
    z=l[, 1:(ncol(l)-5)]
    
    
    if(is.na(z[,1]))
      return(NULL)
    
    col=as.character(z[,1])
    
    filter(cleantable,Activity==col)
  })
  
  output$zoomout = renderUI({
    actionButton("refresh", em("Go to the previous page", style="text-align:center;color:red;font-size:200%"))
  })
  
  refresh=reactive({
    input$refresh
  })
  
  condition1<-reactive({
    
    refresh=refresh()
    
    if(is.null(treemap_clicked$for_condition) & refresh==0){
      result=1}else if((refresh%%2==0) & !is.null(treemap_clicked$for_condition)){
        result =0
      }else if((refresh%%2!=0) & !is.null(treemap_clicked$for_condition)){
        result =1
      }else if((refresh%%2!=0) & is.null(treemap_clicked$for_condition)){
        result =0
      }else if((refresh%%2==0) & is.null(treemap_clicked$for_condition)){
        result =1
      }
  })
  
  
  output$condition1 <- renderText({
    condition1()
  })
  
  outputOptions(output, 'condition1', suspendWhenHidden=FALSE)
  
  
  output$population_time_series<-renderTable({
    head(getRecord_population())
  })
  
})