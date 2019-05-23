#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source("server_files/overview_server.R")
source("server_files/data_server.R")
source("server_files/status_server.R")
source("server_files/feedback_server.R")
source("server_files/searchbox.R")
source("server_files/tc_server.R")
source("server_files/labels_server.R")


# Define server logic 
shinyServer(
  function(input, output, session) {
  shinyURL.server()
  # Automatically stop a Shiny app when closing the browser tab
  #session$onSessionEnded(stopApp) 
   
 
  # Simulate work being done for 1 second
  Sys.sleep(2)
  
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")

  
  env_serv = environment()
  
  # home page slide images
  # output$slickr <- renderSlickR({
  #   imgs <- list.files("www/sliders/", pattern=".png", full.names = TRUE)
  #   slickR(imgs)
  # })
  
  # searchbox
  searchbox(env_serv)
  
  # overview page
  overviewserver(env_serv)
  
  # data page
  dataserver(env_serv)
  
  # status page
  statusserver(env_serv)
  
  # tissue culture labels
  tc_server(env_serv)
  
  # feedback
  feedbackserver(env_serv)
  
  # labels
  labels(env_serv)
  
  # Update data
  # Docs
  
  #withMathJax(includeMarkdown("www/docs/usingbtract.Rmd"))
  session$allowReconnect("force")
  
  observeEvent(input$refresh, {
     session$reload()
   })
})
