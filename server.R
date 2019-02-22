#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("server_files/overview_server.R")
source("server_files/data_server.R")
source("server_files/status_server.R")
source("server_files/feedback_server.R")
source("server_files/searchbox.R")
source("server_files/tc_server.R")
# Define server logic 
shinyServer(
  function(input, output, session) {
  # session$onSessionEnded(stopApp) # Automatically stop a Shiny app when closing the browser tab
   
  env_serv = environment()
  
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
})
