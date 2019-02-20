#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(shinysky)

source("ui_files/data_page.R")
source("ui_files/overview_page.R")
source("ui_files/status_page.R")
source("ui_files/front_page.R")
source("ui_files/about_page.R")

# Define UI for application

navbarPageWithSearchBox <- function(...,menu, inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form; navbar-right",br(), inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

shinyUI(
  navbarPageWithSearchBox("", position = "fixed-top", theme = shinythemes::shinytheme("readable"),
  
  tabPanel("BTracT",
           frontpage()
           ),
  tabPanel("Overview",
           br(),br(),br(),
           includeCSS("www/AdminLTE.css"), # for activating shinydashboard/plus widgets
            overviewPage()
           ),
  tabPanel("Data",
           br(),br(),br(),
           dataPage()
           ),
  tabPanel("Status",
           br(),br(),br(),
           statuspage()
           ),
  tabPanel("About",
           br(),br(),br(),
           helppage()
           ),
  inputs = div(shinysky::textInput.typeahead(id="search", placeholder="", local = data.frame(name=searchlist), valueKey = "name", tokens=c(1:length(searchlist)),
                                             template = HTML("<h6 class='repo-name'>{{name}}</h6>")), 
               actionBttn("go_search",label = "Go",style = "gradient", size = "sm")
    )
  )
)