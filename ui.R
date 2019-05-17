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
library(shinyjs)
library(rmarkdown)
library(shinycssloaders)

source("ui_files/data_page.R")
source("ui_files/overview_page.R")
source("ui_files/status_page.R")
source("ui_files/front_page.R")
source("ui_files/about_page.R")
source("ui_files/labels_page.R")
source("ui_files/docs_page.R")


navbarPageWithSearchBox <- function(..., menu, refresh) {
  
  navbar <- navbarPage(...)
  butt <- tags$form(class = "navbar-form; navbar-left",br(), refresh)
  #log <- tags$form(class = "navbar-form; navbar-right",br(), log0ut)
  #form <- tags$form(class = "navbar-form; navbar-right",br(), inputs)
  
  # navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
  #   navbar[[3]][[1]]$children[[1]], log)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], butt)
  # navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
  #   navbar[[3]][[1]]$children[[1]], form)
  
  
  
  navbar
}

shinyUI(
  
 navbarPageWithSearchBox("",id = "inTabset", theme = shinythemes::shinytheme("readable"), position='fixed-top',
                         
        tabPanel("BTracT",
                 frontpage()
                 ),
        tabPanel("Overview",
                br(),br(),br(),
                 includeCSS("www/AdminLTE.css"), # for activating shinydashboard/plus widgets
                  overviewPage()
                 ),
        tabPanel("Data",
                br(),br(),br(),br(),br(),
                 dataPage()
                 ),
        tabPanel("Status",
                br(),br(),br(),br(),br(),
                 statuspage()
                 ),
        navbarMenu("About",
                   tabPanel("TC Label Management",
                            br(),br(),br(),br(),br(),
                            tc_page()),
                   
                   tabPanel("Download barcodes",
                            br(),br(),br(),br(),
                            labels()
                   ),
                   tabPanel("Docs",
                            br(),br(),br(),br(),
                            docs()
                            )#,
                   # tabPanel(
                   #          a("Using BTracT", href="docs/usingbtract.html", target="_blank", icon=icon("note"))
                   #          
                   # )
                 ),
        
        # log0ut = div(
        #   actionBttn("logout", size="sm", style="jelly", color="success",label = "", block = T,
        #              icon=icon("sign-out-alt", lib = "font-awesome"))
        # ),
         # inputs = div(
         #    shinysky::textInput.typeahead(id="search", placeholder="search", 
         #                                 local = data.frame(name=searchlist), 
         #                                 valueKey = "name", 
         #                                 tokens=c(1:length(searchlist)),
         #                                 template = HTML("<p class='repo-name'>{{name}}</p>")), 
         #              actionBttn("go_search",label = "", icon = icon("search"), color="primary", style = "material-circle", size = "xs", block = T,)
         # ),
        refresh = div( #style='padding:4px; width:1500%; font-size:150%')
              actionBttn("refresh", size="xs", style="pill", color="danger",label = "",
                               icon=icon("refresh", lib = "font-awesome"))
          )
  )
)
