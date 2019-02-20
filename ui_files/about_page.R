library(shiny)
library(shinydashboardPlus)
source("ui_files/feedback_page.R")
helppage = function() tagList(
  
  div(
    column(8, offset = 2,
      
      ## USING DASHBOARD 
      
      h4(tags$a(href = "documentation/using_btract_dashboard.html", style = "color: #FF8C00; text-align:center;", target = "blank","Using Dashboard")),
      
      p("BTracT (Banana Tracking Tool) dashboard is the vizualization and analytic tool of banana crosses and seeds"),
      tags$a(href = "documentation/using_btract_dashboard.html", target = "blank",p(" read more...")), br(),
      
      div(
        div(style="display: inline-block;vertical-align:top; width: 50px;",p("Code")),
        div(style="display: inline-block;vertical-align:top; width: 50px;",socialButton(url = "https://github.com/mkaranja/BTracT-Dashboard", type = "github")) # shinydashboardplus,
        
      ),  
      
      
      br(), br(),
      
      
      ## Data collection tool
      
      h4(tags$a(href = "documentation/datacollectiontool.html", style = "color: #FF8C00; text-align:center;", target = "blank","Data collection tool")),
      
      p("BTracT has been developed using", tags$a(href = "https://www.opendatakit.org/","Open data kit")," (ODK) framework that allows mobile and remote data collection."), 
        
      
      p("Setting up and using ", tags$a(href = "documentation/datacollectiontool.html", target = "blank", "BTracT data collection tool")), br(),
      div(
        div(style="display: inline-block;vertical-align:top; width: 50px;",p("Code")),
        div(style="display: inline-block;vertical-align:top; width: 50px;",socialButton(url = "https://github.com/mkaranja/BTracT-Form", type = "github")) # shinydashboardplus,
        
      ),br(),br(),
      
      ## FFEDBACK
      h4(style = "color: #FF8C00; text-align:left;","Feedback"),
      
      #h4(actionLink(inputId = "feedback",style = "color: #FF8C00; text-align:center;", "Feedback"),
      p("Future developments are made possible by providing feedbacks and comments. To leave your feedback or comments "),
    
      actionLink(inputId = "feedback", p("go..")),
      # bsModal("mod_feedback","","btn",size = "large",
      #         feedback()
      # ),
      
      
      br(),br(),
      
      # ACKNOWLEDGEMENT
      h4(tags$a(href = "documentation/acknowledgements.html", style = "color: #FF8C00; text-align:center;",target = "blank","Acknowledgements")),
      p("This system is as a result of efforts of many people working in different Institutions"),
      
      tags$ol("1. International Institute of Tropical Agriculture (Nairobi, Arusha, Uganda, Ibadan and Onne"),
      tags$ol("2. Boyce Thomson Institute, Cornell University, USA"),
      
      br(), br(), br()
      
      )
  )
)