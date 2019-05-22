

source("ui_files/feedback_page.R")

docs <- function()tagList(
  navlistPanel(fluid = F, selected = 'Dashboard', widths = c(3,8),
               tabPanel(a("Using BTracT (app)", href="docs/usingbtract.html", target="_blank", icon=icon("note"))),
               tabPanel("Dashboard", #a("Dashboard",href="docs/dashboard.html", target="_blank", icon=icon("note"))
                        column(8,
                          includeMarkdown("www/docs/dashboard.Rmd")
                        )
               ),
               tabPanel("Feedback",
                        feedback(), br(), br()
               ),
               tabPanel("References"
                        
               )
  )
)

