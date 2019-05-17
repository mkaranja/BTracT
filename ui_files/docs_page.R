

source("ui_files/feedback_page.R")

docs <- function()tagList(
  
  sidebarLayout(
    sidebarPanel(width = 2, style = "position:fixed;width:inherit;",
                 actionButton(inputId = 'btractTool',label = 'Data Collection Tool', width = "100%", onclick = "window.open('docs/usingbtract.html')"), br(),br(),
                 actionButton(inputId = 'go_dashboard',label = "Using this Dashboard", onClick='$("html,body").animate({scrollTop:$("#dashboard").offset().top},500)', width="3000px"),br(),br(),
                 actionButton(inputId = 'go_feedback',label = "Feedback", onClick='$("html,body").animate({scrollTop:$("#feedback").offset().top},500)', width="3000px"),br(),br(),
                 actionButton(inputId = 'go_acknowledgement',label = "Acknowledgement", onClick='$("html,body").animate({scrollTop:$("#acknowledgement").offset().top},500)', width="3000px"),br(),br()
                 #tags$div(style='text-align:left;',tags$button(type='button',onClick='$("html,body").animate({scrollTop:$("#feedback").offset().top},500)', width="3000px",class='btn btn-default','Feedback')),br(),br()
                 #tags$div(style='text-align:left;',tags$button(type='button',onClick='$("html,body").animate({scrollTop:$("#acknowledgementks").offset().top},500)', width="3000px",class='btn btn-default','Acknowledgements'))
          
    ), # onclick ="window.open('http://google.com', '_blank')")
    
    mainPanel(
           wellPanel(
             h4(id = 'dashboard',"Using this Dashboard"),
             p("This interactive web application has been designed to provide and easy-to-use interface to visualise crosses data"), br(),
             
             tags$blockquote("Info boxes"),
             p("These are clickable to show detailed information in a datatable"),
             h5("Graphs"),
             h5("Data Tables"),
             h5('Download data'),
             p("There are 3 options of downloading data in datatable"),
             tags$ul(
               tags$li("1. Click download button to download everything in the table"),
               tags$li("2. Select the rows of your interest and click the download button"),
               tags$li("3. USe the filters to subset your data, then click on download button")
             ), 
            
              h5("Status Tab"),
             h5("About Tab"),
             
             h4(id = 'feedback', "Feedback"),
              feedback(), br(), br(),
             
             h4(id = 'acknowledgement', "Acknowledgement")
          )
        )
    )
)