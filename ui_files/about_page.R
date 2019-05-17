
source("ui_files/feedback_page.R")
#source("ui_files/tissue_culture_page.R")


dashboard  = function() tagList(
  div(
    column(6, offset = 3,
          fluidRow(
           includeMarkdown("www/documentation/using_btract_dashboard.Rmd")
            ),
          
          fluidRow(
           ## FFEDBACK
            h4(style = "color: #FF8C00; text-align:left;","Feedback"),
            p("Future developments are made possible by providing feedbacks and comments. To leave your feedback or comments "),
            actionLink(inputId = "feedback", p("go..")),
            br()
            
          ),
          
          fluidRow(
            # ACKNOWLEDGEMENT
            h4(style = "color: #FF8C00; text-align:left;","Acknowledgements"),
            p("This system is as a result of combined efforts of many people working in different Institutions"),
            
            tags$ol("1. International Institute of Tropical Agriculture (Nairobi, Arusha, Uganda, Ibadan and Onne"),
            tags$ol("2. Boyce Thomson Institute, Cornell University, USA"),
            
            br(), br()
          )
    )
           
  )
)

datacollection = function() tagList(
    
   div(
     column(6, offset = 3,
            includeMarkdown("www/docs/clean.Rmd")
         )
   )
)


tc_page = function() tagList(
  div(
    includeCSS("abstyles.css"),
    tags$p(style = "color: #FF8C00; font-size: 28px; text-align: center;","Tissue Culture Label Management"), br(),
    
    uiOutput("tc_controls"),
    
    navlistPanel(id = "tc_tabs",widths = c(2, 7), well = TRUE, #selected = "Crosses",
                 tabPanel("How to do it",
                          tags$video(id="video2", type = "video/mp4",src = "printing_tc_labels.mp4", height="600px", width="100%", controls = "controls"), br(),
                          p("Watch this video demo on how to print barcode labels in tissue culture laboratory."), br(),br(),
                          
                          tags$p(style="font-family: serif;","Download the tissue culture barcode printing templates from github",tags$a(href="https://github.com/mkaranja/BTracT-TC-Barcode-Printing-Templates", "BTracT-TC-Barcode-Printing-Templates")
                          )),
                 tabPanel("Crosses",
                          tags$p(style = "font-size: 18px; text-align: center;","This page is for downloading pre-formatted data for generating barcode labels in TC labs"),
                          fluidRow(
                             DT::dataTableOutput("crossesdt"),
                             verbatimTextOutput('t2'),
                             br())
                 ),
                 tabPanel("Embryo Germinating",
                          tags$p(style = "font-size: 18px; text-align: center;","This page is for downloading pre-formatted data for generating barcode labels in TC labs"),
                          DT::dataTableOutput("embryodt"),br()
                 ),
                 tabPanel("Subcultures",
                          tags$p(style = "font-size: 18px; text-align: center;","This page is for downloading pre-formatted data for generating barcode labels in TC labs"),
                          DT::dataTableOutput("subculresdt"),br()
                 )
                 # br(),
                 # tags$a(href="label_instructions.html", target="blank",tags$p(style="color:blue;font-size:18px;","Instructions"))
                 
    )
  )
  )
