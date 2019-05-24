

labels = function() tagList(
  div(
      tags$p(style = "color: black; font-size: 28px; text-align: center;","Real-time barcode generator"),
      tags$p(style = "color: blue; font-size: 18px; text-align: center;", Sys.Date()),
      
      column(2, 
             wellPanel(
                selectInput('barcode_value','Barcode value:', c("Crossnumber","FemalePlotName","MalePlotName","Mother","Father")),
                
                selectInput('barcode_label', 'Barcode label:', c("Crossnumber","FemalePlotName","MalePlotName","Mother","Father")),
               shinyWidgets::prettyRadioButtons(inputId = "format",label = "Label format:", choices = c("Pre-defined", "Custom"),
                                                icon = icon("check"), bigger = F,status = "info", inline = T), br(),
              uiOutput('labelformat'),
              downloadBttn("downloadlabels",style = "bordered", size="sm",color = "primary")
             )),
      # tags$p(style = "font-family: 20px arial;","Use this tool to generate and download dynamic barcode labels for the most recent crosses for immediate use.", br(),br(), 
      #          "QRCODE contains the ",tags$b("Crossnumber"),". Label information include the", tags$b("Crossnumber")," and the ", tags$b("FemalePlotName"), br(),br(),
      #          "Once downloaded, use the office printer to print out labels"), br(), br(),
      # p("To download specific Crosses, select the respective rows in the table and click download."), br(),
      
      column(9,
             wellPanel(
               div(style = 'overflow-x: scroll',
                   DT::dataTableOutput("labelsDT")
               )
             )
      )
  )
)