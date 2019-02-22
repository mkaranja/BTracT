library(shiny)
library(shinyWidgets)

tc_page = function() tagList(
    fluidRow(
      tags$h1(style = 'text-align: center',"BTracT"),
      tags$p(style = "color: orange; font-size: 30px; text-align: center;","Download data for barcode labels generation"),
      tags$p(style = "color: black; font-size: 18px; text-align: center;","This site provides data for barcode labels generation in tissue culture laboratories and other complex BTracT activities"),
     
      column(4, offset = 1,
             plotOutput("barcodeImage")), br(), br(), br(), 
      column(4, offset = 1,
      actionBttn("crossIDLabels","Crosses",style = "fill", color = "success"),br(),br(),br(),
      bsModal("modalCrossIDLabel", tags$span(h3(style = 'color:orange',"Crossnumber")), "crossIDLabels", size = "large",
              br(),
              tags$span(h4(style = 'color:blue',"This table populates immediately after Embryo rescue has been reported")),
              
                selectInput(inputId = 'crosses_site',label = 'Select site',choices = c("Arusha","Sendusu")),
              div(style = 'overflow-x: scroll',
                  DT::dataTableOutput("crosses")),br(),
               p("To download specific barcodes, select their respective rows in the given table.",br(),
                "By not selecting any row in the data table, all the crossIDs barcodes will be available for download"), br(),
              downloadBttn("downloadcrossID","Download", style = "unite", size = "sm")
              
              
      )),br(),br(),br(),

      column(4, offset = 1,
      actionBttn("seedsIDLabels","Seeds germinating after 8 weeks", color = "success", style = "fill"),br(),br(),br(),
      bsModal("modalSeedIDLabels", tags$span(h3(style = 'color:orange',"Seeds germinating after 8 weeks")), "seedsIDLabels", size = "large",
              br(),
              selectInput('seeds_site','Select site',c("Arusha","Sendusu")),
              DT::dataTableOutput("seedsIDTable"),br(),
              p("To download specific barcodes, select their respective rows in the given table.",br(),
                "By not selecting any row in the data table, all the crossIDs barcodes will be available for download"), br(),
              
              downloadBttn("downloadseedsID","Download", style = "unite", size = "sm"),br()
              )),br(),br(),br(),

      column(4, offset = 1,
      actionBttn("subculresIDLabels","PLantlets/ Subcultures",style = "fill", color="success"),br(),br(),br(),
      bsModal("modalsubculresIDLabels", tags$span(h3(style = 'color:orange',"Subcultures")), "subculresIDLabels", size = "large",
              br(),
              selectInput('subs_site','Select site',c("Arusha","Sendusu")),
              DT::dataTableOutput("subculresIDTable"),br(),
              p("To download specific barcodes, select their respective rows in the given table.",br(),
                "By not selecting any row in the data table, all the crossIDs barcodes will be available for download"), br(),
              
              downloadBttn("downloadsubculresID","Download", style = "unite", size = "sm"),br()
              )
      ), br(),br(),br()
     ), br(), hr(),
     br(),
    
   fluidRow(
   column(4, offset=4,
	tags$p(style="color: green; font-size:30px; text-align:center;","Instructions on barcode printing"),
	tags$a(href="documentation/tc_label_instructions.html", target="blank",tags$p(style="color:blue;font-size:25px; text-align:center;","go to.."))
	)
   ),br(),hr(),br(),
    fluidRow(
      HTML('<footer align="center">
           <div class="row">
           <div class="col-sm-2 col-sm-offset-3">
           <a href = "http://breedingbetterbananas.org/">
           <img src = "betterbananaLogo.png", height="65", width="120"> 
           </a>
           </div>
           <div class="col-sm-2">
           <a href = "http://www.iita.org/">
           <img src = "iita.png", height="65", width="120"> 
           </a>
           </div>
           <div class="col-sm-2">
           <a href = "https://btiscience.org/">
           <img src="bti.png", height="70", width="120"> 
           </a>
           </div>
           </div>
           </footer>')
      ),
    windowTitle = "BTracT labels"
  )

