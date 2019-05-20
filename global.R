
# Check if multiple R packages are installed. Install them if they are not,then load them into the R session.

# if(!require(pacman))install.packages("pacman")
# pacman::p_load("shiny", "shinydashboard", "shinydashboardPlus",
#                "shinysky", "shinyWidgets", "shinycssloaders", "shinyFeedback", "shinyBS",
#                "jsonlite","shinyjs","shinyURL",
#                "DT","summarytools","qrencoder","data.table",
#                "highcharter", "collapsibleTree", "rmarkdown",
#                "magrittr","plyr", "dplyr", "brapi", "WriteXLS","dataframes2xls","stringr")

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))

suppressPackageStartupMessages(library(shinysky))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(shinyFeedback))
suppressPackageStartupMessages(library(shinyURL))
suppressPackageStartupMessages(library(shinyBS))

suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(shinyjs))

suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(summarytools))
suppressPackageStartupMessages(library(qrencoder))

suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(highcharter))
suppressPackageStartupMessages(library(collapsibleTree))

suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(brapi))
suppressPackageStartupMessages(library(WriteXLS))
suppressPackageStartupMessages(library(dataframes2xls))
suppressPackageStartupMessages(library(stringr))

source("loaddata.R")
source("tcload.R") # tissue culture data

enableBookmarking(store = "url")

datevalue = c(min(anytime::anydate(cleantable$Date)),max(anytime::anydate(cleantable$Date)))
datemin = min(lubridate::ymd(na.omit(cleantable$Date)))
datemax = max(lubridate::ymd(na.omit(cleantable$Date)))


# searchbar
aa = setorder(cleantable,Accession, -Date)
bb = aa[!duplicated(aa$Accession),]

bb_flower = setDT(bb)[Activity=='Flowering' & Date >= Sys.Date()-7]
bb_other = bb["Activity" !='Flowering'] 
bbDF <- rbind(bb_other, bb_flower)

bbDF_order = "Location"


appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"

searchlist = c(as.character(unique(na.omit(bb$Location))), 
               as.character(unique(na.omit(bb$Activity))), 
               as.character(unique(na.omit(bb$Accession))),
               as.character(unique(na.omit(bb$Mother))), 
               as.character(unique(na.omit(bb$Father))), 
               as.character(unique(na.omit(lubridate::year(bb$Date)))))


BTracTFooter <- function() tagList(
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
    )
    )
