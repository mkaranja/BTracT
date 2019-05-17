
# Check if multiple R packages are installed. Install them if they are not,then load them into the R session.

if(!require(pacman))install.packages("pacman")
pacman::p_load("shiny", "shinydashboard", "shinydashboardPlus",
               "shinysky", "shinyWidgets", "shinycssloaders", "shinyalert", "shinyFeedback", "shinyFeedback", "bsplus", "shinyBS",
               "jsonlite","shinyjs",
               "DT","summarytools","rpivotTable","qrencoder",
               "plotly","highcharter", "collapsibleTree", "ECharts2Shiny", "r2d3", "gridBase", "RColorBrewer",
               "rmarkdown",
               "magrittr", "dplyr", "brapi", "WriteXLS")

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
