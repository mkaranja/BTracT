
source("loaddata.R")

datevalue = c(min(anytime::anydate(cleantable$Date)),max(anytime::anydate(cleantable$Date)))
datemin = min(lubridate::ymd(cleantable$Date))
datemax = max(lubridate::ymd(cleantable$Date))


# searchbar
aa = setorder(cleantable,Accession, -Date)
bb = aa[!duplicated(aa$Accession),]

Allset = setDT(bananadata)[,c("Crossnumber","Total Seeds","Good Seeds","Number of Embryo Rescued","Active after 8 Weeks")]
colnames(Allset)[1] = "Accession"
merge_bb = Reduce(function(x,y) merge(x,y, all=T, by = "Accession"), list(bb, Allset))
merge_bb = merge_bb[!duplicated(merge_bb$Accession),]
bb_flower = setDT(merge_bb)[Activity=='Flowering' & Date >= Sys.Date()-7]


bb_other = merge_bb["Activity" !='Flowering'] #, Activity != 'Seed extraction' & Activity != 'Embryo rescue' & Activity != 'Germination after 8 weeks')
bbDF <- rbind(bb_other, bb_flower) #, bb_seedExtr, bb_rescue, bb_8weeks)

bbDF_order = "Location"
bbDF = setcolorder(bbDF, c(bbDF_order, setdiff(names(bbDF),bbDF_order)))
f_banana = bananadata %>%
  dplyr::select(-c(Location, Mother,Father,FemalePlotName, MalePlotName)) %>%
  dplyr::rename(Accession = Crossnumber)

full_cleantable = dplyr::left_join(cleantable, f_banana, by="Accession") %>%
  dplyr::select(-c(Mother,Father,ends_with("Date")),Date)
  
# full_cleantable
#   filter(merge_bb, Activity=='Seed extraction')# & as.integer(`Total Seeds`) > 0
# bb_rescue = filter(merge_bb, Activity == 'Embryo rescue') # &  as.integer(`Number of Embryo Rescued`) > 0
# bb_8weeks = filter(merge_bb, Activity == 'Germination after 8 weeks')# & as.integer('Actively germinating after 8 weeks') > 0


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
