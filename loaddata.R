
accessions_links_in_musabase = fread('data/accessions_links_in_musabase.csv')
germplasm_info_arusha = fread("data/germplasm_info_Arusha.csv")[,2:3] 

#setwd("./data")
# Load data 
flowering = list.files(patt="AllFlowering.csv$", recursive = TRUE) %>%
  lapply(fread) %>%
  rbindlist(id=T, fill=T) %>%
  .[,-1] %>%
  .[FlowerID!="" | !is.na(FlowerID)]
flowering %<>% mutate_all(as.character)
flowering[,c("Location","Genotype","Plant_Sex")] %<>% mutate_all(as.factor)
flowering$Flowering_Date = anytime::anydate(as.character(flowering$Flowering_Date))
colnames(flowering) = gsub("_"," ", names(flowering))

status = list.files(patt="PlantStatus.csv$", recursive = TRUE) %>% # add location in script
  lapply(fread) %>%
  rbindlist(id=T, fill=T) %>% .[,-1] 

status %<>% mutate_all(as.character)

status$Status_Date = anytime::anydate(as.character(status$Status_Date))
colnames(status) = gsub("_"," ", names(status))
if(nrow(status)>0){
status$Image = paste0("<a href='",status$Image,"'>",'photo',"</a>")
}
status = dplyr::select(status, Location, StatusID, `Status Date`,Status, everything()) %>%
  .[complete.cases(.),]


contamination = list.files(patt="Contamination.csv$", recursive = TRUE) %>%
  lapply(fread) %>%
  rbindlist(id=T, fill=T) %>% .[,-1]

contamination$Location = as.factor(contamination$Location)
contamination$Contamination_Date = anytime::anydate(as.character(contamination$Contamination_Date))
colnames(contamination) =gsub("_"," ", names(contamination))

# crosses
crosses = list.files(patt="BananaData.csv$", recursive = TRUE) %>%
  lapply(fread) %>%
  rbindlist(id=T, fill = T) %>% .[,-1]
crosses %<>% mutate_all(as.character)
crosses = dplyr::select(crosses, -c("Number_Sent_for_Embryo_Rescue","Early_Germination_Seeds"))
#******************************************************
crosses = setDT(crosses)[crosses[, .I[which.min(Reduce(`+`, lapply(.SD, is.na)))], Crossnumber]$V1]
# banana = data.frame(banana)
# # previous records
 arusha_legacy_crosses = fread("data/Last_6_months_tisssue_culture_data.csv")
 rmcol = c("contamination","badseeds",grep("^days", names(arusha_legacy_crosses), value = T))
 arusha_legacy_crosses = arusha_legacy_crosses[, (rmcol) := NULL][] %>% # drop unwanted columns
   setnames(colnames(.), c("Location","Crossnumber","Mother","Father","First_Pollination_Date","Bunch_Harvest_Date", "Seed_Extraction_Date",
                           "Total_Seeds","Good_Seeds","Number_of_Embryo_Rescued", "Embryo_Rescue_Date", "Germination_Date",                  
                           "Number_of_Embryo_Germinating"))
# merge crosses and previous datasets
banana =rbind.fill(crosses, arusha_legacy_crosses)
banana_dates = grep("_Date", names(banana), value = T)
banana[,banana_dates] %<>% mutate_all(anytime::anydate)

banana$Days_to_Maturity = as.integer(banana$Bunch_Harvest_Date - banana$First_Pollination_Date)
banana$Days_in_ripening_shed = as.integer(banana$Seed_Extraction_Date - banana$Bunch_Harvest_Date)


# repeat pollination 
repeatpollination = list.files(patt="RepeatPollination.csv", recursive = TRUE) %>%
  lapply(fread) %>%
  rbindlist(id=T, fill=T) %>% .[,-1]
repeatpollination %<>% mutate_all(as.character)

bananadata = banana
###################################
bananadata$Mother = stringr::str_trim(bananadata$Mother, side = "both")
bananadata$Father = stringr::str_trim(bananadata$Father, side = "both")
bananadata$Father = gsub(" - ","-",bananadata$Father)

# format to Musabase format/ structure
source("helpers/convert_names_to_musabase_format.R")

#######################################
bananadata = bananadata %>%
  dplyr::select(Location, Crossnumber, FemalePlotName, Mother, MalePlotName, Father, First_Pollination_Date, # Number_of_Repeat_Pollinations,
                Days_to_Maturity,Bunch_Harvest_Date, Days_in_ripening_shed, everything(), -c(Cycle,starts_with("Repeat_Pollination_Date.")))

bananadata[,c("Location","Mother","Father")] %<>% mutate_all(as.factor)

# if duplicate germplasm names, keep rows with minimum missing
bananadata = setDT(bananadata)[bananadata[, .I[which.min(Reduce(`+`, lapply(.SD, is.na)))], Crossnumber]$V1]
colnames(bananadata) = gsub("_"," ", names(bananadata))

# include ploidy information
germplasmploidy = fread("data/germplasmploidy.csv")
germplasmploidy$ploidylevel = toupper(germplasmploidy$ploidylevel)
bananadata$germplasmName = as.character(bananadata$Mother)
bananaploidy = dplyr::left_join(bananadata,germplasmploidy, by='germplasmName')
bananaploidy = bananaploidy %>% dplyr::rename(FemalePloidyLevel = ploidylevel)

bananaploidy$germplasmName = NULL
bananaploidy$germplasmName = as.character(bananaploidy$Father)
bananaploidy = dplyr::left_join(bananaploidy, germplasmploidy, by='germplasmName')
bananadata = bananaploidy %>% dplyr::rename(MalePloidyLevel = ploidylevel)
bananadata$germplasmName = NULL

# Musabase Links of genotypes
colnames(accessions_links_in_musabase) = c("Mother",'Female Genotype')
bananadata = dplyr::left_join(bananadata, accessions_links_in_musabase, by="Mother")
colnames(accessions_links_in_musabase) = c("Father",'Male Genotype')
bananadata = dplyr::left_join(bananadata, accessions_links_in_musabase, by="Father")


bananadata = bananadata %>% dplyr::select("Location","Crossnumber","FemalePlotName","Mother","Female Genotype","FemalePloidyLevel","MalePlotName","Father","Male Genotype","MalePloidyLevel", everything())
bananadata = bananadata[!duplicated(bananadata$Crossnumber),]

# update plotnames
# FemalePlotNames
germplasm_info_arusha %<>% mutate_all(as.character)

colnames(germplasm_info_arusha)[2] = "FemalePlotName"
plots = dplyr::left_join(bananadata[,2:3], germplasm_info_arusha, by='FemalePlotName') %>%
  .[,-2] %>%
  .[complete.cases(.),]

bananadata = dplyr::left_join(bananadata, plots, by="Crossnumber")
bananadata$FemalePlotName = ifelse(!is.na(bananadata$ObservationUnitName), bananadata$ObservationUnitName, bananadata$FemalePlotName)
bananadata$ObservationUnitName = NULL

# MalePlotNames
colnames(germplasm_info_arusha)[2] = "MalePlotName"
plots = dplyr::left_join(bananadata[,c(2,7)], germplasm_info_arusha, by='MalePlotName') %>%
  .[,-2] %>%
  .[complete.cases(.),]

bananadata = dplyr::left_join(bananadata, plots, by="Crossnumber")
bananadata$MalePlotName = ifelse(!is.na(bananadata$ObservationUnitName), bananadata$ObservationUnitName, bananadata$MalePlotName)
bananadata$ObservationUnitName = NULL

# Drop ID with wrong formats
droprows = bananadata %>% dplyr::filter(substr(bananadata$Crossnumber,10,11)=="(C" | grepl("C/",bananadata$Crossnumber)==TRUE | grepl("/)",bananadata$Crossnumber[1]))
bananadata = dplyr::anti_join(bananadata, droprows, by="Crossnumber")

# Set Formats
banana_factors = c("Location", "Mother","Father", "Female Genotype", "FemalePloidyLevel", "Male Genotype","MalePloidyLevel" )
banana_chrs = c("Crossnumber","FemalePlotName", "MalePlotName")
banana_int = c("Days to Maturity", "Days in ripening shed", "Seed Extraction Date","Total Seeds","Good Seeds","Number of Embryo Rescued","Number of Embryo Germinating")
banana_dates = grep("Date", names(bananadata), value = T)

# to factors
bananadata[,banana_factors] %<>% mutate_all(as.factor)
# to characters
bananadata[, banana_chrs] %<>% mutate_all(as.character)
# to integer
bananadata[, banana_int] %<>% mutate_all(as.integer)
# dates
bananadata[,banana_dates] %<>% mutate_all(anytime::anydate)

# Plantlets
plantlets = list.files(patt="*Plantlets*", recursive = TRUE) %>%
  lapply(fread) %>%
  rbindlist(id=T, fill = T)

plantlets %<>% mutate_if(is.logical, as.character)
plantlets_dates = grep("Date$", names(plantlets), value = T)
plantlets[,plantlets_dates] %<>% mutate_all(anytime::anydate)
colnames(plantlets) = gsub("_"," ", names(plantlets))

if(nrow(plantlets)>0){
plants_Id = stringr::str_split_fixed(plantlets$PlantletID,"_",3)
plantlets$Crossnumber = paste0(plants_Id[,1],"_",plants_Id[,2])
plantlets = dplyr::left_join(plantlets, bananadata[,c("Crossnumber","Mother","Father")], by="Crossnumber")
}

plantlets = dplyr::select(plantlets, -c(".id", contains("Status")))

plantlets = plantlets %>% dplyr::select(Location, PlantletID, everything()) %>% setDT()

##########################################################################################
## DATA EXPLORER
##########################################################################################

if (nrow(flowering)>0){
  flowered = flowering[,c("Location","Genotype","Flowering Date")]
  flowered$Activity = "Flowering"
  colnames(flowered) <- c("Location","Accession","Date","Activity")
  flowered = flowered[complete.cases(flowered),]
} else {
  flowered = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),Date = anytime::anydate(integer(0)))
}


# F.Polln
if(nrow(bananadata)>0){
    first_pollinationed = bananadata[,c("Location", "Crossnumber", "Mother","Father","First Pollination Date")]
    colnames(first_pollinationed) = c("Location","Accession","Mother","Father","Date")
    first_pollinationed$Activity = "First pollination"
    first_pollinationed = first_pollinationed[complete.cases(first_pollinationed),]
 } else {
   first_pollinationed = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),Date = anytime::anydate(integer(0)))
 }   
    
 # Repeat Polln

repeat_pollinationed = repeatpollination %>%
 dplyr::arrange(.,Crossnumber,desc(Repeat_Pollination_Date)) 
repeat_pollinationed = repeat_pollinationed[!duplicated(repeat_pollinationed$Crossnumber),]
repeat_pollinationed = dplyr::left_join(repeat_pollinationed, bananadata[,c("Crossnumber","Mother","Father")], by="Crossnumber")
colnames(repeat_pollinationed) = c("Location","Accession","Date","Mother","Father")
repeat_pollinationed$Date = anytime::anydate(repeat_pollinationed$Date)
if(nrow(repeat_pollinationed)>0){
 repeat_pollinationed$Activity = "Repeat pollination"
} else {
  repeat_pollinationed$Activity = character()
}
    
# Harvest
harvested = bananadata[,c("Location", "Crossnumber", "Mother","Father","Bunch Harvest Date")]
colnames(harvested) = c("Location","Accession","Mother","Father","Date")
if(nrow(harvested)>0){
  harvested$Activity = "Harvested bunches"
}
harvested = harvested[complete.cases(harvested),]

# Extracted
extracted = bananadata[, c("Location","Crossnumber","Mother","Father","Seed Extraction Date","Total Seeds")]
colnames(extracted)[c(1:6)] <- c("Location","Accession","Mother","Father","Date","Total Seeds")
if(nrow(extracted)>0){
  extracted$Activity = "Seed extraction"
}
extracted = extracted[complete.cases(extracted),]

# Rescue
rescued = bananadata[,c("Location","Crossnumber","Mother","Father","Embryo Rescue Date","Total Seeds","Good Seeds","Number of Embryo Rescued")]
colnames(rescued) <- c("Location","Accession","Mother","Father","Date","Total Seeds","Good Seeds","Number of Embryo Rescued")
if(nrow(rescued)>0){
  rescued$Activity = "Embryo Rescue"
}
#rescued$Date = lubridate::dmy(rescued$Date)
rescued = rescued[complete.cases(rescued),]

# Germination
germinated = bananadata[,c("Location","Crossnumber","Mother","Father","Germination Date","Total Seeds","Good Seeds","Number of Embryo Rescued","Number of Embryo Germinating")]
colnames(germinated) <- c("Location","Accession","Mother","Father","Date","Total Seeds","Good Seeds","Number of Embryo Rescued","Number of Embryo Germinating")
if(nrow(germinated)>0){
  germinated$Activity = "Germination"
}
germinated = germinated[complete.cases(germinated),]



# Subculture
if(nrow(plantlets)>0){
   subcul = plantlets[,c("Location","PlantletID","Mother","Father","Subculture Date","Copies")]
   colnames(subcul) <- c("Location","Accession","Mother","Father","Date", "Copies")
   subcul$Date = anytime::anydate(as.character(subcul$Date))
   if(nrow(subcul)>0){
     subcul$Activity = "Subculturing"
   }
   subcul = subcul[complete.cases(subcul),]
} else{
  subcul = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),Date= anytime::anydate(integer(0)), Copies=character())
}   
  # # Rooted
if(nrow(plantlets)>0){   
   rooted = plantlets[,c("Location","PlantletID","Mother","Father","Date of Rooting","Copies","Number Rooting")]
   colnames(rooted) <- c("Location","Accession","Mother","Father","Date","Copies","Number Rooting")
   if(nrow(rooted)>0){
     rooted$Activity = "Rooting"
   }
   rooted = rooted[complete.cases(rooted),]
} else{
  rooted = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),Date = anytime::anydate(integer(0)))
} 
if(nrow(plantlets)>0){   
  weaning1 = plantlets[,c("Location","PlantletID","Mother","Father","Sending Out Date","Copies","Number Rooting", "Number Sent Out")]
  colnames(weaning1) <- c("Location","Accession","Mother","Father","Date","Copies","Number Rooting", "Number Sent Out")
  if(nrow(weaning1)>0){
    weaning1$Activity = "Weaning 1/ Sent out"
  }
  weaning1 = weaning1[complete.cases(weaning1),]
} else{
  weaning1 = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),Date = anytime::anydate(integer(0)))
} 
if(nrow(plantlets)>0){   
  weaning2 = plantlets[,c("Location","PlantletID","Mother","Father","Weaning 2 Date","Copies","Number Rooting", "Number Sent Out", "Weaning 2 Plantlets")]
  colnames(weaning2) <- c("Location","Accession","Mother","Father","Date","Copies","Number Rooting", "Number Sent Out", "Number in Weaning2")
  if(nrow(weaning2)>0){
    weaning2$Activity = "Weaning 2"
  }
  weaning2 = weaning2[complete.cases(weaning2),]
} else{
  weaning2 = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),Date = anytime::anydate(integer(0)))
} 

# Screenhse
if(nrow(plantlets)>0){      
   screen_housed = plantlets[,c("Location","PlantletID","Mother","Father","Screenhouse Transfer Date","Copies","Number Rooting", "Number Sent Out", "Weaning 2 Plantlets", "Number in Screenhouse" )]
   colnames(screen_housed) <- c("Location","Accession","Mother","Father","Date","Copies","Number Rooting", "Number Sent Out", "Number in Weaning2", "Number in Screenhouse")
   if(nrow(screen_housed)>0){
     screen_housed$Activity = "Screen house"
   }
   screen_housed = screen_housed[complete.cases(screen_housed),]
} else {
  screen_housed = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),Date = anytime::anydate(integer(0)))
}    
   # Hardened
if(nrow(plantlets)>0){    
   hardened = plantlets[,c("Location","PlantletID","Mother","Father","Hardening Date","Copies","Number Rooting", "Number Sent Out", "Weaning 2 Plantlets", "Number in Screenhouse" ,"Number in Hardening")]
   colnames(hardened) <- c("Location","Accession","Mother","Father","Date","Copies","Number Rooting", "Number Sent Out", "Number in Weaning2", "Number in Screenhouse" ,"Number Hardening")
   if(nrow(hardened)>0){
     hardened$Activity = "Hardening"
   }
   hardened = hardened[complete.cases(hardened),]
 } else {
   hardened = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),Date = anytime::anydate(integer(0)))
 }    
   # Openfield
if(nrow(plantlets)>0){    
   open_field = plantlets[,c("Location","PlantletID","Mother","Father","Openfield Transfer Date","Copies","Number Rooting", "Number Sent Out", "Weaning 2 Plantlets", "Number in Screenhouse" ,"Number in Hardening", "Number in Openfield")]
   colnames(open_field) <- c("Location","Accession","Mother","Father","Date","Copies","Number Rooting", "Number Sent Out", "Number in Weaning2", "Number in Screenhouse" ,"Number Hardening", "Number in Openfield")
   if(nrow(open_field)>0){
     open_field$Activity = "Open field"
   }
   open_field = open_field[complete.cases(open_field),]
 } else {
   open_field = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),Date = anytime::anydate(integer(0)))
 } 

cleantable = plyr::rbind.fill(flowered,first_pollinationed,repeat_pollinationed,harvested,extracted,rescued,germinated,
                              subcul, rooted,weaning1, weaning2,screen_housed,hardened, open_field)
cleantable$Date = anytime::anydate(as.character(cleantable$Date))

