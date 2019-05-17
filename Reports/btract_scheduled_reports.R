
setwd("/srv/shiny-server/btract/btract/data")

# Load data 

source("~/BANANA/BTRACT/btract_script_nelsonmandela_31.R") # remove rm(list=ls())
source("~/BANANA/BTRACT/btract_script_sendusu_31.R")

flowering = list.files(patt="AllFlowering.csv$", recursive = TRUE) %>%
  lapply(fread) %>%
  rbindlist(id=T, fill=T) %>%
  .[,-1]
flowering$Location = as.factor(flowering$Location)
flowering$Accession_Name = as.factor(flowering$Accession_Name)
flowering$Plant_Sex = as.factor(flowering$Plant_Sex)
flowering$Flowering_Date = anytime::anydate(as.character(flowering$Flowering_Date))
colnames(flowering) = gsub("_"," ", names(flowering))

status = list.files(patt="PlantStatus.csv$", recursive = TRUE) %>% # add location in script
  lapply(fread) %>%
  rbindlist(id=T, fill=T) %>% .[,-1] 

status$Status_Date = anytime::anydate(as.character(status$Status_Date))
colnames(status) = gsub("_"," ", names(status))
status$Image = paste0("<a href='",status$Image,"'>",'photo',"</a>")
status = dplyr::select(status, Location, StatusID, `Status Date`,Status, everything())

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
##******************************************************
banana = setDT(crosses)[crosses[, .I[which.min(Reduce(`+`, lapply(.SD, is.na)))], Crossnumber]$V1]

banana$First_Pollination_Date = anytime::anydate(as.character(banana$First_Pollination_Date))
banana$Bunch_Harvest_Date = anytime::anydate(as.character(banana$Bunch_Harvest_Date))
banana$Seed_Extraction_Date = anytime::anydate(as.character(banana$Seed_Extraction_Date))
banana$Embryo_Rescue_Date = anytime::anydate(as.character(banana$Embryo_Rescue_Date))
banana$Germination_Date = anytime::anydate(as.character(banana$Germination_Date))

banana$Days_to_Maturity = as.integer(banana$Bunch_Harvest_Date - banana$First_Pollination_Date)
banana$Days_in_ripening_shed = as.integer(banana$Seed_Extraction_Date - banana$Bunch_Harvest_Date)


# repeat pollination 
repeatpollination = list.files(patt="RepeatPollination.csv$", recursive = TRUE) %>%
  lapply(fread) %>%
  rbindlist(id=T, fill=T) %>%
  .[,-1]
repeatpollination = repeatpollination[Crossnumber!='']

repeatDT = as.data.frame(repeatpollination[,-1][,number := 1:.N, by = Crossnumber])
repeatDTwide = reshape(repeatDT,direction = "wide", idvar = "Crossnumber", timevar = "number")

repeatDTwide[,2:ncol(repeatDTwide)] = lapply(repeatDTwide[,2:ncol(repeatDTwide)],anytime::anydate)
repeats = setDT(repeatDTwide)[, Number_of_repeat_pollinations := rowSums(!is.na(repeatDTwide))-1][]
bananadt = dplyr::left_join(banana, repeats, by="Crossnumber")
bananadt$Number_of_repeat_pollinations = as.integer(bananadt$Number_of_repeat_pollinations)

# select ID reported with status
bananadt$StatusID = bananadt$Crossnumber
lostInfo = dplyr::inner_join(bananadt, status, by=c("Location","StatusID"))
lostInfo = janitor::remove_empty(lostInfo,"cols")
cols.num <- grep("_Date", names(lostInfo), value = T)
lostInfo[cols.num] <- sapply(lostInfo[cols.num],as.character)

lostInfo = lostInfo %>% tidyr::gather(Last_activity_recorded, Activity_Date, contains("_Date"), na.rm=T)

lostInfo = setDT(lostInfo)[,c("Location","Crossnumber","Last_activity_recorded","Activity_Date","Status","Notes","Image")][order(-Activity_Date)]
lostInfo = lostInfo %>%
  dplyr::filter(Last_activity_recorded !="Status_Date" & Activity_Date !="")

lostInfo$Last_activity_recorded = gsub("_Date","", lostInfo$Last_activity_recorded)
lostInfo$Last_activity_recorded = gsub("_"," ", lostInfo$Last_activity_recorded)

lostInfo$Last_activity_recorded = ifelse(grepl("^Repeat Pollination.", lostInfo$Last_activity_recorded, perl = T)==TRUE,"Repeat Pollination",lostInfo$Last_activity_recorded)
lostInfo = lostInfo[!duplicated(lostInfo$Crossnumber), ]
colnames(lostInfo) = gsub("_"," ", names(lostInfo))

status$Location = as.factor(status$Location)

# drop IDs reported with status
bananadata = dplyr::anti_join(bananadt, status, by="StatusID")
bananadata$StatusID = NULL

###################################
bananadata$Mother = stringr::str_trim(bananadata$Mother, side = "both")
bananadata$Father = stringr::str_trim(bananadata$Father, side = "both")
bananadata$Father = gsub(" - ","-",bananadata$Father)

bananadata$Mother = ifelse(bananadata$Mother %in% c("cv-Rose","ITC0712"),"ITC0712 Cv Rose", bananadata$Mother)
bananadata$Mother = ifelse(bananadata$Mother %in% c("ITC0609","Pahang","Pisang Pahang"),"ITC0609 Pahang", bananadata$Mother)
bananadata$Mother = ifelse(bananadata$Mother %in% c("Calcutta 4","ITC0249"),"ITC0249 Calcutta 4", bananadata$Mother)
bananadata$Mother = ifelse(bananadata$Mother %in% c("Borneo","ITC0253"),"ITC0253 Borneo", bananadata$Mother)
bananadata$Mother = ifelse(bananadata$Mother %in% c("ITC0766","Paliama"),"ITC0766 Paliama", bananadata$Mother)
bananadata$Mother = ifelse(bananadata$Mother %in% c("ITC1460","Ijihu nkundu"),"ITC1460-Ijihu nkundu", bananadata$Mother)
bananadata$Mother = ifelse(bananadata$Mother %in% c("ITC1468","Kahuti"),"ITC1468-Kahuti", bananadata$Mother)

bananadata$Father = ifelse(bananadata$Father %in% c("cv-Rose","ITC0712"),"ITC0712 Cv Rose", bananadata$Father)
bananadata$Father = ifelse(bananadata$Father %in% c("ITC0609","Pahang","Pisang Pahang"),"ITC0609 Pahang", bananadata$Father)
bananadata$Father = ifelse(bananadata$Father %in% c("Calcutta 4","ITC0249"),"ITC0249 Calcutta 4", bananadata$Father)
bananadata$Father = ifelse(bananadata$Father %in% c("Borneo","ITC0253"),"ITC0253 Borneo", bananadata$Father)
bananadata$Father = ifelse(bananadata$Father %in% c("ITC0766","Paliama"),"ITC0766 Paliama", bananadata$Father)
bananadata$Father = ifelse(bananadata$Father %in% c("ITC1460","Ijihu nkundu"),"ITC1460-Ijihu nkundu", bananadata$Father)
bananadata$Father = ifelse(bananadata$Father %in% c("ITC1468","Kahuti"),"ITC1468-Kahuti", bananadata$Father)

# library(brapi)
# mb = brapi::ba_db()$musabase
# accessions = brapi::ba_germplasm_search(mb, rclass="data.frame") %>%
#   dplyr::select(germplasmName,synonyms)
# 
# bananadata$Mother = ifelse(bananadata$Mother %in% accessions$synonyms, accessions$germplasmName, bananadata$Mother)
# bananadata$Father = ifelse(bananadata$Father %in% accessions$synonyms, accessions$germplasmName, bananadata$Father)

#######################################
bananadata = bananadata %>%
  dplyr::select(Location, Crossnumber, FemalePlotName, Mother, MalePlotName, Father, First_Pollination_Date, Number_of_repeat_pollinations,
                Days_to_Maturity,Bunch_Harvest_Date, Days_in_ripening_shed, everything(), -c(Cycle,starts_with("Repeat_Pollination_Date.")))

bananadata$Location = as.factor(bananadata$Location)
bananadata$Mother = as.factor(bananadata$Mother)
bananadata$Father = as.factor(bananadata$Father)

# if duplicate germplasm names, keep rows with minimum missing
bananadata = setDT(bananadata)[bananadata[, .I[which.min(Reduce(`+`, lapply(.SD, is.na)))], Crossnumber]$V1]
colnames(bananadata) = gsub("_"," ", names(bananadata))

# Plantlets
plantlets = list.files(patt="Plantlets.csv$", recursive = TRUE) %>%
  lapply(fread) %>%
  rbindlist(id=T, fill = T)
plantlets = plantlets[,-1]
plantlets_dates = grep("_Date$", names(plantlets), value = T)
plantlets[,plantlets_dates] <- plantlets[, lapply(.SD, as.Date), .SDcols = plantlets_dates]

colnames(plantlets) = gsub("_"," ", names(plantlets))
plants_Id = stringr::str_split_fixed(plantlets$PlantletID,"_",3)
plantlets$Crossnumber = paste0(plants_Id[,1],"_",plants_Id[,2])
plantlets = dplyr::left_join(plantlets, bananadata[,c("Crossnumber","Mother","Father")], by="Crossnumber")
plantlets = plantlets %>% dplyr::select(Location, PlantletID, everything())

##########################################################################################
## DATA EXPLORER
##########################################################################################


if (nrow(flowering)>0){
  flowered = flowering[,c("Location","Accession Name","Flowering Date")]
  flowered$Activity = "Flowering"
  colnames(flowered) <- c("Location","Accession","Date","Activity")
  flowered = flowered[complete.cases(flowered),]
} else {
  flowered = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),anytime::anydate(integer(0)))
}


# F.Polln
if(nrow(bananadata)>0){
  first_pollinationed = bananadata[,c("Location", "Crossnumber", "Mother","Father","First Pollination Date")]
  colnames(first_pollinationed) = c("Location","Accession","Mother","Father","Date")
  first_pollinationed$Activity = "First pollination"
  first_pollinationed = first_pollinationed[complete.cases(first_pollinationed),]
} else {
  first_pollinationed = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),anytime::anydate(integer(0)))
}   

# Repeat Polln
if(nrow(repeatpollination)>0){
  repeat_pollinationed = repeatpollination %>%
    dplyr::arrange(.,Crossnumber,desc(Repeat_Pollination_Date)) 
  repeat_pollinationed = repeat_pollinationed[!duplicated(repeat_pollinationed$Crossnumber),]
  repeat_pollinationed = dplyr::left_join(repeat_pollinationed, bananadata[,c("Crossnumber","Mother","Father")], by="Crossnumber")
  colnames(repeat_pollinationed) = c("Location","Accession","Date","Mother","Father")
  repeat_pollinationed$Date = anytime::anydate(repeat_pollinationed$Date)
  repeat_pollinationed $Activity = "Repeat pollination"
}

# Harvest
harvested = bananadata[,c("Location", "Crossnumber", "Mother","Father","Bunch Harvest Date")]
colnames(harvested) = c("Location","Accession","Mother","Father","Date")
if(nrow(harvested)>0){
  harvested$Activity = "Harvested bunches"
}
harvested = harvested[complete.cases(harvested),]

# Extracted
extracted = bananadata[, c("Location","Crossnumber","Mother","Father","Seed Extraction Date")]
colnames(extracted) <- c("Location","Accession","Mother","Father","Date")
if(nrow(extracted)>0){
  extracted$Activity = "Seed extraction"
}
extracted = extracted[complete.cases(extracted),]

# Rescue
rescued = bananadata[,c("Location","Crossnumber","Mother","Father","Embryo Rescue Date")]
colnames(rescued) <- c("Location","Accession","Mother","Father","Date")
if(nrow(rescued)>0){
  rescued$Activity = "Embryo Rescue"
}
#rescued$Date = lubridate::dmy(rescued$Date)
rescued = rescued[complete.cases(rescued),]

# Germination
germinated = bananadata[,c("Location","Crossnumber","Mother","Father","Germination Date")]
colnames(germinated) <- c("Location","Accession","Mother","Father","Date")
if(nrow(germinated)>0){
  germinated$Activity = "Germination"
}
germinated = germinated[complete.cases(germinated),]

# Subculture
if(nrow(plantlets)>0){
  subcul = plantlets[,c("Location","PlantletID","Mother","Father","Subculture Date")]
  colnames(subcul) <- c("Location","Accession","Mother","Father","Date")
  subcul$Date = anytime::anydate(as.character(subcul$Date))
  if(nrow(subcul)>0){
    subcul$Activity = "Subculturing"
  }
  subcul = subcul[complete.cases(subcul),]
} else{
  subcul = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),anytime::anydate(integer(0)))
}   
# # Rooted
if(nrow(plantlets)>0){   
  rooted = plantlets[,c("Location","PlantletID","Mother","Father","Date of Rooting")]
  colnames(rooted) <- c("Location","Accession","Mother","Father","Date")
  if(nrow(rooted)>0){
    rooted$Activity = "Rooting"
  }
  rooted = rooted[complete.cases(rooted),]
} else{
  rooted = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),anytime::anydate(integer(0)))
} 
if(nrow(plantlets)>0){   
  weaning1 = plantlets[,c("Location","PlantletID","Mother","Father","Sending Out Date")]
  colnames(weaning1) <- c("Location","Accession","Mother","Father","Date")
  if(nrow(weaning1)>0){
    weaning1$Activity = "Weaning 1/ Sent out"
  }
  weaning1 = weaning1[complete.cases(weaning1),]
} else{
  weaning1 = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),anytime::anydate(integer(0)))
} 
if(nrow(plantlets)>0){   
  weaning2 = plantlets[,c("Location","PlantletID","Mother","Father","Weaning 2 Date")]
  colnames(weaning2) <- c("Location","Accession","Mother","Father","Date")
  if(nrow(weaning2)>0){
    weaning2$Activity = "Weaning 2"
  }
  weaning2 = weaning2[complete.cases(weaning2),]
} else{
  weaning2 = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),anytime::anydate(integer(0)))
} 

# Screenhse
if(nrow(plantlets)>0){      
  screen_housed = plantlets[,c("Location","PlantletID","Mother","Father","Screenhouse Transfer Date")]
  colnames(screen_housed) <- c("Location","Accession","Mother","Father","Date")
  if(nrow(screen_housed)>0){
    screen_housed$Activity = "Screen house"
  }
  screen_housed = screen_housed[complete.cases(screen_housed),]
} else {
  screen_housed = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),anytime::anydate(integer(0)))
}    
# Hardened
if(nrow(plantlets)>0){    
  hardened = plantlets[,c("Location","PlantletID","Mother","Father","Hardening Date")]
  colnames(hardened) <- c("Location","Accession","Mother","Father","Date")
  if(nrow(hardened)>0){
    hardened$Activity = "Hardening"
  }
  hardened = hardened[complete.cases(hardened),]
} else {
  hardened = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),anytime::anydate(integer(0)))
}    
# Openfield
if(nrow(plantlets)>0){    
  open_field = plantlets[,c("Location","PlantletID","Mother","Father","Openfield Transfer Date")]
  colnames(open_field) <- c("Location","Accession","Mother","Father","Date")
  if(nrow(open_field)>0){
    open_field$Activity = "Open field"
  }
  open_field = open_field[complete.cases(open_field),]
} else {
  open_field = data.frame(Location = character(), Accession = character(),Mother = character(),Father = character(),anytime::anydate(integer(0)))
} 

cleantable = plyr::rbind.fill(flowered,first_pollinationed,repeat_pollinationed,harvested,extracted,rescued,germinated,
                              subcul, rooted,weaning1, weaning2,screen_housed,hardened, open_field)
