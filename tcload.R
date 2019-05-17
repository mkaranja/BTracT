library(dplyr)
library(dataframes2xls)
library(stringr)


if(nrow(bananadata)>0){
banana_labels = bananadata %>%
 dplyr::filter(`Number of Embryo Rescued` > 0 & lubridate::ymd(`Embryo Rescue Date`)>=(Sys.Date()-10))

labels = data.frame(stringr::str_split_fixed(banana_labels$Crossnumber,"_",2))
colnames(labels) = c('Prefix','Suffix')
labels$Suffix = gsub("[)]","",(gsub("[(]","", labels$Suffix)))
banana_labels = cbind(banana_labels, labels)
banana_labels = banana_labels %>%
  dplyr::select(Location,Crossnumber,Prefix,Suffix,`Embryo Rescue Date`,`Number of Embryo Rescued`)
}

#**********************************************************************
if(nrow(plantlets)>0){
plantlet_labels = plantlets  %>%
  dplyr::select(`Location`,`PlantletID`,`Subculture Date`,Copies) %>%
  dplyr::filter(!is.na(`Subculture Date`) & lubridate::ymd(`Subculture Date`)>=(Sys.Date()-10))
labels = data.frame(stringr::str_split_fixed(plantlet_labels$PlantletID,"_",3))
colnames(labels) = c('Prefix','Suffix',"EmbryoNo")
labels$Suffix = gsub("[)]","",(gsub("[(]","", labels$Suffix)))
plantlet_labels = cbind(plantlet_labels,labels)
}

#----------------------------------------------------------------------------------------
if(nrow(plantlets)>0){
  if( 'Germination Date' %in% names(plantlets)){
    embryo_labels = plantlets %>%
      dplyr::select(Location, PlantletID, `Germination Date`) %>%
      dplyr::filter(lubridate::ymd(`Germination Date`)>=(Sys.Date()-10))
    labels = data.frame(stringr::str_split_fixed(embryo_labels$PlantletID,"_",3))
    colnames(labels) = c('Prefix','Suffix', "EmbryoNo")
    labels$Suffix = gsub("[)]","",(gsub("[(]","", labels$Suffix)))
    embryo_labels = cbind(embryo_labels,labels)
  }
}

#----------------------------------------------------------------------------------------
