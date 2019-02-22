library(dplyr)
library(dataframes2xls)
library(stringr)


if(nrow(bananadata)>0){
banana_labels = bananadata %>%
 dplyr::filter(lubridate::year(`Embryo Rescue Date`)>=2018 & `Number of Embryo Rescued` > 0) 



banana_labels$Prefix = substr(banana_labels$Crossnumber,1,8)

banana_labels$Suffix = ifelse(nchar(banana_labels$Crossnumber)>15, str_split(banana_labels$Crossnumber,'[()]')[[1]][[2]], substr(banana_labels$Crossnumber,10,15))

banana_labels = banana_labels %>%
  dplyr::select(Location,Crossnumber,Prefix,Suffix,`Number of Embryo Rescued`,`Embryo Rescue Date`,
                `Germination after 2 Weeks Date`,`Active after 2 Weeks`, `Germination after 8 Weeks Date`,`Active after 8 Weeks`)
}

#**********************************************************************
if(nrow(plantlets)>0){
plantlet_labels = plantlets  %>%
  dplyr::select(`Location`,`PlantletID`,`Subculture Date`,
                `Date of Rooting`,`Screenhouse Transfer Date`,`Hardening Date`,`Openfield Transfer Date`)
plantlet_labels$Prefix = substr(plantlet_labels$PlantletID,1,8)
plantlet_labels$Suffix = str_split(plantlet_labels$PlantletID, '[()]')[[1]][[2]]
no = 13 + str_length(plantlet_labels$Suffix)
plantlet_labels$SubNo = substr(plantlet_labels$PlantletID, no, str_length(plantlet_labels$PlantletID))
}

#----------------------------------------------------------------------------------------
if(nrow(seeds_data)>0){
  seeds_labels = seeds %>%
    dplyr::select(Location, SeedID, `Germination after 8 Weeks Date`)
  
  seedsDT$Prefix = substr(seedsDT$SeedID,1,8)
  seedsDT$Suffix = str_split(seedsDT$SeedID, '[()]')[[1]][[2]]
  n1 = 13 + str_length(seedsDT$Suffix)
  seedsDT$SeedNo = substr(seedsDT$SeedID, n1, str_length(seedsDT$SeedID))
   seedsDT =  seedsDT %>%
     dplyr::filter(`Germination after 8 Weeks Date` >= Sys.Date()-1)
}

#----------------------------------------------------------------------------------------
