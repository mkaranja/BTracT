
dt = cleantable[lubridate::year(cleantable$Date)>=2018,]
dt = dt[rev(order(as.Date(dt$Date))),]
df = dt[!duplicated(dt[,1:4]),]
df = df[!duplicated(df$Accession),] %>%
  setDT()
df = df[Activity != "Flowering"][,-c(5:6)]

# days elapsed
df$`Days Elapsed` = Sys.Date()-df$Date

# mean number of days between activities
dff = dplyr::left_join(bananadata, repeatpollination[,-1], by="Crossnumber")
dff$`Days to Germination` = lubridate::date(dff$`Germination Date`) - lubridate::date(dff$`Embryo Rescue Date`)
dff$`Days to Embryo Rescue` = lubridate::date(dff$`Embryo Rescue Date`) - lubridate::date(dff$`Seed Extraction Date`)
dff$`Days to Repeat Pollination` = lubridate::date(dff$Repeat_Pollination_Date) - lubridate::date(dff$`First Pollination Date`)

mean_days_to_repeatpollination = mean(as.integer(na.omit(dff$`Days to Repeat Pollination`))) %>% floor() # to repeat
mean_days_to_maturity = mean(as.integer(na.omit(dff$`Days to Maturity`))) %>% floor() # to harvest
mean_days_in_ripening = mean(as.integer(na.omit(dff$`Days in ripening shed`))) %>% floor() # to extraction
mean_days_to_embryo_rescue = mean(as.integer(na.omit(dff$`Days to Embryo Rescue`))) %>% floor() # to rescue
mean_days_to_germination = mean(as.integer(na.omit(dff$`Days to Germination`))) %>% floor() # to germination





df$status = ifelse(df$Activity=='First pollination' & df$`Days Elapsed` > (mean_days_to_repeatpollination+5), "Overdue",
                ifelse(df$Activity=='First pollination' & df$`Days Elapsed` %between% c(mean_days_to_repeatpollination-5, mean_days_to_repeatpollination+5), "Ready",
                    ifelse(df$Activity=='First pollination' & df$`Days Elapsed` %between% c(mean_days_to_repeatpollination-10, mean_days_to_repeatpollination-5), "Approaching",
                           ifelse(df$Activity=='First pollination' & df$`Days Elapsed` < mean_days_to_repeatpollination-10, "Wait",
                
              ifelse(df$Activity=='Repeat pollination' & df$`Days Elapsed` > (mean_days_to_maturity+10),"Overdue",
                     ifelse(df$Activity=='Repeat pollination' & df$`Days Elapsed` %between% c(mean_days_to_maturity-10, mean_days_to_maturity+10), "Ready",
                            ifelse(df$Activity=='Repeat pollination' & df$`Days Elapsed` %between% c(mean_days_to_maturity-30, mean_days_to_maturity-10), "Approaching",
                                   ifelse(df$Activity=='Repeat pollination' & df$`Days Elapsed` < mean_days_to_maturity-30, "Wait",
                                                              
                           # harvested
                  ifelse(df$Activity=='Harvested bunches' & df$`Days Elapsed` > mean_days_in_ripening+3,"Overdue",
                      ifelse(df$Activity=='Harvested bunches' & df$`Days Elapsed` %between% c(mean_days_in_ripening-3, mean_days_in_ripening+3), "Ready",
                        ifelse(df$Activity=='Harvested bunches' & df$`Days Elapsed` %between% c(mean_days_in_ripening-5, mean_days_in_ripening-3), "Approaching",
                               ifelse(df$Activity=='Harvested bunches' & df$`Days Elapsed` < mean_days_in_ripening-5, "Wait",
                        
                               # Seed extraction 
                           ifelse(df$Activity=='Seed extraction'  & df$`Total Seeds` > 0 & df$`Days Elapsed` > (mean_days_to_embryo_rescue+3), "Overdue",
                                ifelse(df$Activity=='Seed extraction'  & df$`Total Seeds` > 0 & df$`Days Elapsed` %between% c(mean_days_to_embryo_rescue-2, mean_days_to_embryo_rescue+3), "Ready",
                                      ifelse(df$Activity=='Seed extraction'  & df$`Total Seeds` > 0 & df$`Days Elapsed` %between% c(3,mean_days_to_embryo_rescue-3), "Approaching",
                                             ifelse(df$Activity=='Seed extraction'  & df$`Total Seeds` > 0 & df$`Days Elapsed` < 3, "Wait",
                                         
                                              # EMbryo rescue
                                            ifelse(df$Activity=='Embryo Rescue' & df$`Number of Embryo Rescued` >0 & df$`Days Elapsed` > 56, "Overdue",
                                               ifelse(df$Activity=='Embryo Rescue'  & df$`Number of Embryo Rescued` >0 & df$`Days Elapsed` %between% c(50,56), "Ready",
                                                  ifelse(df$Activity=='Embryo Rescue'  & df$`Number of Embryo Rescued` >0 & df$`Days Elapsed` %between% c(45, 49), "Approaching",
                                                         ifelse(df$Activity=='Embryo Rescue'  & df$`Number of Embryo Rescued` >0 & df$`Days Elapsed` < 45, "Wait",NA
                                                       
                                  ))))))))))))))))))))
scheduler = df[!is.na(df$status),]
scheduler = scheduler[status !='Wait']
scheduler$NextActivity = ifelse(scheduler$Activity=="First pollination","Repeat Pollination",
                                   ifelse(scheduler$Activity=="Repeat pollination","Bunch Harvesting",
                                          ifelse(scheduler$Activity=="Harvested bunches","Seed extraction",
                                                 ifelse(scheduler$Activity=="Seed extraction","Embryo Rescue",
                                                        ifelse(scheduler$Activity=="Embryo Rescue","Germination",'')))))
scheduler = scheduler[,c("Location", "Accession","Date", "Activity","Days Elapsed", "status", "NextActivity")]
colnames(scheduler)[4] = "CurrentActivity"
