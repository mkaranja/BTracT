rm(list=ls(all=T))
cat("\014")
library(dplyr)
library(lubridate)
library(mailR)
library(magrittr)
library(data.table)
library(WriteXLS)

source("~/BANANA/BTRACT/BTracT_Reports/btract_scheduled_reports.R")

# Generate BTracT daily reports

daily_count = cleantable %>%
  dplyr::filter(Date==Sys.Date()) %>%
  dplyr::group_by(Location)

loc = unique(daily_count$Location)

for(k in 1:length(loc)){
  fname <- paste0("/srv/shiny-server/btract/btract/Reports/Daily/",loc[k],"-BTracTDailyReport-",Sys.Date(),".xls")
  summaries = cleantable %>%
    dplyr::filter(Location==loc[k], Date==Sys.Date()) %>%
    dplyr::group_by(Activity) %>%
    dplyr::tally() %>%
    dplyr::collect()
  
  colnames(summaries)[2] = "Number reported"
  
  details = cleantable %>%
    dplyr::filter(Location == loc[k], Date==Sys.Date())
  
  WriteXLS::WriteXLS(c("summaries","details"), fname, perl = "perl")
}

arusha_report = paste0("/srv/shiny-server/btract/btract/Reports/Daily/Arusha-BTracTDailyReport-",Sys.Date(),".xls")
sendusu_report = paste0("/srv/shiny-server/btract/btract/Reports/Daily/Sendusu-BTracTDailyReport-",Sys.Date(),".xls")

# Send the reports via email
if(file.exists(arusha_report)){
send.mail(from = "bananatrackertool@gmail.com",
          to = c("S.Bayo@cgiar.org","m.karanja@cgiar.org"),
          subject = paste("Arusha-BTracT Daily Report-",Sys.Date()),
          body = paste("Hello Stanley, attached find today's btract records from Arusha. Bests"), 
          html = TRUE,
          smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "bananatrackertool@gmail.com", passwd = "Btract101", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE,
          attach.files = paste0("/srv/shiny-server/btract/btract/Reports/Daily/Arusha-BTracTDailyReport-",Sys.Date(),".xls"),debug = F) # srv/shiny-server/btract/btract

} else {
  send.mail(from = "bananatrackertool@gmail.com",
            to = c("S.Bayo@cgiar.org","m.karanja@cgiar.org"),
            subject = paste("NO DATA-BTracT Daily Report-",Sys.Date()),
            body = paste("Hello Stanley, please note there are no records from Arusha today. Bests"), 
            html = TRUE,
            smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "bananatrackertool@gmail.com", passwd = "Btract101", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE,debug = F)
  
}

if(file.exists(sendusu_report)){
send.mail(from = "bananatrackertool@gmail.com",
          to = c("V.Akech@cgiar.org","m.karanja@cgiar.org"),  
          subject = paste("Sendusu-BTracT Daily Report-",Sys.Date()),
          body = paste("Hello Violet, attached find today's btract records from Sendusu. Bests"), 
          smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "bananatrackertool@gmail.com", passwd = "Btract101", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE,
          attach.files = paste0("/srv/shiny-server/btract/btract/Reports/Daily/Sendusu-BTracTDailyReport-",Sys.Date(),".xls"),debug = F) 
} else {
  send.mail(from = "bananatrackertool@gmail.com",
            to = c("V.Akech@cgiar.org","m.karanja@cgiar.org"),  
            subject = paste("NO DATA-BTracT Daily Report-",Sys.Date()),
            body = paste("Hello Violet, please note there are no records from Sendusu today. Bests"), 
            html = TRUE,
            smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "bananatrackertool@gmail.com", passwd = "Btract101", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE,debug = F)
}

# END






