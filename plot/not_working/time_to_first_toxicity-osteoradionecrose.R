library(ggplot2)
library(dplyr)

## REMOVE LATER, TEMPORARY
cat("\014") #clear console
rm(list=ls()) #clear memory
## REMOVE ABOVE

if(!exists("comb")){
  cat("No castor_osteoradionecrose object found in memory: running prepare.r script!\n")
  source("clean.r")
}


castor_osteoradionecrose <- comb[,c("Anoniem.nummer", "datum_overlijden_vlg_gba", "einddatum_cum_dosis", "Week1Osteoradionecrose", "Week2Osteoradionecrose", "Week3Osteoradionecrose", "Datum1mnd", "Mnd1Osteoradionecrose", "Datum6mnd", "Mnd6Osteoradionecrose", "Datum12mnd", "Mnd12Osteoradionecrose", "Datum24mnd", "Mnd24Osteoradionecrose")]

castor_osteoradionecrose[] <- lapply(castor_osteoradionecrose, function(x) (as.character(x)))
castor_osteoradionecrose[castor_osteoradionecrose == ""] <- NA


#castor_osteoradionecrose$einddatum_cum_dosis <- gsub("/", "-", castor_osteoradionecrose$einddatum_cum_dosis)
castor_osteoradionecrose$Datum1mnd <- gsub("/", "-", castor_osteoradionecrose$Datum1mnd)
castor_osteoradionecrose$Datum6mnd <- gsub("/", "-", castor_osteoradionecrose$Datum6mnd)
castor_osteoradionecrose$Datum12mnd <- gsub("/", "-", castor_osteoradionecrose$Datum12mnd)
castor_osteoradionecrose$Datum24mnd <- gsub("/", "-", castor_osteoradionecrose$Datum24mnd)

castor_osteoradionecrose$datum_overlijden_vlg_gba <- gsub("/", "-", castor_osteoradionecrose$datum_overlijden_vlg_gba)
castor_osteoradionecrose$einddatum_cum_dosis <- gsub("/", "-", castor_osteoradionecrose$einddatum_cum_dosis)




#castor_osteoradionecrose$einddatum_cum_dosis <- as.Date(castor_osteoradionecrose$einddatum_cum_dosis, format = "%d-%m-%Y")
castor_osteoradionecrose$Datum1mnd <- as.Date(castor_osteoradionecrose$Datum1mnd, format = "%d-%m-%Y")
castor_osteoradionecrose$Datum6mnd <- as.Date(castor_osteoradionecrose$Datum6mnd, format = "%d-%m-%Y")
castor_osteoradionecrose$Datum12mnd <- as.Date(castor_osteoradionecrose$Datum12mnd, format = "%d-%m-%Y")
castor_osteoradionecrose$Datum24mnd <- as.Date(castor_osteoradionecrose$Datum24mnd, format = "%d-%m-%Y")

castor_osteoradionecrose$datum_overlijden_vlg_gba <- as.Date(castor_osteoradionecrose$datum_overlijden_vlg_gba, format = "%Y-%m-%d")
castor_osteoradionecrose$einddatum_cum_dosis <- as.Date(castor_osteoradionecrose$einddatum_cum_dosis, format = "%Y-%m-%d")

#castor_osteoradionecrose$Datum1mnd <- ifelse(castor_osteoradionecrose$Datum1mnd >= "2018-08-28", 1, castor_osteoradionecrose$Datum1mnd)
#castor_osteoradionecrose$Datum6mnd <- ifelse(castor_osteoradionecrose$Datum6mnd > "2018-08-28", NA, castor_osteoradionecrose$Datum6mnd)
#castor_osteoradionecrose$Datum12mnd <- ifelse(castor_osteoradionecrose$Datum12mnd > "2018-08-28", NA, castor_osteoradionecrose$Datum12mnd)
#castor_osteoradionecrose$Datum24mnd <- ifelse(castor_osteoradionecrose$Datum24mnd > "2018-08-28", NA, castor_osteoradionecrose$Datum24mnd)
#castor_osteoradionecrose$Datum1mnd <- ifelse(castor_osteoradionecrose$Datum1mnd == "2997-01-01", NA, castor_osteoradionecrose$Datum1mnd)
#castor_osteoradionecrose$survivalstat <- ifelse(castor_osteoradionecrose$datum_overlijden_vlg_gba == "2018-10-30", 0, 1)


#castor_osteoradionecrose$Datum1mnd_stat <- ifelse(castor_osteoradionecrose$Datum1mnd > "2018-08-28", 0, 1)
#castor_osteoradionecrose$Datum6mnd_stat <- ifelse(castor_osteoradionecrose$Datum6mnd > "2018-08-28", 0, 1)
#castor_osteoradionecrose$Datum12mnd_stat <- ifelse(castor_osteoradionecrose$Datum12mnd > "2018-08-28", 0, 1)
#castor_osteoradionecrose$Datum24mnd_stat <- ifelse(castor_osteoradionecrose$Datum24mnd > "2018-08-28", 0, 1)

castor_osteoradionecrose$survivalstat <- ifelse(castor_osteoradionecrose$datum_overlijden_vlg_gba == "2018-10-30", 0, 1)
castor_osteoradionecrose$diff_in_days <- difftime(castor_osteoradionecrose$datum_overlijden_vlg_gba, castor_osteoradionecrose$einddatum_cum_dosis, units = c("days"))




castor_osteoradionecrose["Datum1Week"] <- 7
castor_osteoradionecrose["Datum2Week"] <- 14
castor_osteoradionecrose["Datum3Week"] <- 21

castor_osteoradionecrose$Datum1mnd <- ifelse(castor_osteoradionecrose$Datum1mnd > "2018-08-28" | is.na(castor_osteoradionecrose$Datum1mnd), 31, difftime(castor_osteoradionecrose$Datum1mnd, castor_osteoradionecrose$einddatum_cum_dosis, units = c("days")))
castor_osteoradionecrose$Datum6mnd <- ifelse(castor_osteoradionecrose$Datum6mnd > "2018-08-28" | is.na(castor_osteoradionecrose$Datum6mnd), 182, difftime(castor_osteoradionecrose$Datum6mnd, castor_osteoradionecrose$einddatum_cum_dosis, units = c("days")))
castor_osteoradionecrose$Datum12mnd <- ifelse(castor_osteoradionecrose$Datum12mnd > "2018-08-28" | is.na(castor_osteoradionecrose$Datum12mnd), 365, difftime(castor_osteoradionecrose$Datum12mnd, castor_osteoradionecrose$einddatum_cum_dosis, units = c("days")))
castor_osteoradionecrose$Datum24mnd <- ifelse(castor_osteoradionecrose$Datum24mnd > "2018-08-28" | is.na(castor_osteoradionecrose$Datum24mnd), 730, difftime(castor_osteoradionecrose$Datum24mnd, castor_osteoradionecrose$einddatum_cum_dosis, units = c("days")))

castor_osteoradionecrose["ID"] <- seq.int(nrow(castor_osteoradionecrose))


castor_osteoradionecrose["first_Osteoradionecrose"] <- NA
castor_osteoradionecrose["first_Osteoradionecrose"] <- ifelse(!is.na(castor_osteoradionecrose$Week1Osteoradionecrose) & castor_osteoradionecrose$Week1Osteoradionecrose, castor_osteoradionecrose$Datum1Week, castor_osteoradionecrose$first_Osteoradionecrose)
castor_osteoradionecrose["first_Osteoradionecrose"] <- ifelse(is.na(castor_osteoradionecrose$first_Osteoradionecrose) & castor_osteoradionecrose$Week2Osteoradionecrose > 0, castor_osteoradionecrose$Datum2Week, castor_osteoradionecrose$first_Osteoradionecrose)
castor_osteoradionecrose["first_Osteoradionecrose"] <- ifelse(is.na(castor_osteoradionecrose$first_Osteoradionecrose) & castor_osteoradionecrose$Week3Osteoradionecrose > 0, castor_osteoradionecrose$Datum3Week, castor_osteoradionecrose$first_Osteoradionecrose)
castor_osteoradionecrose["first_Osteoradionecrose"] <- ifelse(is.na(castor_osteoradionecrose$first_Osteoradionecrose) & castor_osteoradionecrose$Mnd1Osteoradionecrose > 0, castor_osteoradionecrose$Datum1mnd, castor_osteoradionecrose$first_Osteoradionecrose)
castor_osteoradionecrose["first_Osteoradionecrose"] <- ifelse(is.na(castor_osteoradionecrose$first_Osteoradionecrose) & castor_osteoradionecrose$Mnd6Osteoradionecrose > 0, castor_osteoradionecrose$Datum6mnd, castor_osteoradionecrose$first_Osteoradionecrose)
castor_osteoradionecrose["first_Osteoradionecrose"] <- ifelse(is.na(castor_osteoradionecrose$first_Osteoradionecrose) & castor_osteoradionecrose$Mnd12Osteoradionecrose > 0, castor_osteoradionecrose$Datum12mnd, castor_osteoradionecrose$first_Osteoradionecrose)
castor_osteoradionecrose["first_Osteoradionecrose"] <- ifelse(is.na(castor_osteoradionecrose$first_Osteoradionecrose) & castor_osteoradionecrose$Mnd24Osteoradionecrose > 0, castor_osteoradionecrose$Datum24mnd, castor_osteoradionecrose$first_Osteoradionecrose)


#castor_month1$Mnd1Osteoradionecrose <- ifelse(castor_month1$Mnd1Osteoradionecrose == 0 | is.na(castor_month1$Mnd1Osteoradionecrose), 0, 1)

#apple <- head(castor_month1)

ggplot(castor_osteoradionecrose, aes(x = ID)) + 
  
  # Plot line (dotted for censored time) representing time from t1 to t2
  geom_linerange(aes(ymin = 0, ymax = diff_in_days)) +  
 # geom_linerange(aes(ymin = stop_followup, ymax = 182, linetype = "dashed")) +
  
  # Plot points representing event
  # The ifelse() function moves censored marker to middle of interval
  geom_point(data = castor_osteoradionecrose[which(!is.na(castor_osteoradionecrose$first_Osteoradionecrose)),], aes(y = first_Osteoradionecrose), size = 1, color = "red") +
  # Flip coordinates
  coord_flip() + 
  
 # scale_linetype_manual(name = "Censoring", values = c(1, 2, 2), labels = c("Not censored", "Interval censored", "Interval censored")) +
  # Add custom shape scale.  Change the values to get different shapes.
 # scale_shape_manual(name = "Mnd1Osteoradionecrose", values = c(18 ,19, 15)) +
  
  # Add custom name to linetype scale, 
  # otherwise it will default to "as.factor(censored))"
  #scale_linetype_manual(name = "Censoring", values = c(1, 2), labels = c("Not censored", "Interval censored")) +
  # Add custom shape scale.  Change the values to get different shapes.
  #scale_shape_manual(name = "Event", values = c(19, 15)) +
  # Add main title and axis labels
  #opts(title = "Patient follow-up") + xlab("Patient ID") +  ylab("Days") + 
  # I think the bw theme looks better for this graph, 
  # but leave it out if you prefer the default theme
  theme_bw()


#castor_osteoradionecroseine all subsets
#rbind ofzo