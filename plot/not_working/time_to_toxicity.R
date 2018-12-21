library(ggplot2)
library(dplyr)

## REMOVE LATER, TEMPORARY
cat("\014") #clear console
rm(list=ls()) #clear memory
## REMOVE ABOVE

if(!exists("castor_hoest")){
  cat("No castor_hoest object found in memory: running prepare.r script!\n")
  source("clean.r")
}


castor_hoest <- comb[,c("Anoniem.nummer", "datum_overlijden_vlg_gba", "einddatum_cum_dosis", "Week1Hoest", "Week2Hoest", "Week3Hoest", "Datum1mnd", "Mnd1Hoest", "Datum6mnd", "Mnd6Hoest", "Datum12mnd", "Mnd12Hoest", "Datum24mnd", "Mnd24Hoest")]

castor_hoest[] <- lapply(castor_hoest, function(x) (as.character(x)))
castor_hoest[castor_hoest == ""] <- NA


#castor_hoest$einddatum_cum_dosis <- gsub("/", "-", castor_hoest$einddatum_cum_dosis)
castor_hoest$Datum1mnd <- gsub("/", "-", castor_hoest$Datum1mnd)
castor_hoest$Datum6mnd <- gsub("/", "-", castor_hoest$Datum6mnd)
castor_hoest$Datum12mnd <- gsub("/", "-", castor_hoest$Datum12mnd)
castor_hoest$Datum24mnd <- gsub("/", "-", castor_hoest$Datum24mnd)

castor_hoest$datum_overlijden_vlg_gba <- gsub("/", "-", castor_hoest$datum_overlijden_vlg_gba)
castor_hoest$einddatum_cum_dosis <- gsub("/", "-", castor_hoest$einddatum_cum_dosis)




#castor_hoest$einddatum_cum_dosis <- as.Date(castor_hoest$einddatum_cum_dosis, format = "%d-%m-%Y")
castor_hoest$Datum1mnd <- as.Date(castor_hoest$Datum1mnd, format = "%d-%m-%Y")
castor_hoest$Datum6mnd <- as.Date(castor_hoest$Datum6mnd, format = "%d-%m-%Y")
castor_hoest$Datum12mnd <- as.Date(castor_hoest$Datum12mnd, format = "%d-%m-%Y")
castor_hoest$Datum24mnd <- as.Date(castor_hoest$Datum24mnd, format = "%d-%m-%Y")

castor_hoest$datum_overlijden_vlg_gba <- as.Date(castor_hoest$datum_overlijden_vlg_gba, format = "%Y-%m-%d")
castor_hoest$einddatum_cum_dosis <- as.Date(castor_hoest$einddatum_cum_dosis, format = "%Y-%m-%d")

#castor_hoest$Datum1mnd <- ifelse(castor_hoest$Datum1mnd >= "2018-08-28", 1, castor_hoest$Datum1mnd)
#castor_hoest$Datum6mnd <- ifelse(castor_hoest$Datum6mnd > "2018-08-28", NA, castor_hoest$Datum6mnd)
#castor_hoest$Datum12mnd <- ifelse(castor_hoest$Datum12mnd > "2018-08-28", NA, castor_hoest$Datum12mnd)
#castor_hoest$Datum24mnd <- ifelse(castor_hoest$Datum24mnd > "2018-08-28", NA, castor_hoest$Datum24mnd)
#castor_hoest$Datum1mnd <- ifelse(castor_hoest$Datum1mnd == "2997-01-01", NA, castor_hoest$Datum1mnd)
#castor_hoest$survivalstat <- ifelse(castor_hoest$datum_overlijden_vlg_gba == "2018-10-30", 0, 1)


#castor_hoest$Datum1mnd_stat <- ifelse(castor_hoest$Datum1mnd > "2018-08-28", 0, 1)
#castor_hoest$Datum6mnd_stat <- ifelse(castor_hoest$Datum6mnd > "2018-08-28", 0, 1)
#castor_hoest$Datum12mnd_stat <- ifelse(castor_hoest$Datum12mnd > "2018-08-28", 0, 1)
#castor_hoest$Datum24mnd_stat <- ifelse(castor_hoest$Datum24mnd > "2018-08-28", 0, 1)

castor_hoest$survivalstat <- ifelse(castor_hoest$datum_overlijden_vlg_gba == "2018-10-30", 0, 1)
castor_hoest$diff_in_days <- difftime(castor_hoest$datum_overlijden_vlg_gba, castor_hoest$einddatum_cum_dosis, units = c("days"))




castor_hoest["Datum1Week"] <- 7
castor_hoest["Datum2Week"] <- 14
castor_hoest["Datum3Week"] <- 21

castor_hoest$Datum1mnd <- ifelse(castor_hoest$Datum1mnd > "2018-08-28" | is.na(castor_hoest$Datum1mnd), 31, difftime(castor_hoest$Datum1mnd, castor_hoest$einddatum_cum_dosis, units = c("days")))
castor_hoest$Datum6mnd <- ifelse(castor_hoest$Datum6mnd > "2018-08-28" | is.na(castor_hoest$Datum6mnd), 182, difftime(castor_hoest$Datum6mnd, castor_hoest$einddatum_cum_dosis, units = c("days")))
castor_hoest$Datum12mnd <- ifelse(castor_hoest$Datum12mnd > "2018-08-28" | is.na(castor_hoest$Datum12mnd), 365, difftime(castor_hoest$Datum12mnd, castor_hoest$einddatum_cum_dosis, units = c("days")))
castor_hoest$Datum24mnd <- ifelse(castor_hoest$Datum24mnd > "2018-08-28" | is.na(castor_hoest$Datum24mnd), 730, difftime(castor_hoest$Datum24mnd, castor_hoest$einddatum_cum_dosis, units = c("days")))

castor_hoest["ID"] <- seq.int(nrow(castor_hoest))


castor_hoest["first_hoest"] <- NA
castor_hoest["first_hoest"] <- ifelse(!is.na(castor_hoest$Week1Hoest), castor_hoest$Datum1Week, castor_hoest$first_hoest)
castor_hoest["first_hoest"] <- ifelse(is.na(castor_hoest$first_hoest) & !is.na(castor_hoest$Week2Hoest), castor_hoest$Datum2Week, castor_hoest$first_hoest)
castor_hoest["first_hoest"] <- ifelse(is.na(castor_hoest$first_hoest) & !is.na(castor_hoest$Week3Hoest), castor_hoest$Datum3Week, castor_hoest$first_hoest)
castor_hoest["first_hoest"] <- ifelse(is.na(castor_hoest$first_hoest) & !is.na(castor_hoest$Mnd1Hoest), castor_hoest$Datum1mnd, castor_hoest$first_hoest)
castor_hoest["first_hoest"] <- ifelse(is.na(castor_hoest$first_hoest) & !is.na(castor_hoest$Mnd6Hoest), castor_hoest$Datum6mnd, castor_hoest$first_hoest)
castor_hoest["first_hoest"] <- ifelse(is.na(castor_hoest$first_hoest) & !is.na(castor_hoest$Mnd12Hoest), castor_hoest$Datum12mnd, castor_hoest$first_hoest)
castor_hoest["first_hoest"] <- ifelse(is.na(castor_hoest$first_hoest) & !is.na(castor_hoest$Mnd24Hoest), castor_hoest$Datum24mnd, castor_hoest$first_hoest)


#castor_month1$Mnd1Hoest <- ifelse(castor_month1$Mnd1Hoest == 0 | is.na(castor_month1$Mnd1Hoest), 0, 1)

#apple <- head(castor_month1)

ggplot(castor_hoest, aes(x = ID)) + 
  
  # Plot line (dotted for censored time) representing time from t1 to t2
  geom_linerange(aes(ymin = 0, ymax = diff_in_days)) +  
 # geom_linerange(aes(ymin = stop_followup, ymax = 182, linetype = "dashed")) +
  
  # Plot points representing event
  # The ifelse() function moves censored marker to middle of interval
  geom_point(data = castor_hoest[which(!is.na(castor_hoest$first_hoest)),], aes(y = first_hoest), size = 1, color = "red") +
  # Flip coordinates
  coord_flip() + 
  
 # scale_linetype_manual(name = "Censoring", values = c(1, 2, 2), labels = c("Not censored", "Interval censored", "Interval censored")) +
  # Add custom shape scale.  Change the values to get different shapes.
 # scale_shape_manual(name = "Mnd1Hoest", values = c(18 ,19, 15)) +
  
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


#castor_hoestine all subsets
#rbind ofzo