library(ggplot2)
library(dplyr)

## REMOVE LATER, TEMPORARY
cat("\014") #clear console
rm(list=ls()) #clear memory
## REMOVE ABOVE

if(!exists("comb")){
  cat("No castor_pneumonitis object found in memory: running prepare.r script!\n")
  source("clean.r")
}


castor_pneumonitis <- comb[,c("Anoniem.nummer", "datum_overlijden_vlg_gba", "einddatum_cum_dosis", "Week1Pneumonitis", "Week2Pneumonitis", "Week3Pneumonitis", "Datum1mnd", "Mnd1Pneumonitis", "Datum6mnd", "Mnd6Pneumonitis", "Datum12mnd", "Mnd12Pneumonitis", "Datum24mnd", "Mnd24Pneumonitis")]

castor_pneumonitis[] <- lapply(castor_pneumonitis, function(x) (as.character(x)))
castor_pneumonitis[castor_pneumonitis == ""] <- NA


#castor_pneumonitis$einddatum_cum_dosis <- gsub("/", "-", castor_pneumonitis$einddatum_cum_dosis)
castor_pneumonitis$Datum1mnd <- gsub("/", "-", castor_pneumonitis$Datum1mnd)
castor_pneumonitis$Datum6mnd <- gsub("/", "-", castor_pneumonitis$Datum6mnd)
castor_pneumonitis$Datum12mnd <- gsub("/", "-", castor_pneumonitis$Datum12mnd)
castor_pneumonitis$Datum24mnd <- gsub("/", "-", castor_pneumonitis$Datum24mnd)

castor_pneumonitis$datum_overlijden_vlg_gba <- gsub("/", "-", castor_pneumonitis$datum_overlijden_vlg_gba)
castor_pneumonitis$einddatum_cum_dosis <- gsub("/", "-", castor_pneumonitis$einddatum_cum_dosis)




#castor_pneumonitis$einddatum_cum_dosis <- as.Date(castor_pneumonitis$einddatum_cum_dosis, format = "%d-%m-%Y")
castor_pneumonitis$Datum1mnd <- as.Date(castor_pneumonitis$Datum1mnd, format = "%d-%m-%Y")
castor_pneumonitis$Datum6mnd <- as.Date(castor_pneumonitis$Datum6mnd, format = "%d-%m-%Y")
castor_pneumonitis$Datum12mnd <- as.Date(castor_pneumonitis$Datum12mnd, format = "%d-%m-%Y")
castor_pneumonitis$Datum24mnd <- as.Date(castor_pneumonitis$Datum24mnd, format = "%d-%m-%Y")

castor_pneumonitis$datum_overlijden_vlg_gba <- as.Date(castor_pneumonitis$datum_overlijden_vlg_gba, format = "%Y-%m-%d")
castor_pneumonitis$einddatum_cum_dosis <- as.Date(castor_pneumonitis$einddatum_cum_dosis, format = "%Y-%m-%d")

#castor_pneumonitis$Datum1mnd <- ifelse(castor_pneumonitis$Datum1mnd >= "2018-08-28", 1, castor_pneumonitis$Datum1mnd)
#castor_pneumonitis$Datum6mnd <- ifelse(castor_pneumonitis$Datum6mnd > "2018-08-28", NA, castor_pneumonitis$Datum6mnd)
#castor_pneumonitis$Datum12mnd <- ifelse(castor_pneumonitis$Datum12mnd > "2018-08-28", NA, castor_pneumonitis$Datum12mnd)
#castor_pneumonitis$Datum24mnd <- ifelse(castor_pneumonitis$Datum24mnd > "2018-08-28", NA, castor_pneumonitis$Datum24mnd)
#castor_pneumonitis$Datum1mnd <- ifelse(castor_pneumonitis$Datum1mnd == "2997-01-01", NA, castor_pneumonitis$Datum1mnd)
#castor_pneumonitis$survivalstat <- ifelse(castor_pneumonitis$datum_overlijden_vlg_gba == "2018-10-30", 0, 1)


#castor_pneumonitis$Datum1mnd_stat <- ifelse(castor_pneumonitis$Datum1mnd > "2018-08-28", 0, 1)
#castor_pneumonitis$Datum6mnd_stat <- ifelse(castor_pneumonitis$Datum6mnd > "2018-08-28", 0, 1)
#castor_pneumonitis$Datum12mnd_stat <- ifelse(castor_pneumonitis$Datum12mnd > "2018-08-28", 0, 1)
#castor_pneumonitis$Datum24mnd_stat <- ifelse(castor_pneumonitis$Datum24mnd > "2018-08-28", 0, 1)

castor_pneumonitis$survivalstat <- ifelse(castor_pneumonitis$datum_overlijden_vlg_gba == "2018-10-30", 0, 1)
castor_pneumonitis$diff_in_days <- difftime(castor_pneumonitis$datum_overlijden_vlg_gba, castor_pneumonitis$einddatum_cum_dosis, units = c("days"))




castor_pneumonitis["Datum1Week"] <- 7
castor_pneumonitis["Datum2Week"] <- 14
castor_pneumonitis["Datum3Week"] <- 21

castor_pneumonitis$Datum1mnd <- ifelse(castor_pneumonitis$Datum1mnd > "2018-08-28" | is.na(castor_pneumonitis$Datum1mnd), 31, difftime(castor_pneumonitis$Datum1mnd, castor_pneumonitis$einddatum_cum_dosis, units = c("days")))
castor_pneumonitis$Datum6mnd <- ifelse(castor_pneumonitis$Datum6mnd > "2018-08-28" | is.na(castor_pneumonitis$Datum6mnd), 182, difftime(castor_pneumonitis$Datum6mnd, castor_pneumonitis$einddatum_cum_dosis, units = c("days")))
castor_pneumonitis$Datum12mnd <- ifelse(castor_pneumonitis$Datum12mnd > "2018-08-28" | is.na(castor_pneumonitis$Datum12mnd), 365, difftime(castor_pneumonitis$Datum12mnd, castor_pneumonitis$einddatum_cum_dosis, units = c("days")))
castor_pneumonitis$Datum24mnd <- ifelse(castor_pneumonitis$Datum24mnd > "2018-08-28" | is.na(castor_pneumonitis$Datum24mnd), 730, difftime(castor_pneumonitis$Datum24mnd, castor_pneumonitis$einddatum_cum_dosis, units = c("days")))

castor_pneumonitis["ID"] <- seq.int(nrow(castor_pneumonitis))


castor_pneumonitis["first_Pneumonitis"] <- NA
castor_pneumonitis["first_Pneumonitis"] <- ifelse(!is.na(castor_pneumonitis$Week1Pneumonitis), castor_pneumonitis$Datum1Week, castor_pneumonitis$first_Pneumonitis)
castor_pneumonitis["first_Pneumonitis"] <- ifelse(is.na(castor_pneumonitis$first_Pneumonitis) & !is.na(castor_pneumonitis$Week2Pneumonitis), castor_pneumonitis$Datum2Week, castor_pneumonitis$first_Pneumonitis)
castor_pneumonitis["first_Pneumonitis"] <- ifelse(is.na(castor_pneumonitis$first_Pneumonitis) & !is.na(castor_pneumonitis$Week3Pneumonitis), castor_pneumonitis$Datum3Week, castor_pneumonitis$first_Pneumonitis)
castor_pneumonitis["first_Pneumonitis"] <- ifelse(is.na(castor_pneumonitis$first_Pneumonitis) & !is.na(castor_pneumonitis$Mnd1Pneumonitis), castor_pneumonitis$Datum1mnd, castor_pneumonitis$first_Pneumonitis)
castor_pneumonitis["first_Pneumonitis"] <- ifelse(is.na(castor_pneumonitis$first_Pneumonitis) & !is.na(castor_pneumonitis$Mnd6Pneumonitis), castor_pneumonitis$Datum6mnd, castor_pneumonitis$first_Pneumonitis)
castor_pneumonitis["first_Pneumonitis"] <- ifelse(is.na(castor_pneumonitis$first_Pneumonitis) & !is.na(castor_pneumonitis$Mnd12Pneumonitis), castor_pneumonitis$Datum12mnd, castor_pneumonitis$first_Pneumonitis)
castor_pneumonitis["first_Pneumonitis"] <- ifelse(is.na(castor_pneumonitis$first_Pneumonitis) & !is.na(castor_pneumonitis$Mnd24Pneumonitis), castor_pneumonitis$Datum24mnd, castor_pneumonitis$first_Pneumonitis)


#castor_month1$Mnd1Pneumonitis <- ifelse(castor_month1$Mnd1Pneumonitis == 0 | is.na(castor_month1$Mnd1Pneumonitis), 0, 1)

#apple <- head(castor_month1)

ggplot(castor_pneumonitis, aes(x = ID)) + 
  
  # Plot line (dotted for censored time) representing time from t1 to t2
  geom_linerange(aes(ymin = 0, ymax = diff_in_days)) +  
 # geom_linerange(aes(ymin = stop_followup, ymax = 182, linetype = "dashed")) +
  
  # Plot points representing event
  # The ifelse() function moves censored marker to middle of interval
  geom_point(data = castor_pneumonitis[which(!is.na(castor_pneumonitis$first_Pneumonitis)),], aes(y = first_Pneumonitis), size = 1, color = "red") +
  # Flip coordinates
  coord_flip() + 
  
 # scale_linetype_manual(name = "Censoring", values = c(1, 2, 2), labels = c("Not censored", "Interval censored", "Interval censored")) +
  # Add custom shape scale.  Change the values to get different shapes.
 # scale_shape_manual(name = "Mnd1Pneumonitis", values = c(18 ,19, 15)) +
  
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


#castor_pneumonitisine all subsets
#rbind ofzo