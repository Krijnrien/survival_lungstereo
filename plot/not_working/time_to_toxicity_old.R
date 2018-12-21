library(ggplot2)
library(dplyr)

## REMOVE LATER, TEMPORARY
cat("\014") #clear console
rm(list=ls()) #clear memory
## REMOVE ABOVE

if(!exists("comb")){
  cat("No comb object found in memory: running prepare.r script!\n")
  source("clean.r")
}



#comb$einddatum_cum_dosis <- gsub("/", "-", comb$einddatum_cum_dosis)
comb$Datum1mnd <- gsub("/", "-", comb$Datum1mnd)
comb$Datum6mnd <- gsub("/", "-", comb$Datum6mnd)
comb$Datum12mnd <- gsub("/", "-", comb$Datum12mnd)
comb$Datum24mnd <- gsub("/", "-", comb$Datum24mnd)

#comb$einddatum_cum_dosis <- as.Date(comb$einddatum_cum_dosis, format = "%d-%m-%Y")
comb$Datum1mnd <- as.Date(comb$Datum1mnd, format = "%d-%m-%Y")
comb$Datum6mnd <- as.Date(comb$Datum6mnd, format = "%d-%m-%Y")
comb$Datum12mnd <- as.Date(comb$Datum12mnd, format = "%d-%m-%Y")
comb$Datum24mnd <- as.Date(comb$Datum24mnd, format = "%d-%m-%Y")


#comb$Datum1mnd <- ifelse(comb$Datum1mnd >= "2018-08-28", 1, comb$Datum1mnd)
#comb$Datum6mnd <- ifelse(comb$Datum6mnd > "2018-08-28", NA, comb$Datum6mnd)
#comb$Datum12mnd <- ifelse(comb$Datum12mnd > "2018-08-28", NA, comb$Datum12mnd)
#comb$Datum24mnd <- ifelse(comb$Datum24mnd > "2018-08-28", NA, comb$Datum24mnd)
#comb$Datum1mnd <- ifelse(comb$Datum1mnd == "2997-01-01", NA, comb$Datum1mnd)
#comb$survivalstat <- ifelse(comb$datum_overlijden_vlg_gba == "2018-10-30", 0, 1)


#comb$Datum1mnd_stat <- ifelse(comb$Datum1mnd > "2018-08-28", 0, 1)
#comb$Datum6mnd_stat <- ifelse(comb$Datum6mnd > "2018-08-28", 0, 1)
#comb$Datum12mnd_stat <- ifelse(comb$Datum12mnd > "2018-08-28", 0, 1)
#comb$Datum24mnd_stat <- ifelse(comb$Datum24mnd > "2018-08-28", 0, 1)


comb["start_followup"] <- NA
comb["stop_followup"] <- NA
comb["censored"] <- NA

#subset all toxicity follow ups
castor_week1 <- comb[,c("Anoniem.nummer", "einddatum_cum_dosis", "start_followup", "stop_followup", "censored", "Week1Pneumonitis", "Week1Hoest", "Week1Dyspnoe", "Week1Anorexia", "Week1Oesophagitis", "Week1Dermatitis", "Week1Osteoradionecrose")]
castor_week2 <- comb[,c("Anoniem.nummer", "einddatum_cum_dosis", "start_followup", "stop_followup", "censored", "Week2Pneumonitis", "Week2Hoest", "Week2Dyspnoe", "Week2Anorexia", "Week2Oesophagitis", "Week2Dermatitis", "Week2Osteoradionecrose")]
castor_week3 <- comb[,c("Anoniem.nummer", "einddatum_cum_dosis", "start_followup", "stop_followup", "censored", "Week3Pneumonitis", "Week3Hoest", "Week3Dyspnoe", "Week3Anorexia", "Week3Oesophagitis", "Week3Dermatitis", "Week3Osteoradionecrose")]
castor_month1 <- comb[,c("Anoniem.nummer", "einddatum_cum_dosis", "start_followup", "stop_followup", "censored", "Datum1mnd", "Mnd1Pneumonitis", "Mnd1Hoest", "Mnd1Dyspnoe", "Mnd1Anorexia", "Mnd1Oesophagitis", "Mnd1Dermatitis", "Mnd1Osteoradionecrose")]
castor_month6 <- comb[,c("Anoniem.nummer", "einddatum_cum_dosis", "start_followup", "stop_followup", "censored", "Datum1mnd", "Datum6mnd","Mnd6Pneumonitis", "Mnd6Hoest", "Mnd6Dyspnoe", "Mnd6Anorexia", "Mnd6Oesophagitis", "Mnd6Dermatitis", "Mnd6Osteoradionecrose")]
castor_month12 <- comb[,c("Anoniem.nummer", "einddatum_cum_dosis", "start_followup", "stop_followup", "censored", "Datum6mnd","Datum12mnd", "Mnd12Pneumonitis", "Mnd12Hoest", "Mnd12Dyspnoe", "Mnd12Anorexia", "Mnd12Oesophagitis", "Mnd12Dermatitis", "Mnd12Osteoradionecrose")]
castor_month24 <- comb[,c("Anoniem.nummer", "einddatum_cum_dosis", "start_followup", "stop_followup", "censored", "Datum12mnd", "Datum24mnd", "Mnd24Pneumonitis", "Mnd24Hoest", "Mnd24Dyspnoe", "Mnd24Anorexia", "Mnd24Oesophagitis", "Mnd24Dermatitis", "Mnd24Osteoradionecrose")]


# if datum is filled in (Not NA) and datum_stat is 0 (a valid date) but the toxicity values are NA then update all toxicity values to 0
castor_month1 <- castor_month1 %>% mutate_at(.vars = c("Mnd1Pneumonitis", "Mnd1Hoest", "Mnd1Dyspnoe", "Mnd1Anorexia", "Mnd1Oesophagitis", "Mnd1Dermatitis", "Mnd1Osteoradionecrose"), funs(ifelse(!is.na(castor_month1$Datum1mnd) & is.na(castor_month1$Mnd1Hoest), 0, .)))
castor_month6 <- castor_month6 %>% mutate_at(.vars = c("Mnd6Pneumonitis", "Mnd6Hoest", "Mnd6Dyspnoe", "Mnd6Anorexia", "Mnd6Oesophagitis", "Mnd6Dermatitis", "Mnd6Osteoradionecrose"), funs(ifelse(!is.na(castor_month6$Datum6mnd) & is.na(castor_month6$Mnd6Hoest), 0, .)))
castor_month12 <- castor_month12 %>% mutate_at(.vars = c("Mnd12Pneumonitis", "Mnd12Hoest", "Mnd12Dyspnoe", "Mnd12Anorexia", "Mnd12Oesophagitis", "Mnd12Dermatitis", "Mnd12Osteoradionecrose"), funs(ifelse(!is.na(castor_month12$Datum12mnd) & is.na(castor_month12$Mnd12Hoest), 0, .)))
castor_month24 <- castor_month24 %>% mutate_at(.vars = c("Mnd24Pneumonitis", "Mnd24Hoest", "Mnd24Dyspnoe", "Mnd24Anorexia", "Mnd24Oesophagitis", "Mnd24Dermatitis", "Mnd24Osteoradionecrose"), funs(ifelse(!is.na(castor_month24$Datum24mnd) & is.na(castor_month24$Mnd24Hoest), 0, .)))


castor_week1$censored <- ifelse(is.na(castor_week1$Week1Hoest), 1, 0)
castor_week2$censored <- ifelse(is.na(castor_week2$Week2Hoest), 1, 0)
castor_week3$censored <- ifelse(is.na(castor_week3$Week3Hoest), 1, 0)
castor_month1$censored <- ifelse(is.na(castor_month1$Mnd1Hoest), 1, 0)
castor_month6$censored <- ifelse(is.na(castor_month6$Mnd6Hoest), 1, 0)
castor_month12$censored <- ifelse(is.na(castor_month12$Mnd12Hoest), 1, 0)
castor_month24$censored <- ifelse(is.na(castor_month24$Mnd24Hoest), 1, 0)


castor_week1$start_followup <- 0
castor_week1$stop_followup <- 7

castor_week2$start_followup <- 7
castor_week2$stop_followup <- 14

castor_week3$start_followup <- 14
castor_week3$stop_followup <- 21


castor_month1$start_followup <- 21
castor_month1$stop_followup <- ifelse(comb$Datum1mnd > "2018-08-28" | is.na(comb$Datum1mnd), 31, difftime(comb$Datum1mnd, comb$einddatum_cum_dosis, units = c("days")))

castor_month6$start_followup <- ifelse(comb$Datum1mnd > "2018-08-28" | is.na(comb$Datum1mnd), 31, difftime(comb$Datum1mnd, comb$einddatum_cum_dosis, units = c("days")))
castor_month6$stop_followup <- ifelse(comb$Datum6mnd > "2018-08-28" | is.na(comb$Datum6mnd), 182, difftime(comb$Datum6mnd, comb$einddatum_cum_dosis, units = c("days")))

castor_month12$start_followup <- ifelse(comb$Datum6mnd > "2018-08-28" | is.na(comb$Datum6mnd), 182, difftime(comb$Datum6mnd, comb$einddatum_cum_dosis, units = c("days")))
castor_month12$stop_followup <- ifelse(comb$Datum12mnd > "2018-08-28" | is.na(comb$Datum12mnd), 365, difftime(comb$Datum12mnd, comb$einddatum_cum_dosis, units = c("days")))

castor_month24$start_followup <- ifelse(comb$Datum12mnd > "2018-08-28" | is.na(comb$Datum12mnd), 365, difftime(comb$Datum12mnd, comb$einddatum_cum_dosis, units = c("days")))
castor_month24$stop_followup <- ifelse(comb$Datum24mnd > "2018-08-28" | is.na(comb$Datum24mnd), 730, difftime(comb$Datum24mnd, comb$einddatum_cum_dosis, units = c("days")))


castor_month1$start_followup <- ifelse(castor_month1$start_followup < 21, 21, castor_month1$start_followup)
castor_month1$stop_followup <- ifelse(castor_month1$stop_followup > 182, 182, castor_month1$stop_followup)
castor_month1$stop_followup <- ifelse(castor_month1$stop_followup < 21, 21, castor_month1$stop_followup)

#castor_month1_valid <- castor_month1[which(castor_month1$Datum1mnd > castor_month1$einddatum_cum_dosis),]
#castor_month1_invalid <- castor_month1[which(castor_month1$Datum1mnd < castor_month1$einddatum_cum_dosis),]


castor_month1["ID"] <- seq.int(nrow(castor_month1))

#castor_month1$Mnd1Hoest <- ifelse(castor_month1$Mnd1Hoest == 0 | is.na(castor_month1$Mnd1Hoest), 0, 1)

#apple <- head(castor_month1)

ggplot(castor_month1, aes(x = ID)) + 
  
  # Plot line (dotted for censored time) representing time from t1 to t2
  geom_linerange(aes(ymin = start_followup, ymax = stop_followup, linetype = as.factor(censored))) +  
 # geom_linerange(aes(ymin = stop_followup, ymax = 182, linetype = "dashed")) +
  
  # Plot points representing event
  # The ifelse() function moves censored marker to middle of interval
  geom_point(data = castor_month1[which(castor_month1$Mnd1Hoest == 1),], aes(y = stop_followup, shape = "a"), size = 1.5) +
  # Flip coordinates
  coord_flip() + 
  
  scale_linetype_manual(name = "Censoring", values = c(1, 2, 2), labels = c("Not censored", "Interval censored", "Interval censored")) +
  # Add custom shape scale.  Change the values to get different shapes.
  scale_shape_manual(name = "Mnd1Hoest", values = c(18 ,19, 15)) +
  
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



#combine all subsets
#rbind ofzo