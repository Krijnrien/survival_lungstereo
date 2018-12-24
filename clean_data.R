library(dplyr)

source("load_data.r")

# 1 = upper lobe, 2 = lower lobe, 3 is middle lobe
data["location_stat"] <- NA

# 1 LBK, 2 LOK, 3 RBK, 4 RMK, 5 ROK
data$location_stat[data$location == 1] <- 1
data$location_stat[data$location == 3] <- 1
data$location_stat[data$location == 2] <- 2
data$location_stat[data$location == 5] <- 2
data$location_stat[data$location == 4] <- 3



# Fill missing castor location with RTH location (which only has 3 options (missing left/right) compared to 5 castor locations)
data_NA_location <- data[which(is.na(data$location)),]
data_location <- data[which(!is.na(data$location)),]

data_NA_location$location_stat[data_NA_location$code_localisatie_doelgebied == 303] <- 1
data_NA_location$location_stat[data_NA_location$code_localisatie_doelgebied == 305] <- 2
data_NA_location$location_stat[data_NA_location$code_localisatie_doelgebied == 304] <- 3

data <- rbind(data_NA_location, data_location)
rm(data_NA_location, data_location)



# Fill missing castor SBRT schedule with schedule from RTH
data_NAschedule <- data[which(is.na(data$Schedule)),]
data_schedule <- data[which(!is.na(data$Schedule)),]

data_NAschedule$Schedule[data_NAschedule$cum_aantal_fracties == 3] <- 1
data_NAschedule$Schedule[data_NAschedule$cum_aantal_fracties == 5] <- 2
data_NAschedule$Schedule[data_NAschedule$cum_aantal_fracties == 8] <- 3
data_NAschedule$Schedule[data_NAschedule$cum_aantal_fracties == 12] <- 4

data <- rbind(data_schedule, data_NAschedule)
rm(data_NAschedule, data_schedule)



# Fill missing castor cN value with RTH extracted cT
data$cT <- ifelse(is.na(data$cT), data$cT_rth, data$cT)
# Fill missing castor cN value with RTH extracted cN
data$cN <- ifelse(is.na(data$cN), data$cN_rth, data$cN)
# Fill missing castor cM value with RTH extracted cM
data$cM <- ifelse(is.na(data$cM), data$cM_rth, data$cM)

# Only take first character (aka removing A and B notation from tnm-stage)
data$cT_stat <- substr(data$cT, 0, 1)
data$cN_stat <- substr(data$cN, 0, 1)
data$cM_stat <- substr(data$cM, 0, 1)


# Create binary survival value. If datum_overlijden_vlg_gba value is 2018-10-30 (date of export of GBA) then patient is still alive. Thus 0 (event didnt happen) otherwise make 1 (event happened)
data$survivalstat <- ifelse(data$datum_overlijden_vlg_gba == "2018-10-30", 0, 1)


# Calculate amount of days between death and finish treatment
data$diff_in_days <- difftime(data$datum_overlijden_vlg_gba, data$einddatum_cum_dosis, units = c("days"))

