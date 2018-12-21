library(survival)
library(survminer)
library(dplyr)

## REMOVE LATER, TEMPORARY
cat("\014") #clear console
rm(list=ls()) #clear memory
## REMOVE ABOVE

if(!exists("comb")){
  cat("No comb object found in memory: running prepare.r script!\n")
  source("clean.r")
}


comb <- comb[which(!is.na(comb$origin)),]

#comb <- comb[c("Anoniem.nummer", "diff_in_days","origin", "survivalstat", "originstat"),]


#comb$originstat[is.na(comb$origin)] <- 1
#comb$originstat[which(comb$origin == 1)] <- 1

comb["originstat"] <- NA


#comb$originstat <- ifelse(comb$origin == 1, comb$origin, NA)

#comb$originstat <- ifelse( comb$origin == 2 | comb$origin == 3 | comb$origin == 4 | comb$origin == 5 | comb$origin == 6 | comb$origin == 9, 2, comb$originstat)

#comb$originstat <- ifelse(comb$origin == 1 | is.na(comb$origin), 1, comb$originstat)
comb$originstat[comb$origin == 1] <- 1

# 2 is lung metastasen
#comb$originstat[comb$origin == 2] <- 2
comb$originstat[comb$origin == 3] <- 2
comb$originstat[comb$origin == 4] <- 2
comb$originstat[comb$origin == 5] <- 2
comb$originstat[comb$origin == 6] <- 2
comb$originstat[comb$origin == 9] <- 2



# 1 adenoca, 2 squamous cell carcinoma, 3 carconima NOS, 4 sarcoma ,5 BAC, 6 other malignancy, 9 no PA/ proved malignancy
comb$originstat <- factor(comb$originstat, levels = c("1", "2"), labels = c("primair", "metastases"))

fit <- survfit(Surv(comb$diff_in_days, comb$survivalstat) ~ originstat, data = comb)


ggsurv <- ggsurvplot(
  fit, 
  data = comb,
  
  pval = TRUE,
  pval.size = 4,
  pval.method = TRUE,
  pval.method.size = 3,
  log.rank.weights = "1",
  
  conf.int = TRUE,
  conf.int.style = "ribbon",
  
  surv.median.line = "hv",
  surv.plot.height = 2,
  
  xlab = "Time in days",
  #xscale = 365.25,
  
  legend.title = "Primary lung carcinoma and\nmetastases lung carcinoma survival",
  legend.labs = c("Primary", "metastases"),
  
  surv.scale = "default",
  
  risk.table = TRUE,
  tables.height = 0.2,
  
  ncensor.plot = TRUE,
  ncensor.plot.height = 0.2,
  
  cumevents = TRUE,
  cumevents.height = 0.2,
  
  palette = c("#6E6E6E", "#000000"),
  tables.theme = theme_cleantable(),
  ggtheme = theme_bw()
)

print(ggsurv)
