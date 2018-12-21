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

comb <- comb[which(comb$origin == 1),]
#comb$PA[is.na(comb$PA)] <- 9

comb["PAstat"] <- NA

# 1 adenoca, 2 squamous cell carcinoma, 3 carconima NOS, 4 sarcoma ,5 BAC, 6 other malignancy, 9 no PA/ proved malignancy
comb$PAstat <- ifelse(comb$PA == 1 | comb$PA == 2 | comb$PA == 3 | comb$PA == 4 | comb$PA == 5 | comb$PA == 6, 1, 2)


comb$PA <- factor(comb$PA, levels = c("1", "2"), labels = c("PA", "No PA"))

# Fit survival comba using the Kaplan-Meier method
#comb_surv <- Surv(time = comb$diff_in_days, event = comb$survivalstat)

#fit1 <- survfit(surv_object ~ Schedule, type='kaplan-meier', conf.type='none')
#comb_survfit <- survfit(comb_surv ~ survivalstat, comba = comb)
fit <- survfit(Surv(comb$diff_in_days, comb$survivalstat) ~ PAstat, data = comb)



ggsurv <- ggsurvplot(
  fit, 
  comba = comb,
  
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
  
  legend.title = "Primary lung carcinoom pathology patients \ntumor (PA) proven and unproven survival",
  legend.labs = c("PA proven", "No PA proven"),
  
  surv.scale = "default",
  #censor.shape = "|",
  
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

