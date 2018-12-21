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

comb <- comb[which(comb$cT == 1 | comb$cT == 2 | comb$cT == 3 | comb$cT == 4),]

fit <- survfit(Surv(comb$diff_in_days, comb$survivalstat) ~ cT, data = comb)

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
  
  legend.title = "tnm",
  legend.labs = c("t1", "t2", "t3", "t4"),
  
  surv.scale = "default",
  #censor.shape = "|",
  
  risk.table = TRUE,
  tables.height = 0.2,
  
  ncensor.plot = TRUE,
  ncensor.plot.height = 0.2,
  
  cumevents = TRUE,
  cumevents.height = 0.2,
  
  #palette = c("#6E6E6E", "#000000"),
  tables.theme = theme_cleantable(),
  ggtheme = theme_bw()
)

print(ggsurv)

