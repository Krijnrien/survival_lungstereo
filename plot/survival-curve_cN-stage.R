library(survival)
library(survminer)
library(dplyr)

## REMOVE LATER, TEMPORARY
cat("\014") #clear console
rm(list=ls()) #clear memory
## REMOVE ABOVE

if(!exists("comb")){
  cat("No comb objecN found in memory: running prepare.r script!\n")
  source("clean.r")
}

comb <- comb[which(comb$cN == 1 | comb$cN == 2 | comb$cN == 3),]

fit <- survfit(Surv(comb$diff_in_days, comb$survivalstat) ~ cN, data = comb)

ggsurv <- ggsurvplot(
  fit, 
  comba = comb,
  
  pval = TRUE,
  pval.size = 4,
  pval.method = TRUE,
  pval.method.size = 3,
  log.rank.weights = "1",
  
  #conf.int = TRUE,
  #conf.int.style = "ribbon",
  
  surv.median.line = "hv",
  surv.plot.height = 2,
  
  xlab = "Time in days",
  #xscale = 365.25,
  
  legend.title = "tnm",
  legend.labs = c("n1", "n2", "n3"),
  
  surv.scale = "default",
  #censor.shape = "|",
  
  risk.table = TRUE,
  tables.height = 0.2,
  
  ncensor.plot = TRUE,
  ncensor.plot.height = 0.2,
  
  cumevents = TRUE,
  cumevents.height = 0.2,
  
  tables.theme = theme_cleantable(),
  ggtheme = theme_bw()
)

print(ggsurv)

