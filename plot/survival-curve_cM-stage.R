library(survival)
library(survminer)
library(dplyr)

## REMOVE LATER, TEMPORARY
cat("\014") #clear console
rm(list=ls()) #clear memory
## REMOVE ABOVE

source("clean_data.r")

data <- data[which(data$cM == 0 | data$cM == 1),]

fit <- survfit(Surv(data$diff_in_days, data$survivalstat) ~ cM, data = data)

ggsurv <- ggsurvplot(
  fit, 
  data = data,
  
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
  legend.labs = c("m0", "m1"),
  
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

