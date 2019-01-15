library(survival)
library(survminer)
library(dplyr)

cat("\014") # clear console
rm(list=ls()) # clear memory
graphics.off() # clear plots

source("clean_data.r")

data$sex <- factor(data$sex, levels = c("1", "2"), labels = c("male", "female"))


fit <- survfit(Surv(data$diff_in_days, data$survivalstat) ~ sex, data = data)


ggsurv <- ggsurvplot(
  fit, 
  data = data,
  
  pval = TRUE,
  pval.size = 4,
  pval.method = TRUE,
  pval.method.size = 3,
  log.rank.weights = "1",
  
  conf.int = TRUE,
  conf.int.style = "ribbon",
  conf.int.alpha = 0.2,
  
  surv.median.line = "hv",
  surv.plot.height = 2,
  
  xlab = "Time in days",
  #xscale = 365.25,
  surv.scale = "default",
  
  risk.table = TRUE,
  tables.height = 0.2,
  
  ncensor.plot = TRUE,
  ncensor.plot.height = 0.2,
  
  cumevents = TRUE,
  cumevents.height = 0.2,
  
  #palette = c("#6E6E6E", "#666666", "#000000"),
  #palette = c("grey"),
  tables.theme = theme_cleantable(),
  ggtheme = theme_bw()
)
print(ggsurv)
