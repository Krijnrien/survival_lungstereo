library(survival)
library(survminer)
library(dplyr)

cat("\014") # clear console
rm(list=ls()) # clear memory
graphics.off() # clear plots

source("clean_data.r")


data$WHO <- ifelse(data$WHO == 3 , 2, data$WHO)
data$WHO <- factor(data$WHO, levels = c("0", "1", "2"), labels = c("asymptomatic", "symptomatic but completely ambulatory", "symptomatic")) # 0 asymptomatic, 1 symptomatic but completely ambulatory, 2 symptomatic, 3 symptomatic, 4 bedbound, 5 death



data_365 <- data
data_365$survivalstat <- ifelse(data_365$diff_in_days > 450, 0, data_365$survivalstat)
data_365$diff_in_days <- ifelse(data_365$diff_in_days > 450, 450, data_365$diff_in_days)

sdf <- survdiff(Surv(data_365$diff_in_days, data_365$survivalstat) ~ Schedule,data = data_365)
p.val <- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)
pvalue_365 <- format(round(p.val, 2), nsmall = 2)





fit <- survfit(Surv(data$diff_in_days, data$survivalstat) ~ WHO, data = data)


ggsurv <- ggsurvplot(
  fit, 
  data = data,
  
  pval = TRUE,
  pval.size = 4,
  pval.method = TRUE,
  pval.method.size = 3,
  log.rank.weights = "1",
  test.for.trend = TRUE,
  
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
  
 # palette = c("#6E6E6E", "#666666", "#000000"),
  #palette = c("grey"),
  tables.theme = theme_cleantable(),
  ggtheme = theme_bw()
)
print(ggsurv)
