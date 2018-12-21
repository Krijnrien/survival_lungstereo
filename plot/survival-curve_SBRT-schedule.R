library(survival)
library(survminer)
library(dplyr)

cat("\014") # clear console
rm(list=ls()) # clear memory
graphics.off() # clear plots

if(!exists("data")){
  cat("No data object found in memory: running prepare.r script!\n")
  source("clean.r")
}

data <- data[which(data$Schedule != 4),]
#data_surv <- Surv(time = data$diff_in_days, event = data$survivalstat)


data_365 <- data
data_365$survivalstat <- ifelse(data_365$diff_in_days > 365, 0, data_365$survivalstat)
data_365$diff_in_days <- ifelse(data_365$diff_in_days > 365, 365, data_365$diff_in_days)

sdf <- survdiff(Surv(data_365$diff_in_days, data_365$survivalstat) ~ Schedule,data = data_365)
p.val <- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)
pvalue_365 <- format(round(p.val, 2), nsmall = 2)



data_730 <- data
data_730$survivalstat <- ifelse(data_730$diff_in_days > 730, 0, data_730$survivalstat)
data_730$diff_in_days <- ifelse(data_730$diff_in_days > 730, 730, data_730$diff_in_days)


sdf <- survdiff(Surv(data_730$diff_in_days, data_730$survivalstat) ~ Schedule,data = data_730)
p.val <- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)
pvalue_730 <- format(round(p.val, 2), nsmall = 2)


wilcoxon <- survdiff(Surv(data$diff_in_days, data$survivalstat) ~ Schedule,data = data, rho=1)
p.val <- 1 - pchisq(wilcoxon$chisq, length(wilcoxon$n) - 1)
pvalue_wilcoxon <- format(round(p.val, 2), nsmall = 2)


fit <- survfit(Surv(data$diff_in_days, data$survivalstat) ~ Schedule, data = data)


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
  #Longstereo_primair-PA-survival_KvdBurg_20181122
  legend.title = "Lung carcinoma SBRT\nfractioning schedules survival",
  legend.labs = c("3 x 18Gy", "5 x 11Gy", "8 x 7,5Gy"),
  
  surv.scale = "default",
  
  risk.table = TRUE,
  tables.height = 0.2,
  
  ncensor.plot = TRUE,
  ncensor.plot.height = 0.2,
  
  cumevents = TRUE,
  cumevents.height = 0.2,
  
  palette = c("#6E6E6E", "#666666", "#000000"),
  #palette = c("grey"),
  tables.theme = theme_cleantable(),
  ggtheme = theme_bw()
)

ggsurv$plot <- ggsurv$plot + 
  #geom_text(aes(x = 120,y=.6,label = paste0("wilcoxon = ", pvalue_wilcoxon))) +
  geom_vline(aes(xintercept=365))+ geom_text(size=3, aes(x = 480,y=.2,label = paste0("p = ", pvalue_365))) +
  geom_vline(aes(xintercept=730))+ geom_text(size=3, aes(x = 850,y=.2,label = paste0("p = ", pvalue_730)))

print(ggsurv)



glist <- list(
  ggsurvplot(fit, fun = "event", main = "Cumulative proportion", palette = c("grey")),
  ggsurvplot(fit, fun = "cumhaz",  main = "Cumulative Hazard", palette = c("grey"))
  #ggsurvplot(fit, fun = "cloglog", main = "Complementary log−log")
)
arrange_ggsurvplots(glist, ncol = 1, nrow = 2)

