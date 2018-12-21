library(ggplot2)
library(reshape2)
library(dplyr)
library(ggpubr) #only used to show multiple plots on one page.


## REMOVE LATER, TEMPORARY
cat("\014") #clear console
rm(list=ls()) #clear memory
## REMOVE ABOVE

if(!exists("comb")){
  cat("No comb object found in memory: running prepare.r script!\n")
  source("clean.r")
}

ptv <- comb[, c("Anoniem.nummer", "V5.PTV", "V10.PTV", "V15.PTV", "V20.PTV", "V25.PTV", "V30.PTV", "V35.PTV", "V40.PTV", "V45.PTV", "V50.PTV", "V55.PTV", "V60.PTV", "V65.PTV", "V70.PTV", "V75.PTV")]
ptv <- ptv %>% rename("V5" = "V5.PTV" , "V10" = "V10.PTV", "V15" = "V15.PTV", "V20" = "V20.PTV", "v25" = "V25.PTV",  "V30" = "V30.PTV", "v35" = "V35.PTV", "v40" = "V40.PTV", "v45" = "V45.PTV", "v50" = "V50.PTV","v55" = "V55.PTV","v60" = "V60.PTV","v65" = "V65.PTV","v70" = "V70.PTV","v75" = "V75.PTV")

ptv[] <- lapply(ptv, function(x) as.numeric(as.character(x)))
ptv <- ptv[which(ptv$V5 > 0),]
ptv <- ptv %>% mutate_if(is.numeric, round, 0)
melted = melt(ptv, id.vars="Anoniem.nummer")

ptv_line <- ggplot(data=melted, aes(x=variable, y=value, group=Anoniem.nummer)) + geom_line() +
  labs(title = "PTV", x = "Dosage", y = "Percentage", color = "Legend Title\n") +
  theme(axis.text.x = element_text(colour="grey20",size=8,angle=90,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"))
ptv_line




Hart_en_AortaAsc <- comb[, c("Anoniem.nummer", "V5.Hart_en_AortaAsc", "V10.Hart_en_AortaAsc", "V15.Hart_en_AortaAsc", "V20.Hart_en_AortaAsc", "V25.Hart_en_AortaAsc", "V30.Hart_en_AortaAsc", "V35.Hart_en_AortaAsc", "V40.Hart_en_AortaAsc", "V45.Hart_en_AortaAsc", "V50.Hart_en_AortaAsc", "V55.Hart_en_AortaAsc", "V60.Hart_en_AortaAsc", "V65.Hart_en_AortaAsc", "V70.Hart_en_AortaAsc", "V75.Hart_en_AortaAsc")]
Hart_en_AortaAsc <- Hart_en_AortaAsc %>% rename("V5" = "V5.Hart_en_AortaAsc" , "V10" = "V10.Hart_en_AortaAsc", "V15" = "V15.Hart_en_AortaAsc", "V20" = "V20.Hart_en_AortaAsc", "v25" = "V25.Hart_en_AortaAsc",  "V30" = "V30.Hart_en_AortaAsc", "v35" = "V35.Hart_en_AortaAsc", "v40" = "V40.Hart_en_AortaAsc", "v45" = "V45.Hart_en_AortaAsc", "v50" = "V50.Hart_en_AortaAsc","v55" = "V55.Hart_en_AortaAsc","v60" = "V60.Hart_en_AortaAsc","v65" = "V65.Hart_en_AortaAsc","v70" = "V70.Hart_en_AortaAsc","v75" = "V75.Hart_en_AortaAsc")

Hart_en_AortaAsc[] <- lapply(Hart_en_AortaAsc, function(x) as.numeric(as.character(x)))
Hart_en_AortaAsc <- Hart_en_AortaAsc %>% mutate_if(is.numeric, round, 0)
melted = melt(Hart_en_AortaAsc, id.vars="Anoniem.nummer")

Hart_en_AortaAsc_line <- ggplot(data=melted, aes(x=variable, y=value, group=Anoniem.nummer)) + geom_line() +
  labs(title = "Heart and aorta", x = "Dosage", y = "Percentage", color = "Legend Title\n") +
  theme(axis.text.x = element_text(colour="grey20",size=8,angle=90,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"))
Hart_en_AortaAsc_line




Longen <- comb[, c("Anoniem.nummer", "V5.Longen", "V10.Longen", "V15.Longen", "V20.Longen", "V25.Longen", "V30.Longen", "V35.Longen", "V40.Longen", "V45.Longen", "V50.Longen", "V55.Longen", "V60.Longen", "V65.Longen", "V70.Longen", "V75.Longen")]
Longen <- Longen %>% rename("V5" = "V5.Longen" , "V10" = "V10.Longen", "V15" = "V15.Longen", "V20" = "V20.Longen", "v25" = "V25.Longen",  "V30" = "V30.Longen", "v35" = "V35.Longen", "v40" = "V40.Longen", "v45" = "V45.Longen", "v50" = "V50.Longen","v55" = "V55.Longen","v60" = "V60.Longen","v65" = "V65.Longen","v70" = "V70.Longen","v75" = "V75.Longen")

Longen[] <- lapply(Longen, function(x) as.numeric(as.character(x)))
Longen <- Longen %>% mutate_if(is.numeric, round, 0)
melted = melt(Longen, id.vars="Anoniem.nummer")

Longen_line <- ggplot(data=melted, aes(x=variable, y=value, group=Anoniem.nummer)) + geom_line() +
  labs(title = "Lungs", x = "Dosage", y = "Percentage", color = "Legend Title\n") +
  theme(axis.text.x = element_text(colour="grey20",size=8,angle=90,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"))
Longen_line




Oes <- comb[, c("Anoniem.nummer", "V5.Oes", "V10.Oes", "V15.Oes", "V20.Oes", "V25.Oes", "V30.Oes", "V35.Oes", "V40.Oes", "V45.Oes", "V50.Oes", "V55.Oes", "V60.Oes", "V65.Oes", "V70.Oes", "V75.Oes")]
Oes <- Oes %>% rename("V5" = "V5.Oes" , "V10" = "V10.Oes", "V15" = "V15.Oes", "V20" = "V20.Oes", "v25" = "V25.Oes",  "V30" = "V30.Oes", "v35" = "V35.Oes", "v40" = "V40.Oes", "v45" = "V45.Oes", "v50" = "V50.Oes","v55" = "V55.Oes","v60" = "V60.Oes","v65" = "V65.Oes","v70" = "V70.Oes","v75" = "V75.Oes")

Oes[] <- lapply(Oes, function(x) as.numeric(as.character(x)))
Oes <- Oes %>% mutate_if(is.numeric, round, 0)
melted = melt(Oes, id.vars="Anoniem.nummer")

Oes_line <- ggplot(data=melted, aes(x=variable, y=value, group=Anoniem.nummer)) + geom_line() +
  labs(title = "Oesophagus", x = "Dosage", y = "Percentage", color = "Legend Title\n") +
  theme(axis.text.x = element_text(colour="grey20",size=8,angle=90,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"))
Oes_line



multiplot <- ggarrange(ptv_line, Hart_en_AortaAsc_line, Longen_line, Oes_line, ncol = 2, nrow = 2)
#print(multiplot)



annotate_figure(multiplot,
                top = text_grob("Dosimetric parameters",face = "bold", size = 14),
                bottom = text_grob("Percentage of the contour volume (y axis) receiving specified dosage (x axis)", hjust = 1, x = 1, face = "italic", size = 10)
)

