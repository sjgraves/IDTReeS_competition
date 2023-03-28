# script to create figures for manuscript
# only figures for participant scores for task 2 - classification
# updating in Nov 2022 to add accuracy to metrics figure

library(ggplot2)
library(gridExtra)
library(ggrepel)
library(dplyr)
library(ggtext)
library(caret)

source("scripts/00-functions.R")

# SET UP VISUALIZATION PARAMETERS ------
# colors for genus groups and the order the teams appear in the plots


# hex codes for genera - used for plotting
# only keeping colors for 9 genera
genus_colors <- read.csv("genus_colors_9.csv",stringsAsFactors = F,na.strings = "")
my_colors <- genus_colors$set3.8 # this is used
names(my_colors) <- genus_colors$genus
rm(genus_colors)

# set standard theme for figures
my_theme <-   theme(
  legend.position="bottom",
  strip.background = element_blank(),
  plot.title = element_text(hjust = 0.5,size=14),
  strip.text = element_text(color="black",size=14),
  axis.text.y=element_text(size=12),
  axis.text.x = element_text(vjust = 0.5),
  # Hide panel borders and remove grid lines
  panel.spacing=unit(1, "lines"),
  panel.border=element_rect(colour="black",size=1),
  panel.grid.minor = element_blank(),panel.grid.major = element_blank())

# set order of teams to use in figures based on overall metrics
# team-level metrics
metrics <- read.csv("output_data/table_metrics_allTeams.csv")

# order teams by weighted F1 for no TALL set
# Dec 2022 changing to ordering by accuracy
# no TALL chosen because this is the site group where most teams did best (being easy on them)
wf1 <- metrics[metrics$metric=="accuracy" & metrics$sites=="No TALL",]
teams_ordered <- wf1$team[order(wf1$value,decreasing = T)]
rm(metrics,wf1)

# [1] "Fujitsu"   "CAU"       "Jalapenos" "Treepers"  "baseline" 

#################


# OVERALL METRICS -----
# update Nov 2022 - keeping accuracy
metrics <- read.csv("output_data/table_metrics_allTeams.csv")
metrics$team <- factor(metrics$team,levels=teams_ordered)
metrics$metric <- factor(metrics$metric,levels=c("accuracy","weighted","macro","logloss"))
levels(metrics$metric) <- c("Accuracy","Weighted F1","Macro F1","Cross entropy loss")

# head(metrics)
# team sites      metric value
# 1 baseline   All    Accuracy  0.35
# 2 baseline   All    Macro F1  0.10
# 3 baseline   All Weighted F1  0.29
# 4 baseline   All    Cross entropy loss  9.50
# 5      CAU   All    Accuracy  0.40
# 6      CAU   All    Macro F1  0.22

# separate out metric types because log loss is plotted separately due to lower=better
accMf1 <- metrics[metrics$metric=="Accuracy"|metrics$metric=="Macro F1",]
f1acc <- metrics[metrics$metric!="Cross entropy loss",]
logloss <- metrics[metrics$metric=="Cross entropy loss",]

# TABLE 1 ####
metrics[metrics$sites=="No TALL",]

# team   sites             metric value
# 21  baseline No TALL           Accuracy  0.44
# 22  baseline No TALL           Macro F1  0.13
# 23  baseline No TALL        Weighted F1  0.39
# 24  baseline No TALL Cross entropy loss  7.40
# 25       CAU No TALL           Accuracy  0.52
# 26       CAU No TALL           Macro F1  0.24
# 27       CAU No TALL        Weighted F1  0.50
# 28       CAU No TALL Cross entropy loss  7.00
# 29   Fujitsu No TALL           Accuracy  0.55
# 30   Fujitsu No TALL           Macro F1  0.32
# 31   Fujitsu No TALL        Weighted F1  0.53
# 32   Fujitsu No TALL Cross entropy loss  3.60
# 33 Jalapenos No TALL           Accuracy  0.50
# 34 Jalapenos No TALL           Macro F1  0.14
# 35 Jalapenos No TALL        Weighted F1  0.40
# 36 Jalapenos No TALL Cross entropy loss  2.40
# 37  Treepers No TALL           Accuracy  0.46
# 38  Treepers No TALL           Macro F1  0.09
# 39  Treepers No TALL        Weighted F1  0.43
# 40  Treepers No TALL Cross entropy loss  9.20

# Nov 2022 update
# changing arrangement of panels and legend to accomodate accuracy box
gf1acc <- ggplot(data=accMf1[accMf1$sites!="All",],aes(x=team,y=value,fill=sites)) +
  geom_bar(stat="identity",position="dodge",alpha=0.5) +
  facet_wrap(~metric,ncol=3,labeller = ) +
  geom_text(aes(x=team,y=value,label=value),vjust=-.5,position=position_dodge(width=0.9)) +
  scale_fill_grey(start=0.8,end=0.2,name = "Evaluation site", labels = c("OSBS& & MLBS", "Only TALL")) +
  scale_y_continuous(limits=c(0,0.6)) +
  labs(y="") +
  theme_light() +
  theme(
    legend.position="none",
    strip.background = element_blank(),
    strip.text = element_text(color="black",size=15),
    axis.text.x = element_text(angle=15,vjust=0.75),
    axis.title.x = element_blank(),
    # Hide panel borders and remove grid lines
    panel.spacing=unit(0.2, "lines"),
    panel.border=element_rect(colour="black",size=1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

gll <- ggplot(data=logloss[logloss$sites!="All",],aes(x=team,y=value,fill=sites)) +
  geom_bar(stat="identity",position="dodge",alpha=0.5) +
  facet_wrap(~metric) +
  geom_text(aes(x=team,y=value,label=value),vjust=-.5,position=position_dodge(width=0.9)) +
  scale_fill_grey(start=0.8,end=0.2,name = "Test site", labels = c("Trained (OSBS & MLBS)" , "Untrained (Only TALL)")) +
  scale_y_continuous(trans="reverse") +
  labs(y="") +
  guides(fill = guide_legend(nrow=1)) +
  theme_light() +
  theme(
    legend.position = "right",
    #legend.position="bottom",
    strip.background = element_blank(),
    strip.text = element_text(color="black",size=15),
    axis.text.x = element_text(angle=15,vjust=0.75),
    axis.title.x = element_blank(),
    # Hide panel borders and remove grid lines
    panel.spacing=unit(0.2, "lines"),
    panel.border=element_rect(colour="black",size=1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )


g <- grid.arrange(gf1acc,gll,nrow=2)

ggsave("figures/Figure4.pdf",plot=g,width=8,height=6,units="in")
ggsave("figures/Figure4.png",plot=g,width=8,height=6,units="in")


# F1 SCORE BY TAXONID ----
# list of scientific names, taxonID, and numbers of samples in different training/testing datasets
taxonID_table_full <- read.csv("output_data/table_taxonID_trainEval.csv")

# scientificName taxonID om_train om_eval tall_eval
# 1 Pinus palustris Mill.   PIPA2      302     159        18
# 2      Quercus rubra L.    QURU      169      24        NA
# 3        Acer rubrum L.    ACRU      139      21        13
# 4       Quercus alba L.    QUAL      103       6        17
# 5 Quercus laevis Walter   QULA2       76      35        NA
# 6      Quercus coccinea   QUCO2       53      NA        NA





taxonID_table_full <- read.csv("output_data/table_taxonID_trainEval.csv")
taxonID_metrics <- read.csv("output_data/taxonID_metrics_allTeams.csv")

# remove taxonID that don't have evaluation data
taxonID_remove <- taxonID_table_full$taxonID[is.na(taxonID_table_full$om_eval) & is.na(taxonID_table_full$tall_eval)]
taxonID_table <- taxonID_table_full[!taxonID_table_full$taxonID %in% taxonID_remove,]

# order taxonID by the number in the training data
taxonID_table$taxonID = factor(taxonID_table$taxonID,
                               levels=unique(taxonID_table$taxonID[order(taxonID_table$om_train,decreasing = F)]),ordered=TRUE)



# remove taxonID in the metrics data
taxonID_metrics <- taxonID_metrics[!taxonID_metrics$taxonID %in% taxonID_remove,]
taxonID_metrics$taxonID <- factor(taxonID_metrics$taxonID,
                                  levels=unique(taxonID_table$taxonID[order(taxonID_table$om_train
                                                                                     ,decreasing = F)]),ordered=TRUE)
taxonID_metrics$genus <- as.factor(taxonID_metrics$genus)
taxonID_metrics$team <- factor(taxonID_metrics$team,levels=teams_ordered)

taxonID_metrics$fontface <- NA
taxonID_metrics$fontface[taxonID_metrics$sites=="trained"] <- "plain"
taxonID_metrics$fontface[taxonID_metrics$sites=="untrained"] <- "bold"

# subset, only F1
taxonID_f1 <- taxonID_metrics[taxonID_metrics$metric=="f1.score",]
taxonID_f1$value <- round(taxonID_f1$value,2)

taxonID_f1$label <- taxonID_f1$value
taxonID_f1$label[taxonID_f1$value==0] <- ""


# create y axis label that uses number of samples in om_train
# this way is not the same as the order of the levels because those with the same # are swapped
# makes the ylab axis labels incorrect
# need to match the levels order
#ylab <- paste0(rev(taxonID_table$taxonID)," (",rev(taxonID_table$om_train),")")
ylab <- paste0(levels(rev(taxonID_table$taxonID))," (",rev(taxonID_table$om_train),")")


# F1 by taxa
ggplot(data=taxonID_f1[taxonID_f1$sites!="all",],aes(x=value,y=taxonID)) +
  geom_vline(xintercept = 0,color="gray") +
  geom_point(aes(fill=genus,shape=sites),size=8,color="white") +
  facet_grid(~team) +
  geom_text_repel(aes(label=label,fontface=fontface)) + 
  scale_fill_manual(name = "Genus",na.value="grey", values = my_colors) +
  scale_shape_manual(values=c(21,24),name="Sites",labels=c("Trained (OSBS & MLBS)","**Untrained (TALL)**")) +
  scale_x_continuous(limits=c(-0.10,1.1),breaks=c(0,0.5,1)) +
  scale_y_discrete("Taxonomic ID",labels=ylab) +
  guides(fill = guide_legend(nrow=2,override.aes=list(shape=22)),shape=guide_legend(override.aes = list(color="black"))) +
  guides(shape=guide_legend(nrow=2,override.aes = list(color="black"))) +
  labs(x="F1 score") +
  theme_light() +
  theme(legend.text = element_markdown(),
        legend.position="bottom",
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5,size=14),
        strip.text = element_text(color="black",size=14),
        axis.text.y=element_text(size=12),
        axis.text.x = element_text(vjust = 0.5),
        # Hide panel borders and remove grid lines
        panel.spacing=unit(1, "lines"),
        panel.border=element_rect(colour="black",size=1),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank())

ggsave("figures/Figure7_revision.pdf",width=10,height=8,units="in")

ggsave("figures/Figure7_revision.png",width=10,height=8,units="in")





# AGGREGATED CONFUSION MATRIX -----
type_of_cm <- "aggregated"
cm <- read.csv(paste0("output_data/","confusion_matrix_",type_of_cm,".csv"),stringsAsFactors = T)

taxonID_table_full <- read.csv("output_data/table_taxonID_trainEval.csv")
taxonID_metrics <- read.csv("output_data/taxonID_metrics_allTeams.csv")

# remove taxonID that don't have evaluation data
taxonID_remove <- taxonID_table_full$taxonID[is.na(taxonID_table_full$om_eval) & is.na(taxonID_table_full$tall_eval)]

# drop taxonID
# this is an important line but it is confusing logic
# removing all rows that have the taxonID in either the Prediction or Truth column
cm <- cm[!cm$Prediction %in% taxonID_remove &! cm$Truth %in% taxonID_remove,]
cm$Prediction <- droplevels(cm$Prediction)
cm$Truth <- droplevels(cm$Truth)

# order Truth alphabetically
cm$Truth <- sort_alphabetically(cm$Truth)
cm$Prediction <- sort_alphabetically(cm$Prediction,reverse = F)

# figure out how to add diag as bold
cm$fontface <- "plain"

cm$fontface[cm$Prediction == cm$Truth] <- "bold"

gcm <- ggplot(cm,aes(x=Prediction,y=Truth,fill=TruthGenus,color=PredGenus,alpha=log(Freq)))+
  geom_tile(size=1) +
  geom_text(aes(label = FreqLabels,fontface=fontface),color="black", vjust = .5, alpha = 1) +
  scale_fill_manual(name = "genus",na.value="grey", values = my_colors) +
  scale_color_manual(name = "genus",na.value="grey", values = my_colors) +
  labs(fill = "Genus") +
  guides(color = FALSE, alpha = FALSE,fill=guide_legend(nrow=2)) +
  theme_light() +
  my_theme +
  theme(axis.text.x = element_text(hjust=0.9,vjust = 0.9,angle=45,size=12),
        legend.text = element_text(face="italic"))

ggsave(plot=gcm,filename = "figures/Figure8.png",height=8.5,width=8,units="in")
ggsave(plot=gcm,filename = "figures/Figure8.pdf",height=8.5,width=8,units="in")

# User and producer accuracy (precision and recall)

# subset, precision and recall
taxonID_pr <- taxonID_metrics[taxonID_metrics$metric!="f1.score",]
taxonID_pr$value <- round(taxonID_pr$value,2)

taxonID_pr$label <- taxonID_pr$value
taxonID_pr$label[taxonID_pr$value==0] <- ""




# calculate precison and recall from aggregated confusion matrix
cm_tx <- cm %>% select(Prediction,Truth,Freq)
cm_tx <- pivot_wider(data=cm_tx,names_from=Prediction,values_from = Freq) %>%
  as.data.frame()

cm_tx[is.na(cm_tx)] <- 0

row.names(cm_tx) <- cm_tx$Truth
cm_tx <- cm_tx[,-1]

sum(cm_tx)
cm <- as.matrix(cm_tx)

# calcuate stuff
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

precision = round(diag / colsums,2) 
recall = round(diag / rowsums,2) 
f1 = round(2 * precision * recall / (precision + recall),2)

cm_table <- data.frame(precision, recall, f1) 
cm_table <- arrange(cm_table,row.names(cm_table))

write.csv(cm_table,"output_data/table_taxonID_precison_recall_Fscore.csv",row.names = T)

