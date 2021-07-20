# script to create figures for manuscript
# only figures for participant scores for task 2 - classification

library(ggplot2)
library(gridExtra)
library(ggrepel)
library(dplyr)
library(ggtext)

source("scripts/00-functions.R")

taxonID_table_full <- read.csv("output_data/table_taxonID_trainEval.csv")
genus_colors <- read.csv("genus_colors_9.csv",stringsAsFactors = F,na.strings = "")

my_colors <- genus_colors$set3.8 # this is used
names(my_colors) <- genus_colors$genus

# set order of teams to use in figures based on overall metrics
metrics <- read.csv("output_data/table_metrics_allTeams.csv")

# order teams by weighted F1 for no TALL set
wf1 <- metrics[metrics$metric=="weighted" & metrics$sites=="No TALL",]
teams_ordered <- wf1$team[order(wf1$value,decreasing = T)]

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


# OVERALL METRICS -----
metrics$team <- factor(metrics$team,levels=teams_ordered)
metrics$metric <- factor(metrics$metric,levels=c("weighted","macro","logloss"))
levels(metrics$metric) <- c("Weighted F1","Macro F1","Log-Loss")

# just F1
f1 <- metrics[metrics$metric!="Log-Loss",]
logloss <- metrics[metrics$metric=="Log-Loss",]


gf1<- ggplot(data=f1[f1$sites!="All",],aes(x=team,y=value,fill=sites)) +
  geom_bar(stat="identity",position="dodge",alpha=0.5) +
  facet_wrap(~metric,ncol=1,labeller = ) +
  geom_text(aes(x=team,y=value,label=value),vjust=-.5,position=position_dodge(width=0.9)) +
  scale_fill_grey(start=0.8,end=0.2,name = "Evaluation site", labels = c("OSBS& & MLBS", "Only TALL")) +
  scale_y_continuous(limits=c(0,0.6)) +
  labs(y="") +
  theme_light() +
  theme(
    legend.position="none",
    strip.background = element_blank(),
    strip.text = element_text(color="black",size=15),
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
  guides(fill = guide_legend(nrow=2)) +
  theme_light() +
  theme(
    legend.position="bottom",
    strip.background = element_blank(),
    strip.text = element_text(color="black",size=15),
    # Hide panel borders and remove grid lines
    panel.spacing=unit(0.2, "lines"),
    panel.border=element_rect(colour="black",size=1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

g <- grid.arrange(gf1,gll,nrow=1)

ggsave("figures/figure6.pdf",plot=g,width=8,height=6,units="in")

# F1 SCORE BY TAXONID ----
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
ylab <- paste0(rev(taxonID_table$taxonID)," (",rev(taxonID_table$om_train),")")


# potential new figure
ggplot(data=taxonID_f1[taxonID_f1$sites!="all",],aes(x=value,y=taxonID)) +
  geom_vline(xintercept = 0,color="gray") +
  geom_point(aes(fill=genus,shape=sites),size=8,color="white") +
  facet_grid(~team) +
  geom_text_repel(aes(label=label,fontface=fontface)) + 
  scale_fill_manual(name = "Genus",na.value="grey", values = my_colors) +
  scale_shape_manual(values=c(21,24),name="Sites",labels=c("Trained (OSBS & MLBS)","**Untrained (TALL)**")) +
  scale_x_continuous(limits=c(-0.10,1.1),breaks=c(0,0.5,1)) +
  scale_y_discrete(labels=ylab) +
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

ggsave("figures/Figure7.pdf",width=10,height=8,units="in")

#ggsave("figures/Figure7.png",width=10,height=8,units="in")





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

ggplot(cm,aes(x=Prediction,y=Truth,fill=TruthGenus,color=PredGenus,alpha=log(Freq)))+
  geom_tile(size=1) +
  geom_text(aes(label = FreqLabels,fontface=fontface),color="black", vjust = .5, alpha = 1) +
  scale_fill_manual(name = "genus",na.value="grey", values = my_colors) +
  scale_color_manual(name = "genus",na.value="grey", values = my_colors) +
  labs(fill = "Genus") +
  guides(color = FALSE, alpha = FALSE,fill=guide_legend(nrow=2)) +
  theme_light() +
  my_theme +
  theme(axis.text.x = element_text(hjust=0.9,vjust = 0.9,angle=45,size=12),)

ggsave(filename = "figures/Figure8.png",height=8.5,width=8,units="in")
ggsave(filename = "figures/Figure8.pdf",height=8.5,width=8,units="in")

