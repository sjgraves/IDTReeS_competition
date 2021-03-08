# script to create figures
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(gridExtra)

source("scripts/00-functions.R")
custom_palette10 <- c("#F781BF","#377EB8","#E41A1C","#984EA3","#4DAF4A","#FF7F00","#35978F","#A65628","#FFED6F","#999999")

taxonID_table <- read.csv("output_data/table_taxonID_trainEval.csv")

my_theme <-   theme(
  legend.text = element_text(face = "italic"),
  legend.position="bottom",
  strip.background = element_blank(),
  strip.text = element_text(color="black",size=15),
  axis.text.x = element_text(angle=45,vjust = 0.5),
  # Hide panel borders and remove grid lines
  panel.spacing=unit(1, "lines"),
  panel.border=element_rect(colour="black",size=1),
  panel.grid.minor = element_blank(),panel.grid.major = element_blank())


# OVERALL METRICS -----
metrics <- read.csv("output_data/table_metrics_allTeams.csv")
metrics$team <- factor(metrics$team,levels=c("baseline","Fujitsu","CAU","Jalapenos","Treepers","Gatorsense"))
metrics$metric <- factor(metrics$metric,levels=c("weighted","macro","logloss"))
levels(metrics$metric) <- c("Weighted F1","Macro F1","Log-Loss")


# BASED ON TEAM CONVERSATION - REMOVE ALL FROM FIGURE SINCE THE INTERESTING QUESTION IS ABOUT ON TRAINED VS UNTRAINED SITES
metrics <- metrics[metrics$site!="All",]

# remove gatorsense
metrics <- metrics[metrics$team!= "Gatorsense",]

# just F1
f1 <- metrics[metrics$metric!="Log-Loss",]
logloss <- metrics[metrics$metric=="Log-Loss",]

# plot metrics by sites used, team, and metric
ggplot(data=metrics,aes(x=team,y=value,fill=sites)) +
  geom_bar(stat="identity",position="dodge",alpha=0.5) +
  facet_wrap(~metric,scales="free",ncol=1) +
  #geom_label(aes(x=team,y=value,label=value),vjust=0.5,position=position_dodge(),fill="white") +
  geom_text(aes(x=team,y=value,label=value),vjust=1,position=position_dodge(width=0.9),fontface="bold") +
  scale_fill_grey(start=0.8,end=0.2,name = "Evaluation site", labels = c("OSBS& & MLBS", "Only TALL")) +
  #scale_y_continuous(trans="reverse") +
  theme_light() +
  theme(
    legend.position="bottom",
    strip.background = element_blank(),
    strip.text = element_text(color="black",size=15),
    # Hide panel borders and remove grid lines
    panel.spacing=unit(0.2, "lines"),
    panel.border=element_rect(colour="black",size=1),
    panel.grid.minor = element_blank()
    )

gf1<- ggplot(data=f1,aes(x=team,y=value,fill=sites)) +
  geom_bar(stat="identity",position="dodge",alpha=0.5) +
  facet_wrap(~metric,ncol=1,labeller = ) +
  #geom_label(aes(x=team,y=value,label=value),vjust=0.5,position=position_dodge(),fill="white") +
  geom_text(aes(x=team,y=value,label=value),vjust=-.5,position=position_dodge(width=0.9)) +
  scale_fill_grey(start=0.8,end=0.2,name = "Evaluation site", labels = c("OSBS& & MLBS", "Only TALL")) +
  scale_y_continuous(limits=c(0,0.6)) +
  theme_light() +
  theme(
    legend.position="none",
    strip.background = element_blank(),
    strip.text = element_text(color="black",size=15),
    # Hide panel borders and remove grid lines
    panel.spacing=unit(0.2, "lines"),
    panel.border=element_rect(colour="black",size=1),
    panel.grid.minor = element_blank()
  )

gll <- ggplot(data=logloss,aes(x=team,y=value,fill=sites)) +
  geom_bar(stat="identity",position="dodge",alpha=0.5) +
  facet_wrap(~metric) +
  #geom_label(aes(x=team,y=value,label=value),vjust=0.5,position=position_dodge(),fill="white") +
  geom_text(aes(x=team,y=value,label=value),vjust=-.5,position=position_dodge(width=0.9)) +
  scale_fill_grey(start=0.8,end=0.2,name = "Evaluation site", labels = c("OSBS& & MLBS", "Only TALL")) +
  scale_y_continuous(trans="reverse") +
  theme_light() +
  theme(
    legend.position="bottom",
    strip.background = element_blank(),
    strip.text = element_text(color="black",size=15),
    # Hide panel borders and remove grid lines
    panel.spacing=unit(0.2, "lines"),
    panel.border=element_rect(colour="black",size=1),
    panel.grid.minor = element_blank()
  )

g <- grid.arrange(gf1,gll,nrow=1)

ggsave("figures/barplot_metrics_noGatorsense_multipanel.png",plot=g,width=8,height=6)


# TAXONID ACCURACY METRICS - ALL TAXA -----

# need taxonID table with train eval numbers for sorting levels

taxonID_table$taxonID = factor(taxonID_table$taxonID,
                               levels=unique(taxonID_table$taxonID[order(taxonID_table$om_train,decreasing = F)]),ordered=TRUE)

#taxonID_metrics <- read.csv("output_data/taxonID_metrics_allTeams.csv")
taxonID_metrics <- read.csv("output_data/taxonID_metrics_allTeams_noTall.csv")

taxonID_metrics$taxonID <- factor(taxonID_metrics$taxonID,
                                  levels=unique(taxonID_table$taxonID[order(taxonID_table$om_train
                                                                            ,decreasing = F)]),ordered=TRUE)
#taxonID_metrics$taxonID <- sort_alphabetically(taxonID_metrics$taxonID)
taxonID_metrics$genus <- as.factor(taxonID_metrics$genus)
taxonID_metrics$metric2 <- factor(taxonID_metrics$metric2,levels=c("primary","secondary"))
taxonID_metrics$team <- factor(taxonID_metrics$team,levels=c("Fujitsu","CAU","Jalapenos","Treepers","Gatorsense","baseline"))

# subset, only F1
taxonID_f1 <- taxonID_metrics[taxonID_metrics$metric=="f1.score",]
taxonID_f1$value <- round(taxonID_f1$value,2)

# remove gatorsense
levels(taxonID_f1$team)
taxonID_f1 <- taxonID_f1[taxonID_f1$team!="Gatorsense",]

ggplot(data=taxonID_f1,aes(x=value,y=taxonID)) +
  #geom_point(aes(color=genus),size=5) +
  geom_label(data=taxonID_f1,aes(label=value,fill=genus),fontface="bold",alpha=0.7) +
  facet_grid(~team) +
  #geom_text_repel(data=taxonID_f1,aes(label=value),fontface="bold",direction="x") + 
  #scale_shape_manual(values=c(0,1,2),name="Metric") +
  #scale_alpha_discrete(range=c(1,0.3),guide=FALSE) +
  #scale_fill_manual(values=custom_palette10,na.value="grey",name="Genus") +
  scale_x_continuous(limits=c(-.01,1.0)) +
  scale_fill_manual(values=custom_palette10,na.value="grey",name="Genus") +
  scale_color_manual(values=custom_palette10,na.value="grey",name="Genus") +
  theme_light() +
  labs(x="F1 score") +
  guides(fill = guide_legend(nrow=1,override.aes = aes(label = ""))) +
  my_theme

ggsave("figures/taxonID_metrics_noTall_orderTrain_noGatorsense.png",height=8,width=8)


# TAXONID ACCURACY METRICS - SELECT TAXON ----

# this figure should show how well methods generalize and translate to other sites
# use the summary data that distinguishes between no tall and only tall

selected_taxonID <- c("PIPA2","PITA","LITU","QULA2","Other")
selected_palette <- custom_palette10[c(3,6,7,10)]

noTall <- read.csv("output_data/taxonID_metrics_allTeams_noTall.csv")
onlyTall <- read.csv("output_data/taxonID_metrics_allTeams_onlyTall.csv")

noTall$sites <- "OSBS & MLBS"
onlyTall$sites <- "TALL"

taxonID_metrics <- rbind(noTall,onlyTall)

# subset- only f1 and only for select taxonID
taxonID_metrics <- taxonID_metrics[taxonID_metrics$metric=="f1.score",]
taxonID_metrics$value <- round(taxonID_metrics$value,2)
taxonID_metrics <- taxonID_metrics[taxonID_metrics$taxonID %in% selected_taxonID,]

taxonID_metrics$genus <- as.factor(taxonID_metrics$genus)
taxonID_metrics$metric2 <- factor(taxonID_metrics$metric2,levels=c("primary","secondary"))
taxonID_metrics$team <- factor(taxonID_metrics$team,levels=c("baseline","Fujitsu","CAU","Jalapenos","Treepers","Gatorsense"))

taxonID_metrics$taxonID <- factor(taxonID_metrics$taxonID,
                                  levels=unique(taxonID_table$taxonID[order(taxonID_table$om_train
                                                                            ,decreasing = F)]),ordered=TRUE)
taxonID_metrics$fontface <- NA
taxonID_metrics$fontface[taxonID_metrics$sites=="OSBS & MLBS"] <- "bold"
taxonID_metrics$fontface[taxonID_metrics$sites=="TALL"] <- "italic"


# remove gatorsense
taxonID_metrics <- taxonID_metrics[taxonID_metrics$team!="Gatorsense",]


ggplot(data=taxonID_metrics,aes(x=value,y=taxonID)) +
  #geom_label(data=taxonID_metrics,aes(label=value,fill=genus,fontface=fontface),alpha=0.7) +
  geom_point(aes(fill=genus,shape=sites),size=5,color="black") +
  facet_grid(~team) +
  geom_text_repel(aes(label=value,fontface=fontface),box.padding = 0.5) + 
  #scale_alpha_discrete(range=c(1,0.3),guide=FALSE) +
  scale_fill_manual(values=selected_palette,na.value="grey",name="Genus") +
  scale_shape_manual(values=c(21,24),name="Sites") +
  scale_x_continuous(limits=c(-.01,1.0)) +
  guides(fill = guide_legend(override.aes=list(shape=21))) +
  theme_light() +
  my_theme

ggsave("figures/taxonID_metrics_comparing_sites_selected_taxonID.png",height=6,width=8)



# TAXONID TRAIN EVAL ----
taxonID_table <- read.csv("output_data/table_taxonID_trainEval.csv")

taxonID_table$taxonID = factor(taxonID_table$taxonID,
                               levels=unique(taxonID_table$taxonID[order(taxonID_table$om_train,decreasing = F)]),ordered=TRUE)

taxonID_long <- taxonID_table %>%
  pivot_longer(cols=c(om_train,om_eval,tall_eval)
               ,values_to="count"
               ,names_to="use")

taxonID_long <- as.data.frame(taxonID_long)
taxonID_long$site2 <- "OSBS & MLBS"
taxonID_long$site2[taxonID_long$use=="tall_eval"] <- "TALL"

taxonID_long$fontface <- "plain"
taxonID_long$fontface[taxonID_long$use=="tall_eval"] <- "italic"

# sort
#taxonID_long$taxonID <- sort_alphabetically(taxonID_long$taxonID)
taxonID_long$use <- factor(taxonID_long$use,levels=c("om_train","om_eval","tall_eval"))


ggplot(data=taxonID_long,aes(y=taxonID,x=count)) +
  geom_point(stat="identity",aes(fill=use,shape=site2),size=5,color="white",alpha=0.7) +
  geom_text_repel(aes(label=count,fill=use,fontface=fontface),direction="x",label.padding=0.5) + 
  scale_shape_manual(values=c(21,24),name="Sites") +
  scale_fill_brewer(palette="Set2",name = "Data group",labels = c("OSBS- & MLBS train", "OSBS-MLBS eval.", "TALL eval.")) +
  guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 24))),shape=F) +
  theme_bw() +
  xlab("Number of samples") +
  theme(legend.position = c(0.8,0.1))

ggsave("figures/taxonID_groups_orderByTrain.png",height=8,width=6)

# CONFUSION MATRIX -------------------

# must run the confusion matrix file first
type_of_cm <- "team"

cm <- read.csv(paste0("output_data/","confusion_matrix_",type_of_cm,".csv"),stringsAsFactors = T)


# sort levels for plotting
levels_sort <- "alphabetically"

if(levels_sort=="alphabetically"){
  
  print("sorting alphabetically")
  
  levels_noOther <- levels(cm$Prediction)[!levels(cm$Prediction)=="Other"]
  
  cm$Prediction <- factor(cm$Prediction,levels=sort(levels(cm$Prediction)))
  cm$Truth <- factor(cm$Truth,levels=sort(levels(cm$Truth),decreasing=T))
  
  # now move other to end
  cm$Prediction <- factor(cm$Prediction,levels=c(levels_noOther,"Other"))
  cm$Truth <- factor(cm$Truth,levels=c("Other",sort(levels_noOther,decreasing = T)))
  
} else {
  
  # sort by frequency
  freq_of_Pred <- cm %>%
    group_by(Prediction) %>%
    summarize(total=sum(Freq))
  
  levels(freq_of_Pred$Prediction)
  
  freq_of_Pred$Prediction <- reorder(freq_of_Pred$Prediction,freq_of_Pred$total)
  
  cm$Prediction <- factor(cm$Prediction,levels=levels(freq_of_Pred$Prediction))
  cm$Truth <- factor(cm$Truth,levels=levels(freq_of_Pred$Prediction))
  
}

# AGGREGATED CONFUSION MATRIX -----
type_of_cm <- "aggregated"
cm <- read.csv("output_data/confusion_matrix_noGatorsenseaggregated.csv")

# order Truth alphabetically
cm$Truth <- sort_alphabetically(cm$Truth)
cm$Prediction <- sort_alphabetically(cm$Prediction,reverse = F)

# figure out how to add diag as bold
cm$fontface <- "plain"

cm$fontface[cm$Prediction == cm$Truth] <- "bold"

ggplot(cm,aes(x=Prediction,y=Truth,fill=TruthGenus,color=PredGenus,alpha=log(Freq)))+
  geom_tile(size=1) +
  geom_text(aes(label = FreqLabels,fontface=fontface),color="black", vjust = .5, alpha = 1) +
  scale_fill_manual(values=custom_palette10,na.value="grey") +
  scale_color_manual(values=custom_palette10,na.value="grey") +
  labs(fill = "Genus") +
  guides(color = FALSE, alpha = FALSE,fill=guide_legend(nrow=1)) +
  theme_light() +
  my_theme

ggsave(filename = "figures/cm_aggregated_noGatorsense.png",height=8.5,width=8)

# CONFUSION MATRIX BY TEAM -----
ggplot(cm,aes(x=Prediction,y=Truth,fill=TruthGenus,color=PredGenus,alpha=log(Freq)))+
  geom_tile(color="grey") +
  geom_tile(size=1) +
  geom_text(aes(label = FreqLabels),color="black", vjust = .5, alpha = 1) +
  scale_fill_manual(values=custom_palette10,na.value="grey") +
  scale_color_manual(values=custom_palette10,na.value="grey") +
  labs(fill = "Genus") +
  guides(color = FALSE, alpha = FALSE) +
  theme_light() +
  facet_wrap(~Team,nrow=3) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color="black",size=15),
    axis.text.x = element_text(angle=90),
    # Hide panel borders and remove grid lines
    panel.spacing=unit(1, "lines"),
    panel.border=element_rect(colour="black",size=1),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    )

ggsave(filename = "figures/cm_byTeam.png",height=14,width=10)




