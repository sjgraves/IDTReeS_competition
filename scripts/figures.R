# SCRIPT TO CREATE MANUSCRIPT FIGURES -----
library(dplyr)
library(plyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(raster)
library(RStoolbox)
library(ggtext)
library(sf)

source("scripts/00-functions.R")

taxonID_table <- read.csv("output_data/table_taxonID_trainEval.csv")
genus_colors <- read.csv("genus_colors_9.csv",stringsAsFactors = F,na.strings = "")

my_colors <- genus_colors$set3.8
names(my_colors) <- genus_colors$genus

# taxonIDs to remove from figures - no evaluation data
taxonID_remove <- taxonID_table$taxonID[is.na(taxonID_table$om_eval) & is.na(taxonID_table$tall_eval)]
taxonID_table <- taxonID_table[!taxonID_table$taxonID %in% taxonID_remove,]
taxonID_table$taxonID = factor(taxonID_table$taxonID,
                               levels=unique(taxonID_table$taxonID[order(taxonID_table$om_train,decreasing = F)]),ordered=TRUE)

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
  labs(y="") +
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
  scale_fill_grey(start=0.8,end=0.2,name = "Test site", labels = c("OSBS & MLBS", "Only TALL")) +
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
    panel.grid.minor = element_blank()
  )

g <- grid.arrange(gf1,gll,nrow=1)

ggsave("figures/barplot_metrics_noGatorsense_multipanel.png",plot=g,width=8,height=6)


# TAXONID ACCURACY METRICS - ALL TAXA -----

#taxonID_metrics <- read.csv("output_data/taxonID_metrics_allTeams.csv")
taxonID_metrics <- read.csv("output_data/taxonID_metrics_noGatorsense_noTall.csv")

# remove taxonID
taxonID_metrics <- taxonID_metrics[!taxonID_metrics$taxonID %in% taxonID_remove,]
taxonID_metrics$taxonID <- factor(taxonID_metrics$taxonID,
                                  levels=unique(taxonID_table$taxonID[order(taxonID_table$om_train
                                                                            ,decreasing = F)]),ordered=TRUE)
taxonID_metrics$genus <- as.factor(taxonID_metrics$genus)
taxonID_metrics$team <- factor(taxonID_metrics$team,levels=c("Fujitsu","CAU","Jalapenos","Treepers","baseline"))

# subset, only F1
taxonID_f1 <- taxonID_metrics[taxonID_metrics$metric=="f1.score",]
taxonID_f1$value <- round(taxonID_f1$value,2)

# remove QUMO4, not sure why this is still in the dataset
taxonID_f1 <- taxonID_f1[!taxonID_f1$taxonID=="QUMO4",]

# join genus colors
#taxonID_f1 <- left_join(taxonID_f1,genus_colors)

tmp <- taxonID_table[!is.na(taxonID_table$om_eval),]
ylab <- paste0(rev(tmp$taxonID)," (",rev(tmp$om_train),")")

gall <- ggplot(data=taxonID_f1,aes(x=value,y=taxonID)) +
  geom_point(aes(fill=genus),color="white",size=8,shape=21) +
  #geom_label(data=taxonID_f1,aes(label=value,fill=genus),fontface="bold",alpha=0.7) +
  geom_text(aes(label=value),fontface="plain") +
  facet_grid(~team) +
  #geom_text_repel(data=taxonID_f1,aes(label=value),fontface="bold",direction="x") + 
  #scale_shape_manual(values=c(0,1,2),name="Metric") +
  #scale_alpha_discrete(range=c(1,0.3),guide=FALSE) +
  scale_x_continuous(limits=c(-.05,1.0),breaks=c(0,0.5,1)) +
  scale_y_discrete(labels=ylab) +
  #scale_fill_manual(values=fig_colors,na.value="grey",name="Genus") +
  #scale_color_manual(values=fig_colors,na.value="grey",name="Genus") +
  scale_fill_manual(name = "genus",na.value="grey", values = my_colors) +
  theme_light() +
  #labs(x="F1 score") +
  guides(fill = guide_legend(nrow=1,override.aes = aes(label = ""),title="Genus")) +
  my_theme +
  theme(legend.position = "top",axis.title.x = element_blank())

ggsave("figures/taxonID_metrics_noTall_orderTrain_noGatorsense_reformat.png",height=6,width=8,units="in")


# TAXONID ACCURACY METRICS - SELECT TAXON ----

# this figure should show how well methods generalize and translate to other sites
# use the summary data that distinguishes between no tall and only tall

selected_taxonID <- c("PIPA2","PITA","LITU","QULA2","ACRU","Other")

noTall <- read.csv("output_data/taxonID_metrics_noGatorsense_noTall.csv")
onlyTall <- read.csv("output_data/taxonID_metrics_noGatorsense_onlyTall.csv")

noTall$sites <- "OSBS & MLBS"
onlyTall$sites <- "TALL"

taxonID_metrics <- rbind(noTall,onlyTall)

# subset- only f1 and only for select taxonID
taxonID_metrics <- taxonID_metrics[taxonID_metrics$metric=="f1.score",]
taxonID_metrics$value <- round(taxonID_metrics$value,2)
taxonID_metrics <- taxonID_metrics[taxonID_metrics$taxonID %in% selected_taxonID,]

taxonID_metrics$genus <- as.factor(taxonID_metrics$genus)
taxonID_metrics$metric2 <- factor(taxonID_metrics$metric2,levels=c("primary","secondary"))
taxonID_metrics$team <- factor(taxonID_metrics$team,levels=c("Fujitsu","CAU","Jalapenos","Treepers","Gatorsense","baseline"))

taxonID_metrics$taxonID <- factor(taxonID_metrics$taxonID,
                                  levels=unique(taxonID_table$taxonID[order(taxonID_table$om_train
                                                                            ,decreasing = F)]),ordered=TRUE)
taxonID_metrics$fontface <- NA
taxonID_metrics$fontface[taxonID_metrics$sites=="OSBS & MLBS"] <- "plain"
taxonID_metrics$fontface[taxonID_metrics$sites=="TALL"] <- "bold"

tmp_select <- taxonID_table[taxonID_table$taxonID %in% selected_taxonID,]
ylab_select <- paste0(rev(tmp_select$taxonID)," (",rev(tmp_select$om_train),")")

gselect <- ggplot(data=taxonID_metrics,aes(x=value,y=taxonID)) +
  #geom_label(data=taxonID_metrics,aes(label=value,fill=genus,fontface=fontface),alpha=0.7) +
  geom_point(aes(fill=genus,shape=sites),size=8,color="white") +
  facet_grid(~team) +
  geom_text_repel(aes(label=value,fontface=fontface),box.padding = 0.5) + 
  scale_fill_manual(name = "genus",na.value="grey", values = my_colors,guide=F) +
  scale_shape_manual(values=c(21,24),name="Sites",labels=c("OSBS & MLBS","**TALL**")) +
  scale_x_continuous(limits=c(-.05,1.0),breaks=c(0,0.5,1)) +
  scale_y_discrete(labels=ylab_select) +
  #guides(fill = guide_legend(nrow=1,override.aes=list(shape=21)),shape=guide_legend(override.aes = list(color="black"))) +
  guides(shape=guide_legend(override.aes = list(color="black"))) +
  labs(x="F1 score") +
  theme_light() +
  my_theme +
  theme(legend.text = element_markdown(),strip.text = element_blank())

ggsave("figures/taxonID_metrics_comparing_sites_selected_taxonID_revised.png",height=6,width=8)

# COMBINE FOR FIGURE 7 -----
gcombo <- grid.arrange(gall,gselect,nrow=2,heights=c(2,1.5))
ggsave("figures/figure7.png",plot=gcombo,units="in",height=8,width=8)



# TAXONID TRAIN EVAL ----
taxonID_table <- read.csv("output_data/table_taxonID_trainEval.csv")

taxonID_table$taxonID = factor(taxonID_table$taxonID,
                               levels=unique(taxonID_table$taxonID[order(taxonID_table$om_train,decreasing = TRUE)]),ordered=TRUE)

taxonID_long <- taxonID_table %>%
  pivot_longer(cols=c(om_train,om_eval,tall_eval)
               ,values_to="count"
               ,names_to="use")

taxonID_long <- as.data.frame(taxonID_long)
taxonID_long$site2 <- "OSBS & MLBS"
taxonID_long$site2[taxonID_long$use=="tall_eval"] <- "TALL"

taxonID_long$fontface <- "plain"
taxonID_long$fontface[taxonID_long$use=="om_eval"] <- "italic"
taxonID_long$fontface[taxonID_long$use=="tall_eval"] <- "bold"

# sort
#taxonID_long$taxonID <- sort_alphabetically(taxonID_long$taxonID)
taxonID_long$use <- factor(taxonID_long$use,levels=c("om_train","om_eval","tall_eval"))

# ggplot(data=taxonID_long,aes(y=taxonID,x=count)) +
#   geom_point(stat="identity",aes(fill=use,shape=site2),size=5,color="white",alpha=0.7) +
#   geom_text_repel(aes(label=count,fill=use,fontface=fontface),direction="x",label.padding=0.5) + 
#   scale_shape_manual(values=c(21,24),name="Sites") +
#   scale_fill_brewer(palette="Set2",name = "Data group",labels = c("OSBS & MLBS train", "OSBS & MLBS test", "*TALL test*")) +
#   guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 24))),shape=F) +
#   theme_bw() +
#   xlab("Number of samples") +
#   theme(legend.position = c(0.7,0.1),legend.text = element_markdown())


gwide <- ggplot(data=taxonID_long,aes(x=taxonID,y=count)) +
  geom_point(stat="identity",aes(fill=use,shape=site2),size=5,color="white",alpha=0.7) +
  geom_text_repel(aes(label=count,fill=use,fontface=fontface),direction="y",label.padding=0.5) + 
  scale_shape_manual(values=c(21,24),name="Sites") +
  scale_fill_brewer(palette="Set2",name = "Data group",labels = c("OSBS & MLBS train", "*OSBS & MLBS test*", "**TALL test**")) +
  guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 24))),shape=F) +
  theme_bw() +
  labs(x="taxonID",y="Number of samples") +
  my_theme +
  theme(legend.position = c(0.85,0.7),legend.text = element_markdown()
        ,axis.text.x=element_text(angle=45,hjust=0.9,vjust = 0.9),panel.grid.major = element_line())

#ggsave("figures/taxonID_groups_orderByTrain.png",plot=gwide,height=4,width=8,units="in")ggsave("figures/Figure.png",plot=gwide,height=4,width=8,units="in")


# HSI REFL FOR PLOTS ----
rsdf <- read.csv("output_data/sample_hsi_values.csv")

rsdf$mean <- rsdf$mean/10000
rsdf$sd <- rsdf$sd/10000


rsdf$datagroup <- factor(rsdf$datagroup,levels=c("train","om_test","tall_test"))
#rsdf$fontface <- "bold"
#rsdf$fontface[rsdf$site=="TALL"] <- "italic"

gspec <- ggplot(data=rsdf) +
  #geom_point(data=rsdf,alpha=0.1,aes(x=band,y=value,color=site)) +
  #geom_point(data=rsdf_mean,aes(x=band,y=mean,color=site)) +
  geom_errorbar(aes(y=mean,x=micro_group,ymin=mean-sd, ymax=mean+sd,color=datagroup),
                width=.2,position=position_dodge(0.05),alpha=0.5) +
  geom_line(aes(x=micro_group,y=mean,color=datagroup,linetype=datagroup),size=1,alpha=0.8) +
  labs(y="Reflectance",x="Wavelength (nanometers)") +
  scale_color_brewer(palette="Set2",name="Data group",labels = c("OSBS & MLBS train", "OSBS & MLBS test", "TALL test")) +
  scale_linetype(name="Data group",labels = c("OSBS & MLBS train", "OSBS & MLBS test", "TALL test")) +
  scale_y_continuous(limits = c(0,0.5)) +
  scale_x_continuous(limits=c(400,2300)) +
  theme_bw() +
  my_theme +
  theme(legend.position = c(.85,.7),legend.key.width=unit(2,"cm"),panel.grid.major = element_line())

#ggsave("figures/wavelength.png",plot=gspec,width=8,height=3,units="in")


# MULTI-PANEL PLOT FIGURE 3 ----
both <- grid.arrange(gwide,gspec,ncol=1)
ggsave("figures/Figure3.png",plot=both,width=8,height=6,units="in")
ggsave("figures/Figure3.pdf",plot=both,width=8,height=7,units="in")


# CONFUSION MATRIX -------------------

# must run the confusion matrix file first
type_of_cm <- "aggregated"

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
cm <- read.csv(paste0("output_data/","confusion_matrix_noGatorsense_",type_of_cm,".csv"),stringsAsFactors = T)

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
  theme(axis.text.x = element_text(vjust = 0.7,angle=45,size=12),)

ggsave(filename = "figures/cm_aggregated_noGatorsense_revised.png",height=8.5,width=8)

# CONFUSION MATRIX BY TEAM -----
type_of_cm <- "team"
cm <- read.csv(paste0("output_data/","confusion_matrix_noGatorsense_",type_of_cm,".csv"))

# remove taxonID
# this is an important line but it is confusing logic
# removing all rows that have the taxonID in either the Prediction or Truth column
cm <- cm[!cm$Prediction %in% taxonID_remove &! cm$Truth %in% taxonID_remove,]

# order teams
cm$Team <- factor(cm$Team,levels=c("Fujitsu","CAU","Jalapenos","Treepers","baseline"))

# order Truth alphabetically
cm$Truth <- sort_alphabetically(cm$Truth)
cm$Prediction <- sort_alphabetically(cm$Prediction,reverse = F)

# figure out how to add diag as bold
cm$fontface <- "plain"

cm$fontface[cm$Prediction == cm$Truth] <- "bold"

for(team in levels(cm$Team)){
  print(team)
  cm_team <- cm[cm$Team==team,]
  
  ggplot(cm_team,aes(x=Prediction,y=Truth,fill=TruthGenus,color=PredGenus,alpha=log(Freq)))+
    geom_tile(size=1) +
    geom_text(aes(label = FreqLabels,fontface=fontface),color="black", vjust = .5, alpha = 1) +
    scale_fill_manual(name = "genus",na.value="grey", values = my_colors) +
    scale_color_manual(name = "genus",na.value="grey", values = my_colors) +
    labs(fill = "Genus",title=team) +
    guides(color = FALSE, alpha = FALSE,fill=guide_legend(nrow=2)) +
    theme_light() +
    my_theme +
    theme(axis.text.x = element_text(vjust = 0.7,angle=45,size=12),)
  
  
  ggsave(filename = paste0("figures/cm_",team,".png"),height=8.50,width=8)
  
}

# DELINEATION SCORES ----

scores <- read.csv("output_data/scores_itcs.csv",stringsAsFactors = T)
scores$itc_id <- as.character(scores$itc_id)
scores$team <- factor(scores$team,levels=c("Fujitsu","CAU","INRAE","baseline"))

iou <- scores[scores$name=="IoU",]
iou$name <- as.character(droplevels(iou$name))

iou$area_bin <- round_any(iou$area,20,f=ceiling)

iou_binned <- iou %>%
  group_by(team,site,area_bin) %>%
  dplyr:::summarise(IoU=round(mean(value),2))

iou_binned$site <- factor(iou_binned$site,levels=c("OSBS","MLBS","TALL"))

scatter <- ggplot(iou_binned,aes(x=area_bin,y=IoU,color=site))+
  #geom_line() +
  geom_point(size=4,alpha=0.5) +
  stat_smooth(method="loess",se=F,color="black") +
  labs(x="Crown area (sq m)") +
  facet_wrap(~team,nrow=4,strip.position = "right") +
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.5,1)) +
  theme_bw() +
  my_theme +
  theme(legend.text = element_text(face = "plain"),legend.position = "none",
        panel.grid.major = element_line(),
        axis.text.y = element_text(size=8),
        strip.text = element_blank(),
        panel.spacing = unit(.5, "lines"))

#ggsave("figures/scatterplot_iou_areaBin.png",width=6,height=4)


# boxplot by team and site?
boxplot <- ggplot(iou_binned,aes(x=site,y=IoU))+
  geom_boxplot(aes(fill=site),alpha=0.5) +
  facet_wrap(~team,nrow=4,strip.position = "right") +
  scale_y_continuous(breaks=c(0,.5,1)) +
  theme_bw() +
  my_theme +
  theme(panel.grid.major = element_line(),
        legend.position = "none",
        axis.title.y = element_blank(),axis.text.y = element_blank()
        ,panel.spacing = unit(.5, "lines"))

combo <- grid.arrange(scatter,boxplot,nrow=1,widths=c(2,1))

ggsave("figures/iou_scatter_boxplot.png",plot=combo,width=8,height=6,units="in")



# EXAMPLE DELINEATION PLOTS -----
rs_folder <- "/Users/sjgraves/idtrees_competition_evaluation/RS/"
rs_paths <- list_RS_files(rs_folder,type="RGB",site=site,paths=T)
rsplot <- rs_paths[4]
site <- "MLBS"
base_folder <- "/Users/sjgraves/idtrees_competition_evaluation/"
team_list <- read_csv("~/idtrees_competition_evaluation/task1_teams_list.csv")

delin <- read.csv("output_data/delineations_with_RSname.csv")
sground <- st_read("/Users/sjgraves/idtrees_competition_evaluation/eval_data/MLBS_ground.csv",quiet=T)
sground <- sground[,-1]

submission <- st_read(paste0(base_folder,"eval_team1_final/submission/MLBS_submission.csv"),quiet=T)
submission$id <- paste("NA",site,seq(1:nrow(submission)),sep=".")
submission <- submission[1,]

for(t in 1:nrow(team_list)){
  print(t)
  team_name <- as.character(team_list$team[t])
  team_number <- as.character(team_list$name[t])
  tsubmission <- st_read(paste0(base_folder,"eval_",team_number
                               ,"_final","/submission/MLBS_submission.csv"),quiet=T)
  tsubmission$id <- paste(team_name,site,seq(1:nrow(tsubmission)),sep=".")
  submission <- rbind(submission,tsubmission)
}

submission <- submission[-1,]
submission$team <- factor(gsub("\\..*","", submission$id),levels=c("Fujitsu","CAU","INRAE","baseline"))

if(site=="MLBS"|site=="OSBS"){
  st_crs(sground) <- 32617
  st_crs(submission) <- 32617
} else {st_crs(sground) <- 32616
st_crs(submission) <- 32616}

rs <- stack(rsplot)

# subset data
plotground <- st_crop(sground,extent(rs))
plotsubmission <- st_crop(submission,extent(rs))

subcolor <- "black"
groundcolor <- "orange"
subsize <- 2
delin_theme <- theme(legend.position = "none",
                       axis.title=element_blank(),axis.text=element_blank(),axis.ticks = element_blank(),
                     plot.title = element_text(hjust=0.5,size=16))


nsub <- plotsubmission %>%
  dplyr::count(team)

glist <- list()

for(t in 1:nrow(team_list)){
  team_name <- team_list$team[t]
  print(team_name)
  
  g <- ggRGB(rs,1, g=2, b=3,stretch="lin") +
    geom_sf(data=plotsubmission[plotsubmission$team==team_name,],color=subcolor,size=1,alpha=0.1) +
    geom_sf(data=plotground,fill=NA,color=groundcolor,size=2) +
    ggtitle(team_name) +
    geom_label(aes(x=extent(rs)[1]+4,y=extent(rs)[3]+1),label=paste("n=",nsub$n[nsub$team==team_name]),size=4,fill="white") +
    theme_bw() +
    delin_theme
  
  glist[[t]] <- g

}

gpanel <- grid.arrange(glist[[2]],glist[[1]],glist[[3]],glist[[4]],ncol=4)
ggsave("figures/delineation_examples.png",plot=gpanel,width=8,height=2,units = "in")







