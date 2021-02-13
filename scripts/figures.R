# script to create figures

library(ggplot2)

# CONFUSION MATRIX

# must run the confusion matrix file first
type_of_cm <- "team"

cm <- read.csv(paste0("output_data/","confusion_matrix_",type_of_cm,".csv"),stringsAsFactors = T)


# sort levels for plotting
levels_sort <- "alphabetically"

if(levels_sort=="alphabetically"){
  
  print("sorting alphabetically")
  
  # move other to end
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


custom_palette10 <- c("#F781BF","#377EB8","#E41A1C","#984EA3","#4DAF4A","#FF7F00","#35978F","#A65628","#FFED6F","#999999")

# AGGREGATED CONFUSION MATRIX
if(type_of_cm=="aggregated"){

ggplot(cm,aes(x=Prediction,y=Truth,fill=TruthGenus,color=PredGenus,alpha=log(Freq)))+
  geom_tile(size=1) +
  geom_text(aes(label = FreqLabels),color="black", vjust = .5, alpha = 1) +
  scale_fill_manual(values=custom_palette10,na.value="grey") +
  scale_color_manual(values=custom_palette10,na.value="grey") +
  labs(fill = "Genus") +
  guides(color = FALSE, alpha = FALSE) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle=90),
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

ggsave(filename = "figures/cm_aggregated.pdf",height=8,width=10)}

# CONFUSION MATRIX BY TEAM
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
