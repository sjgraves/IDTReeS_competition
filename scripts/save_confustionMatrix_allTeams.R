# Compile data from individual predictions
# Save out data to create confusion matricies for plotting

library(dplyr)
library(tidyr)

source("scripts/00-functions.R")

# set type of confusion matrix to create "aggregated" or "team"
type_of_cm <- "aggregated"

# read in teams folders
# these are all the folders within the submissions folder
# folders were created manually with submission file
# score reports were created with task2_evaluation.py script
teams <- list.dirs("submissions",recursive = F,full.names = F)

# read in taxonID info
# extract genus
# https://stackoverflow.com/questions/15895050/using-gsub-to-extract-character-string-before-white-space-in-r
taxonID_scientificInfo <- read.csv("evaluation_data/taxonID_ScientificName.csv")
taxonID_scientificInfo$genus <- gsub( " .*$", "", taxonID_scientificInfo$scientificName )

submission_path <- file.path("/Users/sjgraves/Google Drive/analysis/data/IDTREES_competition_2020/"
                             ,"participant_submissions/final_submission_reports/task2/")

# loop through all teams to created combined cm
cm_df_allTeams <- NA

for(i in 1:length(teams)){
  
  team <- teams[i]
  team_submission <- load_indvdID_file(team_name = team
                                       ,report_file_name = "all_indvdIDpredictions_reduced"
                                       ,folder="submissions/")
  
  
  # pull out taxonIDs from predictions and observations
  # need to set same levels for predictions and observations
  taxonID_levels <- unique(c(team_submission$taxonID_pred,team_submission$taxonID_obs))
  
  team_submission$pred <- factor(team_submission$taxonID_pred,levels=taxonID_levels)
  team_submission$obs <- factor(team_submission$taxonID_obs,levels = taxonID_levels)
  
  # create confusion matrix and convert to data frame
  cm <- conf_mat(data=team_submission,truth = obs,estimate=pred)
  cm_df <- as.data.frame(cm$table)
  cm_df$Team <- team
  cm_df_allTeams <- rbind(cm_df_allTeams,cm_df)
  
} # end loop
cm_df_allTeams <- cm_df_allTeams[-1,]

# aggregate by pre and truth taxonID
cm_aggregated <- cm_df_allTeams %>%
  group_by(Prediction,Truth) %>%
  summarise(Freq = sum(Freq))

# SET TYPE OF CONFUSION MATRIX
if(type_of_cm=="aggregated"){
  print("using aggregated data")
  cm <- cm_aggregated
} else{cm <- cm_df_allTeams}

# clean up cm
cm <- as.data.frame(cm)

# calculate percent and remove zeros
cm$Pct <- round((cm$Freq/sum(cm$Freq)),2)

tmp <- cm$Freq==0

cm$FreqLabels <- cm$Freq
cm$FreqLabels[tmp] <- NA

# join genus information for plotting
tmp_Truth_join <- left_join(cm,taxonID_scientificInfo[,c(2,4)],by=c("Truth"="taxonID"))
tmp_Pred_join <- left_join(cm,taxonID_scientificInfo[,c(2,4)],by=c("Prediction"="taxonID"))

tmp_Truth_join <- tmp_Truth_join$genus
tmp_Pred_join <- tmp_Pred_join$genus

cm$PredGenus <- tmp_Pred_join
cm$TruthGenus <- tmp_Truth_join


# SAVE DATA
write.csv(cm,paste0("output_data/","confusion_matrix_",type_of_cm,".csv"),row.names = F)


