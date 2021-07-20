# creating data frame of taxonID metrics

library(dplyr)
library(tidyr)

source("scripts/00-functions.R")

# read in teams folders
# these are all the folders within the submissions folder
# folders were created manually with submission file
# score reports were created with task2_evaluation.py script
teams <- list.dirs("submissions",recursive = F,full.names = F)
sets <- c("all","trained","untrained")

# read in taxonID info
# extract genus
# https://stackoverflow.com/questions/15895050/using-gsub-to-extract-character-string-before-white-space-in-r
taxonID_scientificInfo <- read.csv("evaluation_data/taxonID_ScientificName.csv")
taxonID_scientificInfo$genus <- gsub( " .*$", "", taxonID_scientificInfo$scientificName )

# loop through eval data and teams
taxonID_metrics <- NA
for(t in 1:length(teams)){
  for(s in 1:length(sets)){
    
    set <- sets[s]
    team <- teams[t]
    
    team_output <- load_score_report_file(team_name=team,
                                          folder="submissions/",
                                          report_file_name = paste0("_",set,"_scoreReport.csv"),
                                          output="taxonID")
    
    team_output$sites <- set
    taxonID_metrics <- rbind(taxonID_metrics,team_output)
 
  } # end set loop
} # end team loop

taxonID_metrics <- taxonID_metrics[-1,]

taxonID_metrics_long <- taxonID_metrics %>%
  pivot_longer(cols=c("precision","recall","f1.score"),names_to="metric") %>%
  left_join(taxonID_scientificInfo)

taxonID_metrics_long$metric2 <- "secondary"
taxonID_metrics_long$metric2[taxonID_metrics_long$metric=="f1.score"] <- "primary"

# SAVE DATA
write.csv(taxonID_metrics_long,"output_data/taxonID_metrics_allTeams.csv",row.names = F)



