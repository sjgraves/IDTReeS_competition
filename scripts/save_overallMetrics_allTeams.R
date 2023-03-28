# Compile data of overall metrics for all teams
# Saves long data frame with F1 and logloss scores for all, trained, and untrained evaluation data

library(dplyr)
library(tidyr)

source("scripts/00-functions.R")

# read in teams folders
# these are all the folders within the submissions folder
# folders were created manually with submission file
# score reports were created with task2_evaluation.py script
teams <- list.dirs("submissions",recursive = F,full.names = F)

# overall metrics for all teams
full <- NA
noTall <- NA
onlyTall <- NA
for(i in 1:length(teams)){
  team <- teams[i]
  
  # full
  team_output <- load_score_report_file(team_name=team,folder="submissions/",report_file_name = "_all_scoreReport.csv",output="overall")
  full <- rbind(full,team_output)
  
  # trained
  team_output <- load_score_report_file(team_name=team,folder="submissions/",report_file_name = "_trained_scoreReport.csv",output="overall")
  noTall <- rbind(noTall,team_output)
  
  # untrained
  team_output <- load_score_report_file(team_name=team,folder="submissions/",report_file_name = "_untrained_scoreReport.csv",output="overall")
  onlyTall <- rbind(onlyTall,team_output)
  

} # end team loop

# remove NAs
full <- full[-1,]
noTall <- noTall[-1,]
onlyTall <- onlyTall[-1,]

# add group to each separate data frame
full$sites <- "All"
noTall$sites <- "No TALL"
onlyTall$sites <- "Only TALL"

# merge dfs into 1
metrics <- rbind(full,noTall)
metrics <- rbind(metrics,onlyTall)

# convert to long format 
metrics_long <- pivot_longer(data=metrics,cols=c("accuracy","macro","weighted","logloss"),names_to="metric")

# drop accuracy
# Nov 2022 - not dropping accuracy because editor wants it
#metrics_long <- metrics_long[metrics_long$metric!="accuracy",]

# save output
write.csv(metrics_long,"output_data/table_metrics_allTeams.csv",row.names = F)





