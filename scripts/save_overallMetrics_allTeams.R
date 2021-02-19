# Compile data of overall metrics for all teams

library(dplyr)
library(ggplot2)
library(tidyr)

source("scripts/00-functions.R")

teams <- c("CAU","Fujitsu","Gatorsense","Jalapenos","Treepers","baseline")
#teams <- c("CAU","Fujitsu","Jalapenos","Treepers","baseline")

submission_path <- file.path("/Users/sjgraves/Google Drive/analysis/data/IDTREES_competition_2020/"
                             ,"participant_submissions/final_submission_reports/task2/")

# overall metrics for all teams
full <- NA
noTall <- NA
onlyTall <- NA
for(i in 1:length(teams)){
  team <- teams[i]
  
  # full
  team_output <- load_score_report_file(team_name=team,folder=submission_path,report_file_name = "_score_report.csv",output="overall")
  full <- rbind(full,team_output)
  
  # noTall
  team_output <- load_score_report_file(team_name=team,folder=submission_path,report_file_name = "_score_report_noTall.csv",output="overall")
  noTall <- rbind(noTall,team_output)
  
  # skip over Gatorsense team since they don't have TALL predictions
  if(team!="Gatorsense"){
    team_output <- load_score_report_file(team_name=team,folder=submission_path,report_file_name = "_score_report_onlyTall.csv",output="overall")
    onlyTall <- rbind(onlyTall,team_output)
  }

} # end loop, remove first NA row
full <- full[-1,]
noTall <- noTall[-1,]
onlyTall <- onlyTall[-1,]

# add group to each separate data frame
full$sites <- "All"
noTall$sites <- "No TALL"
onlyTall$sites <- "Only TALL"

metrics <- rbind(full,noTall)
metrics <- rbind(metrics,onlyTall)


# convert to long format
metrics_long <- pivot_longer(data=metrics,cols=c("accuracy","macro","weighted","logloss"),names_to="metric")

overall_metrics_compare$sites <- factor(overall_metrics_compare$sites,levels=c("no TALL","all"))
overall_metrics_compare$metric <- factor(overall_metrics_compare$metric,levels=c("accuracy","weighted","macro","logloss"))

# drop accuracy
metrics_long <- metrics_long[metrics_long$metric!="accuracy",]

# save output
write.csv(metrics_long,"output_data/table_metrics_allTeams.csv",row.names = F)





