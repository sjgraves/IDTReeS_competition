# creating data frame of taxonID metrics

library(dplyr)
library(tidyr)

source("scripts/00-functions.R")
#teams <- c("CAU","Fujitsu","Gatorsense","Jalapenos","Treepers","baseline")
teams <- c("CAU","Fujitsu","Jalapenos","Treepers","baseline")
#remove_from_figure <- c("PRSE2","PEPA37","OXYDE","ACPE","AMLA","GOLA","FAGR")

# dataset
set <- "noTall"
report_name <- paste0("_score_report_",set,".csv")

# read in train data for classification task
dir_data_in <- "../../../data/IDTREES_competition_2020/Data/"
eval <- read.csv(file.path(dir_data_in,"/evaluate/data_task2.csv"))
train <- read.csv(file.path(dir_data_in,"/train_v2/Field/train_data.csv"))


submission_path <- file.path("/Users/sjgraves/Google Drive/analysis/data/IDTREES_competition_2020/"
                             ,"participant_submissions/final_submission_reports/task2/")

train$usage <- "train"
eval$usage <- "eval"
full_df <- bind_rows(train,eval)

# extract genus
# https://stackoverflow.com/questions/15895050/using-gsub-to-extract-character-string-before-white-space-in-r
full_df$genus <- gsub( " .*$", "", full_df$scientificName )

taxonID_scientificInfo <- select(full_df,taxonID,genus) %>% distinct() %>% as.data.frame()

# loop over each team
# rbind taxon ID output
taxonID_metrics <- NA
for(i in 1:length(teams)){
  
  team <- teams[i]
  team_output <- load_score_report_file(team_name=team,
                                        folder=submission_path,
                                        report_file_name = report_name,
                                        output="taxonID")
  
  taxonID_metrics <- rbind(taxonID_metrics,team_output)
  
} # end loop, remove first NA row
taxonID_metrics <- taxonID_metrics[-1,]



taxonID_metrics_long <- left_join(taxonID_metrics,taxonID_scientificInfo) %>%
  pivot_longer(cols=c("precision","recall","f1.score"),names_to = "metric") %>%
  as.data.frame()

taxonID_metrics_long$metric2 <- "secondary"
taxonID_metrics_long$metric2[taxonID_metrics_long$metric=="f1.score"] <- "primary"

#taxonID_metrics_long <- taxonID_metrics_long[!taxonID_metrics_long$taxonID %in% remove_from_figure,]


# SAVE DATA
write.csv(taxonID_metrics_long,
          paste0("output_data/","taxonID_metrics_noGatorsense_",set,".csv"),row.names = F)



