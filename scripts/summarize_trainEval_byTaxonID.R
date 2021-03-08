# Create a summary table with information on the taxonID
# Includes taxonomic information and the number of samples per taxonID

library(dplyr)
library(tidyr)

summarize_other <- TRUE

# read in train data for classification task
dir_data_in <- "../../../data/IDTREES_competition_2020/Data/"

# MULTIPLE PLOTS ASSOCIATED WITH A SINGLE INDVDID IN THIS FILE - WHY???
data_train <- read.csv(file.path(dir_data_in,"/train_v2/Field/train_data.csv"))
data_eval_t2 <- read.csv(file.path(dir_data_in,"/evaluate/data_task2.csv"))

# join these datasets, need column for train and evaluate
data_train$usage <- "train-t2"
data_eval_t2$usage <- "eval-t2"

full_df <- bind_rows(data_train,data_eval_t2)
full_df <- distinct(full_df)

# extract genus
# https://stackoverflow.com/questions/15895050/using-gsub-to-extract-character-string-before-white-space-in-r
full_df$genus <- gsub( " .*$", "", full_df$scientificName )

# create wide table
# rows - taxonID
# columns - # in each of the 3 groupings
# create 3 categories
full_df$t2group <- NA

full_df$t2group[full_df$siteID=="TALL" & full_df$usage=="eval-t2"] <- "tall_eval"
full_df$t2group[full_df$usage=="eval-t2" & full_df$siteID !="TALL"] <- "om_eval"
full_df$t2group[full_df$usage=="train-t2"& full_df$siteID !="TALL"] <- "om_train"

# SUMMARIZE TAXONID BY GROUP (FOR FIGURE) ----
summary_taxonID_byGroup <- full_df %>%
  group_by(taxonID,t2group) %>%
  summarise(count=n(),.groups="drop") %>%
  pivot_wider(id_cols=taxonID,names_from=t2group,values_from=count) %>%
  left_join(full_df[colnames(full_df) %in% c("scientificName","taxonID")]) %>%
  distinct() %>%
  relocate(om_train, .before=om_eval) %>% 
  relocate(scientificName,.before=taxonID) %>%
  dplyr:::arrange(desc(om_train));head(summary_taxonID_byGroup)

# calculate total for Other category
# other category is those without any train data
other_taxonID <- summary_taxonID_byGroup[is.na(summary_taxonID_byGroup$om_train),] %>%
  summarise(scientificName="",taxonID="Other"
            ,om_train=0
            ,om_eval=sum(om_eval,na.rm = T)
            ,tall_eval=sum(tall_eval,na.rm = T))

# combine with summary df
if(summarize_other==T){
  # remove rows from summary df
  print("summarizing Other taxonID class")
  output_df <- summary_taxonID_byGroup %>%
    dplyr:::filter(!is.na(summary_taxonID_byGroup$om_train)) %>%
    rbind(other_taxonID)
} else{output_df=summary_taxonID_byGroup}

# SUMMARIZE BY SITE * GROUP (FOR TABLE)-----
# why do my numbers here differ from numbers in other script for t2?
table_t2_itcs <- full_df %>%
  select(indvdID,siteID,usage) %>%
  distinct() %>%
  group_by(siteID,usage) %>%
  summarise(count=n(),.groups="drop")


# SAVE OUTPUT ----
write.csv(output_df
          ,"output_data/table_taxonID_trainEval.csv"
          ,row.names = F
          ,na="")
