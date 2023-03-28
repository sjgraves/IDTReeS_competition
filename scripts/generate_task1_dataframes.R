# create full task 1 files
# associate all submitted delineation with the plot
# associate selected delineation with attributes of ground delineation
# save out two files to be used in competition analysis

library(sf)
library(raster)
library(dplyr)
library(readr)
library(tidyr)

source("scripts/00-functions.R")

base_folder <- "/Users/sjgraves/idtrees_competition_evaluation/"
rs_folder <- paste0(base_folder,"RS/")
eval_folder <- paste0(base_folder,"eval_data/")
team_list <- read_csv("~/idtrees_competition_evaluation/task1_teams_list.csv")
sites <- c("MLBS","OSBS","TALL")


# CREATE DATA FRAME OF NUMBER OF DELINEATIONS PER PLOT ----
# loops over all teams and all sites
# reads in RS files, creates extent, counts delineations center points within extent
# saves output as data frame
full_rs_itc_count <- data.frame(site=NA,team=NA,rs_names=NA,itc_count=NA)

for(t in 1:nrow(team_list)){
  
  team_name <- as.character(team_list$team[t])
  team_number <- as.character(team_list$name[t])
  print(team_name)
  
  for(s in 1:length(sites)){
  
    site <- sites[s]
    
    rs_names <- list_RS_files(rs_folder,type="RGB",site=site,paths=F)
    rs_paths <- list_RS_files(rs_folder,type="RGB",site=site,paths=T)
    
    ground <- st_read(paste0(eval_folder,site,"_ground.csv"),quiet=T)
    ground <- ground[,-1]
    
    submission <- st_read(paste0(base_folder,"eval_",team_number
                                 ,"_final","/submission/",site,"_submission.csv"),quiet=T)
    submission <- submission[-1]
    
    if(site=="MLBS"|site=="OSBS"){
      st_crs(ground) <- 32617
      st_crs(submission) <- 32617
    } else {st_crs(ground) <- 32616
    st_crs(submission) <- 32616
    }
    
    # How many submissions per plot? ----
    
    # generate center points from itcs
    itc_cent <- st_centroid(submission)
    
    # create empty list to store list of itcs for each raster
    site_team_itc_count <- data.frame(site=site,team=team_name,rs_names,itc_count=NA)
    
    # loop through rasters, generate extents
    # need to know the extents to be able to count the number of delineations per plot
    for(i in 1:length(rs_paths)){
      print(rs_names[i])
      rsi <- raster(rs_paths[i])
      rsi_extent <- as(extent(rsi), "SpatialPolygons")
      crs(rsi_extent) <- crs(ground)
      rs_extent <- st_as_sf(rsi_extent)
      itc_within <- itc_cent[st_within(itc_cent,rs_extent,sparse=F),]
      site_team_itc_count$itc_count[i] <- nrow(itc_within)
    } # end rs loop

    # combine with full list for all sites and teams
    full_rs_itc_count <- rbind(full_rs_itc_count,site_team_itc_count)
  
  } #end site loop
} # end team loop

full_rs_itc_count <- full_rs_itc_count[-1,]

# save output
write.csv(full_rs_itc_count,"output_data/table_itcCount_byPlot.csv",row.names = F)


# CREATE DATA FRAME OF MATCHED SUBMISSION AND GROUND DELINEATIONS ----

# dataframe to store team, itc id of ground delineation, and scores
# this will be used to create a long format and bind with itc size information
full_itc_scores <- data.frame(X=NA,itc_id=NA,rand_index=NA,IoU=NA)

# loop over each team
# read in scores file, bind to full data frame
for(t in 1:4){
  print(t)
  team_name <- as.character(team_list$team[t])
  team_number <- as.character(team_list$name[t])

  # read in file
  print(paste0(base_folder,"scores_",team_number,"_final","/task1_evaluation.csv"))
  scores <- read.csv(paste0(base_folder,"scores_",team_number,"_final","/task1_evaluation.csv"))
  scores[,1] <- team_name
  full_itc_scores <- rbind(full_itc_scores,scores)

} # end team loop
full_itc_scores <- full_itc_scores[-1,]
colnames(full_itc_scores)[1] <- "team"

# convert to long format
long_scores <- pivot_longer(full_itc_scores,cols=c("rand_index","IoU"))
long_scores$value <- round(long_scores$value,2)

# load site ground truth
ground <- data.frame(id=NA,area=NA)
for(s in 1:length(sites)){
  site <- sites[s]

  sground <- st_read(paste0(eval_folder,site,"_ground.csv"),quiet=T)
  sground <- sground[,-1]
  
  if(site=="MLBS"|site=="OSBS"){
    st_crs(sground) <- 32617
  } else {st_crs(sground) <- 32616  }
  
  sground$area <- st_area(sground)
  sgrounddf <- st_drop_geometry(sground)
  ground <- rbind(sgrounddf,ground)
}

colnames(ground)[1] <- "itc_id"
ground <- as.data.frame(ground)
ground$area <- as.numeric(round(ground$area,2))

# join ground and scores
long_scores_join <- left_join(long_scores,ground)

# extract site information
long_scores_join$site <- substr(long_scores_join$itc_id,1,4)

# save data
write.csv(long_scores_join,"output_data/scores_itcs.csv",row.names = F)


#### CREATE DATA FRAME OF ALL DELINATIONS AND PLOT INFO TO USE FOR CREATING A FIGURE --

# uses similar code as creating number of delinations per plot
loopOut <- data.frame(team_name=NA,site=NA,rs_name=NA,id=NA)

for(t in 1:nrow(team_list)){
  
  team_name <- as.character(team_list$team[t])
  team_number <- as.character(team_list$name[t])
  print(team_name)
  
  for(s in 1:length(sites)){
    
    site <- sites[s]
    
    rs_names <- list_RS_files(rs_folder,type="RGB",site=site,paths=F)
    rs_paths <- list_RS_files(rs_folder,type="RGB",site=site,paths=T)
    
    ground <- st_read(paste0(eval_folder,site,"_ground.csv"),quiet=T)
    ground <- ground[,-1]
    
    submission <- st_read(paste0(base_folder,"eval_",team_number
                                 ,"_final","/submission/",site,"_submission.csv"),quiet=T)
    submission <- submission[-1]
    
    # add unique ID number to submission
    submission$id <- paste(team_name,site,seq(1:nrow(submission)),sep=".")
    
    if(site=="MLBS"|site=="OSBS"){
      st_crs(ground) <- 32617
      st_crs(submission) <- 32617
    } else {st_crs(ground) <- 32616
    st_crs(submission) <- 32616
    }
    
    # How many submissions per plot? ----
    
    # generate center points from itcs
    itc_cent <- st_centroid(submission)
    
    # create empty list to store list of itc ids and rs path name
    # should build in the loop since we do not know the length for each iteratrion

    # loop through rasters, generate extents
    # need to know the extents to be able to count the number of delineations per plot
    for(i in 1:length(rs_paths)){
      print(rs_names[i])
      rsi <- raster(rs_paths[i])
      rsi_extent <- as(extent(rsi), "SpatialPolygons")
      crs(rsi_extent) <- crs(ground)
      rs_extent <- st_as_sf(rsi_extent)
      itc_within <- itc_cent[st_within(itc_cent,rs_extent,sparse=F),]
      
      if(nrow(itc_within)>0){
        loopDF <- data.frame(team_name=team_name,site=site,rs_name=rs_names[i],id=itc_within$id)

      } else{ print("no itcs")}
      loopOut <- rbind(loopOut,loopDF)
      #loopJoin <- left_join(loopOut,submission,by="id")
    } # end rs loop
  } # end site loop
} # end team loop

loopOut <- loopOut[-1,]

# next loop to join submission info to 


# summarize output
tally_itcs_perPlot <- loopOut %>%
  group_by(team_name,site,rs_name) %>%
  tally()

mean_itcs_perPlot <- loopOut %>%
  group_by(team_name,site,rs_name) %>%
  tally() %>%
  summarize(avg_itcs=mean(n))

# add rs name info to submission info

# save data
write.csv(mean_itcs_perPlot,"output_data/table_itcMean_byTeamSite.csv",row.names = F)
write.csv(tally_itcs_perPlot,"output_data/table_itcCount_byTeamSitePlot.csv",row.names = F)
write.csv(loopOut,"output_data/delineations_with_RSname.csv",row.names = F)


