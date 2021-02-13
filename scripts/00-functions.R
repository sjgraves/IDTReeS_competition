# supporting functions to analyze IDTReeS competition


load_score_report_file <- function(team_name,folder,report_file_name,output="list"){
  
  # red in file
  score_report <- read.csv(paste0(folder,team_name,"/report/",team_name,report_file_name))
  
  # overall metrics
  logloss <- round(score_report[nrow(score_report),ncol(score_report)],1)
  accuracy <- round(score_report[score_report$X=="accuracy",colnames(score_report) == "f1.score"],2)
  macro <- round(score_report[score_report$X=="macro avg",colnames(score_report) == "f1.score"],2)
  weighted <- round(score_report[score_report$X=="weighted avg",colnames(score_report) == "f1.score"],2)
  
  metrics <- data.frame(team=team_name,
                        accuracy=accuracy,
                        macro=macro,
                        weighted=weighted,
                        logloss=logloss)
  
  # taxonID metrics
  ntaxonID <- nrow(score_report)-4
  taxonID <- score_report[1:ntaxonID,1:4]
  colnames(taxonID)[1] <- "taxonID"
  taxonID$team <- team_name
  
  # give output as list, or individual metrics
  
  if(output=="list") {
    
    print("returning list")
    report_list <- list(taxonID=taxonID,metric=metrics)
    return(report_list)
    
  } else if (output=="taxonID"){
    print("returning metrics for each taxonID")
    return(taxonID)
  } else if (output=="overall"){
    print("returning overall metrics")
    return(metrics)
  }
}


# create the function to read in data, add team name, save output
# use in for loop to generate long data format of all team submissions
load_indvdID_file <- function(team_name,report_file_name,folder){
  
  submission <- read.csv(paste0(folder,team_name,"/report/",team_name,"_",report_file_name,".csv"))
  colnames(submission)[1] <- "team"
  submission$team <- team_name
  
  return(submission)
  
}


# example
# cm <- load_cm_file(team_name = team,report_file_name = "confusion_matrix_full", folder=submission_path)
load_cm_file <- function(team_name,report_file_name,folder){
  
  cm <- read.csv(paste0(folder,team_name,"/report/",team_name,"_",report_file_name,".csv"))
  return(cm)
}
