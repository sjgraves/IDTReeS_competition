#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 25 18:03:54 2020

@author: sjgraves
"""

import os
import pandas as pd
import numpy as np
from sklearn import metrics
from sklearn.metrics import log_loss
from sklearn.metrics import confusion_matrix


cwd = '/Volumes/GoogleDrive/My Drive/analysis/IDTReeS/competition2020_analysis/IDTReeS_competition/'

# set team name - this will be used when reading and saving the right file

submission_folder = cwd + 'submissions/'
evaluation_folder = cwd + 'evaluation_data/'

team_list = sorted([f for f in os.listdir(submission_folder) if not f.startswith('.')])

# loop through all teams 
# loop through all variations of evaluation data

eval_site_options = ['trained','untrained','all']

# options for running one loop
team = 'CAU'
eval_site = 'trained'

for team in team_list:
    for eval_site in eval_site_options:
        print(team,eval_site)
        
        # set file names
        submission_file = submission_folder + team + "/task2_submission.csv"
        #ground_file = evaluation_folder + "task2_labels.csv"
        
        #updating file name to use what is in the folder
        ground_file = evaluation_folder + "task2_labels_WITHHOLD.csv"

        trained_data = evaluation_folder + "taxonID_ScientificName.csv"
        
        # import data
        preds_full = pd.read_csv(submission_file)
        obs_full = pd.read_csv(ground_file)
        list_of_trained_species = pd.read_csv(trained_data)
        
        # SUBSET DATA BY EVALUATION SITE OPTION
        # uses eval_site to keep all the data or remove sites
        # probably a more efficient way to identify and subset rows - leave like this for now
        
        
        tall_obs = [i for i, s in enumerate(obs_full["indvdID"]) if "TALL" in s]
        tall_preds = [i for i, s in enumerate(preds_full["indvdID"]) if "TALL" in s]
        
        osbs_obs = [i for i, s in enumerate(obs_full["indvdID"]) if "OSBS" in s]
        mlbs_obs = [i for i, s in enumerate(obs_full["indvdID"]) if "MLBS" in s]
        
        trained_obs = sorted(osbs_obs+mlbs_obs)
        
        osbs_preds = [i for i, s in enumerate(preds_full["indvdID"]) if "OSBS" in s]
        mlbs_preds = [i for i, s in enumerate(preds_full["indvdID"]) if "MLBS" in s]
        
        trained_preds = sorted(osbs_preds+mlbs_preds)
        
        if eval_site == 'trained':
            print('removing TALL data')
            preds = preds_full.drop(tall_preds)
            obs = obs_full.drop(tall_obs)
        elif eval_site == 'untrained':
            print('removing OSBS and MLBS data')
            preds = preds_full.drop(trained_preds)
            obs = obs_full.drop(trained_obs)
        elif eval_site == 'all':
            print('keeping all data')
            preds = preds_full
            obs = obs_full
        
        del tall_obs,osbs_obs,mlbs_obs,trained_obs
        del tall_preds,osbs_preds,mlbs_preds,trained_preds
        
        
        # CHECK FOR MISSING TREES
        # find if there are any missing indvdIDs in the observed and predicted datasets
        # adds in the missing indvdIDs, gives them a taxonID of Other, and a probability of 1
        missing_obs = np.setdiff1d(obs.indvdID, preds.indvdID)
        miss = np.c_[missing_obs, np.zeros(len(missing_obs)),np.zeros(len(missing_obs))]
        miss[:,2] = "Other"
        miss[:,1] = 1
        preds = preds.append(pd.DataFrame(miss, columns = ["indvdID",  "probability", "taxonID"]))
        
        
        # =============================================================================
        # CREATE CONFUSION MATRIX FOR ORIGINAL DATA
        # Use original taxonID in the observed data - contains classes that do not exist in predictions
        # Using the original data allows us to see the confusion among true classes
        # =============================================================================
        
        # get class from majority vote
        idx_for_cm = preds.groupby(["indvdID"])["probability"].transform(max) == preds["probability"]
        preds_for_cm = preds[idx_for_cm]
        
        # ONE SUBMISSION REQUIRES MANIPULATION OF DATA FRAME
        # some indvdIDs have multiple taxonID predictions with the same probability
        # need to select just 1 predicted taxonID
        # look at each indvdID with muliple predictions
        # if there is a match with the observed data, keep that prediction
        # if there is no match, keep the first prediction
        # recreate the data frame for the confusion matrix with the selected prediction
        
        # this line is merging (or joining) pred and obs based on the indvdID
        evaluation_data_full = preds_for_cm.merge(obs,left_on="indvdID",right_on="indvdID",suffixes=("_pred","_obs"))
        
        if evaluation_data_full.shape[0] != obs.shape[0]:
        
            # find duplicate indvdIDs
            dup_temp = evaluation_data_full.duplicated(subset=['indvdID'],keep=False)
            multipred = evaluation_data_full[dup_temp]
            multipred["keep_match"]=""
            multipred['keep_match'] = np.where(multipred['taxonID_obs'] == multipred['taxonID_pred'],1,0)
            
            match_index = []
            
            # iterate over indvdids
            # if there aren't any 1 is the keep match column for that indvdID
            # change keep match column to 1 for the first record
            for id in np.unique(multipred['indvdID']):
                print(id)
                id_rows = multipred[multipred['indvdID'] == id]
                # does a row have a 1?
                if sum(id_rows['keep_match'])==0:
                    id_rows.iloc[0,id_rows.columns.get_loc("keep_match")] = 1
                    # output index to list
                    match_index.append(id_rows[id_rows['keep_match']==1].index.tolist()[0])
                           
            # use this index_label to change the keep_match to a 1 
            multipred.loc[match_index,"keep_match"] = 1
             
            # now filter to only include keep match 1s
            multipred_select = multipred['keep_match'] == 1
            multipred_select = multipred[multipred_select]
            
            multipred_ids = multipred_select['indvdID']
            # remove last column
            multipred_select = multipred_select.drop(columns='keep_match')
            
            # remove multipred_ids from prediction dataframe
            single_evaluation_data_full = evaluation_data_full[~evaluation_data_full['indvdID'].isin(multipred_ids)]
            
            # add selected multip preds
            evaluation_data_full = single_evaluation_data_full.append(multipred_select)
        
        # CREATE CONFUSION MATRIX
        obs_labels = np.unique(evaluation_data_full["taxonID_obs"])
        pred_labels = np.unique(evaluation_data_full["taxonID_pred"])
        both_labels = np.union1d(obs_labels,pred_labels)
        
        cm_full = confusion_matrix(y_true=evaluation_data_full["taxonID_obs"],y_pred=evaluation_data_full["taxonID_pred"])
        cm_df_full = pd.DataFrame(cm_full,index=both_labels,columns=both_labels)
        
        # =============================================================================
        # COMPUTE EVALUATION METRICS
        # For these, the observed and predicted classes must match
        # For the untrained taxonID, the correct match is to the class "Other"
        # Therefore, need to convert untrained taxonID into the "Other" class
        # =============================================================================
        
        # EDIT THE OBSERVED DATA
        
        # create array of what is in observed and not in trained
        # this is then a logical series that corresponds to the observations
        # false means it is in train data, true means it is not
        untrained = np.setdiff1d(obs.taxonID, list_of_trained_species.taxonID)
        untrained_entries = obs.taxonID.isin(untrained)
        
        # changes all observations that are true (not in train data) as other
        obs.taxonID[untrained_entries] = "Other"
        
        # EDIT THE PREDICTIONS DATA
        
        # create an object that will be used in the log loss function
        # rows are indvdID, columns are the taxonIDs, cell values are the prediction probabilities
        ce_preds = preds.pivot(index="indvdID", columns="taxonID", values="probability")
        
        # convert null predictions to zeros - this was an issue for 1 team
        ce_preds = ce_preds.fillna(0)
        
        # COMPUTE CROSS ENTROPY/LOG LOSS
        
        # compute cross entropy with the log loss function
        LS = log_loss(y_true = obs["taxonID"], y_pred = ce_preds, labels = ce_preds.columns)
        ls_score = LS
        
        # GET MAJORITY VOTE FOR EVALUATION METRICS
        
        # get class from majority vote
        idx = preds.groupby(["indvdID"])["probability"].transform(max) == preds["probability"]
        preds = preds[idx]
        
        # this line merging (or joining) pred and obs based on the indvdID
        evaluation_data = preds.merge(obs,left_on="indvdID",right_on="indvdID",suffixes=("_pred","_obs"))
        
        if evaluation_data.shape[0] != obs.shape[0]:
        
            # find duplicate indvdIDs
            dup_temp = evaluation_data.duplicated(subset=['indvdID'],keep=False)
            
            multipred = evaluation_data[dup_temp]
            
            multipred["keep_match"]=""
            
            multipred['keep_match'] = np.where(multipred['taxonID_obs'] == multipred['taxonID_pred'],1,0)
            
            match_index = []
            
            # iterate over indvdids
            # if there aren't any 1 is the keep match column for that indvdID
            # change keep match column to 1 for the first record
            for id in np.unique(multipred['indvdID']):
                print(id)
                id_rows = multipred[multipred['indvdID'] == id]
                # does a row have a 1?
                if sum(id_rows['keep_match'])==0:
                    id_rows.iloc[0,id_rows.columns.get_loc("keep_match")] = 1
                    # output index to list
                    match_index.append(id_rows[id_rows['keep_match']==1].index.tolist()[0])
                           
            # use this index_label to change the keep_match to a 1 
            multipred.loc[match_index,"keep_match"] = 1
             
            # nowwwwww, filter to only include keep match 1s
            multipred_select = multipred['keep_match'] == 1
            multipred_select = multipred[multipred_select]
            
            multipred_ids = multipred_select['indvdID']
            # remove last column
            multipred_select = multipred_select.drop(columns='keep_match')
            
            # remove multipred_ids from prediction dataframe
            single_evaluation_data = evaluation_data[~evaluation_data['indvdID'].isin(multipred_ids)]
            
            # add selected multip preds
            evaluation_data = single_evaluation_data.append(multipred_select)
        
        # CALCULATE CLASSIFICATION RESULTS AND REDUCED CONFUSION MATRIX
        
        obs_labels = np.unique(evaluation_data["taxonID_obs"])
        pred_labels = np.unique(evaluation_data["taxonID_pred"])
        both_labels = np.union1d(obs_labels,pred_labels)
        
        cm_reduced = confusion_matrix(y_true=evaluation_data["taxonID_obs"],y_pred=evaluation_data["taxonID_pred"])
        cm_df_reduced = pd.DataFrame(cm_reduced,index=both_labels,columns=both_labels)
        
        # create classification report
        classification_report = metrics.classification_report(y_true=evaluation_data["taxonID_obs"],y_pred=evaluation_data["taxonID_pred"],output_dict=True)
        
        metrics.accuracy_score(y_true=evaluation_data["taxonID_obs"],y_pred=evaluation_data["taxonID_pred"])
        
        metrics.precision_recall_fscore_support(y_true=evaluation_data["taxonID_obs"],y_pred=evaluation_data["taxonID_pred"],average='weighted')
        metrics.precision_recall_fscore_support(y_true=evaluation_data["taxonID_obs"],y_pred=evaluation_data["taxonID_pred"],average='micro')
        metrics.precision_recall_fscore_support(y_true=evaluation_data["taxonID_obs"],y_pred=evaluation_data["taxonID_pred"],average='macro')
        
        metrics.f1_score(y_true=evaluation_data["taxonID_obs"],y_pred=evaluation_data["taxonID_pred"],average='micro')
        
        df = pd.DataFrame(classification_report).transpose()
        
        df['LogLoss']=0
        ls_score = pd.Series([0,0,0,0,LS],index=df.columns,name="LogLoss")
        
        df = df.append(ls_score)
        
        # SAVE OUTPUTS
        
        # set names and folders
        report_name = team + '_' + eval_site + "_scoreReport.csv"
        cm_full_name = team + '_' + eval_site + "_confusionMatrix_full.csv"
        cm_reduced_name = team + '_' + eval_site + "_confusionMatrix_reduced.csv"
        
        preds_full_name = team + '_' + eval_site + '_indvdIDpredictions_full.csv'
        preds_reduced_name = team + '_' + eval_site + '_indvdIDpredictions_reduced.csv'
        
        
        # save score report df and confusion matrix
        df.to_csv(submission_folder + team + '/' + report_name)
        cm_df_full.to_csv(submission_folder + team + '/' + cm_full_name)
        cm_df_reduced.to_csv(submission_folder + team + '/' + cm_reduced_name)
        
        # save indvdID predictions
        evaluation_data_full.to_csv(submission_folder + team + '/' + preds_full_name)
        evaluation_data.to_csv(submission_folder + team + '/' + preds_reduced_name)






