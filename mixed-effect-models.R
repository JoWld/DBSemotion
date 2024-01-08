

library(readr)
library(plyr)
library(dplyr) 
library(lme4)
library(lmerTest)
library(emmeans)
library(stringr)
library(rstatix)
## Setup ------------------------------------------------------------------------------------
setwd("/Users/huser/Desktop/All_Subjects-Emotion_DBS")

patients_dirs = list.dirs(getwd(),recursive = FALSE)
patients_names = list.dirs(getwd(),recursive = FALSE, full.names = FALSE)

### Functions & Definitions ---------------------------------------------------------------------

min_response_time = 100 #minimum response time to include
max_sd = 3  #how many standard deviations above mean to include 

#using only last two letters to avoid encoding issues
dict = list('el' = 'DIS', #Ekel
            'ng' = 'SUS', #?berraschung
            'it' = 'HAS', #Fr?hlichkeit
            'er' = 'SAS', #Trauer
            'ck' = 'NES', #neutraler Gesichtsausdruck
            'ut' = 'ANS', #Wut
            'st' = 'AFS') #Angst

emotions = c("AFS", "ANS", "DIS", "HAS", "NES", "SAS", "SUS")

#substring extraction
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#define function to get df per subject and remove trials with outlier RTs for impulsivity task
correct = function(patient,number){
  df = patient[[number]]
  response_time_mean = mean(df$response_time[df$response == "space" | df$response == "space "])
  response_time_sd = sd(df$response_time[df$response == "space" | df$response == "space "])
  df = df[(df$response_time<(response_time_mean+max_sd*response_time_sd) & df$response_time > min_response_time) | df$response == "None",]
  return(df)
}

# same for fitc task
correct_fitc = function(patient,number){
  
  df = patient[[number]]
  
  response_time_mean = mean(df$response_time)
  response_time_sd = sd(df$response_time)
  
  df = df[(df$response_time<(response_time_mean+max_sd*response_time_sd) & df$response_time > min_response_time)]
  return(df)
}

# emotional part of the emo recog task
#first changes all emotion responses to their last two letters (to avoid encoding issues), then replaces with dictionary to compare to correct emotion
correct_emo = function(patient,number){
  
  df = patient[[number]]
  
  for (m in 1:nrow(df["response"])){
    df["response"][m,] = substrRight(df["response"][m,],2)
    
  }
  
  for (j in 1:7){
    df["response"] <- unlist(
      replace(
        df["response"], 
        df["response"] == names(dict[j]), 
        dict[j])
    )
  }
  
  return(df)
}

#non-emotional part of the emo recog task
correct8 = function(patient,number){
  df = patient[[number]]
  return(df)
}

### Data read-in------------------------------------------------------------------------------------------------
# load trial-data per subject

for(h in 1:length(patients_dirs)){
  filenames = list.files(patients_dirs[h], pattern="*.csv", full.names=TRUE)
  
  for(k in 1:length(filenames)){
    assign(patients_names[h],lapply(filenames, read.csv, encoding="latin1"))
    
  }
  print(c("Read-in patient",patients_names[h],"concluded"))
  
}

#create separate dataframes for each task 
imp= data.frame()
#  get dataframe for first tests (impulsivity)
# add columns with id, time, group, emotion info 
for (k in patients_names){
  trials = data.frame()
  j = get(k)
  trials1=correct(j,1)
  trials1$task = "imp_non-emo"
  trials3=correct(j,3)
  trials3$task = "imp_non-emo"
  trials2=correct(j,2)
  trials2$task = "imp_emo"
  trials4=correct(j,4)
  trials4$task = "imp_emo"
  trials = rbind.fill(trials1, trials2, trials3, trials4)
  trials$sub_id=k
  trials$group = sub('(^.*?)-.*', '\\1', trials$sub_id)
  trials$time  = str_sub(trials$sub_id,-2,-1)
  trials$subj = str_sub(trials$sub_id,-7,-4)
  imp = rbind.fill(imp, trials)}

# get separate df only for emotional imp. tasks, separate by "go"-emotion
imp_emo= data.frame()
for (k in patients_names){
  trials = data.frame()
  trials2 = data.frame()
  j = get(k)
  trials2=correct(j,2)
  trials2$task = "happy"
  trials4=correct(j,4)
  trials4$task = "anger"
  trials = rbind.fill(trials2, trials4)
  trials$sub_id=k
  trials$group = sub('(^.*?)-.*', '\\1', trials$sub_id)
  trials$time  = str_sub(trials$sub_id,-2,-1)
  trials$subj = str_sub(trials$sub_id,-7,-4)
  imp_emo = rbind.fill(imp_emo, trials)}


fitc = data.frame()
for (k in patients_names){
  trials = data.frame()
  j = get(k)
  trials1=correct_fitc(j,5)
  trials1$task = "fitc_emo"
  trials2=correct_fitc(j,6)
  trials2$task = "fitc_non-emo"
  trials = rbind.fill(trials1, trials2)
  trials$sub_id=k
  trials$group = sub('(^.*?)-.*', '\\1', trials$sub_id)
  trials$time  = str_sub(trials$sub_id,-2,-1)
  trials$subj = str_sub(trials$sub_id,-7,-4)
  fitc = rbind.fill(fitc, trials)}

# get df for non-emotional part of emo recog task and calculate accuracy
non_emo = data.frame()
for (k in patients_names){
  trials = data.frame()
  m = get(k)
  trials=correct8(m,8)
  trials$task = "emorecog_non-emo"
  trials$sub_id=k
  trials$group = sub('(^.*?)-.*', '\\1', trials$sub_id)
  trials$time  = str_sub(trials$sub_id,-2,-1)
  trials$subj = str_sub(trials$sub_id,-7,-4)
  non_emo = rbind.fill(non_emo, trials)}

non_emo$stimulus = str_sub(non_emo$stimulus,-4,-3)
non_emo$correct[(non_emo$response == "Mann" & non_emo$stimulus== "AM") | (non_emo$response == "Frau" & non_emo$stimulus== "AF") ] <- 1
non_emo$correct[(non_emo$response == "Mann"& non_emo$stimulus== "AF") | (non_emo$response == "Frau" & non_emo$stimulus== "AM") ] <- 0
non_emo$correct <- as.numeric(non_emo$correct)


emo = data.frame()
#  get dataframe for first tests (impulsivity)
for (k in patients_names){
  trials = data.frame()
  j = get(k)
  trials1=correct_emo(j,7)
  trials1$task = "emorecog_emo"
  trials3=correct_emo(j,9)
  trials3$task = "emorecog_emo"
  trials= rbind.fill(trials1, trials3)
  trials$sub_id=k
  trials$group = sub('(^.*?)-.*', '\\1', trials$sub_id)
  trials$time  = str_sub(trials$sub_id,-2,-1)
  trials$subj = str_sub(trials$sub_id,-7,-4)
  emo = rbind.fill(emo, trials)}

emo$correct[(emo$response == emo$emotion) ] <- 1
emo$correct[(emo$response != emo$emotion)] <- 0
emo$correct <- as.numeric(emo$correct)

emorecog <- rbind.fill(emo, non_emo)

############### IMPULSIVITY ###########################
###### commisson errors
imp_nogo <- filter(imp, imp$correct_response == "None")
#get mean and sd accuracy per group,task and time
imp_nogo%>%
  group_by(task, group, time) %>%
  get_summary_stats(correct, type = "mean_sd")
# build model
imp_nogo_logmodel <- glmer(correct ~ group*time*task + (1 | subj), data = imp_nogo, family = binomial)
summary(imp_nogo_logmodel)

contrast(emmeans(imp_nogo_logmodel, ~ time*group*task),
         interaction = c("poly", "consec", "consec"))
# visualize 
emmip(imp_nogo_logmodel, task ~ time | group)
#get ANOVA-style table 
joint_tests(imp_nogo_logmodel)

# ANOVA-style table with grouping to check for time effects within the DBS group only per task
joint_tests(imp_nogo_logmodel, by = c("group", "task"))

##### omission errors
imp_go <- filter(imp, imp$correct_response == "space"| imp$correct_response == "space ")
#get mean and sd accuracy per group,task and time
imp_go %>%
  group_by(task, group, time) %>%
  get_summary_stats(correct, type = "mean_sd")
# build model
imp_go_logmodel <- glmer(correct ~ group*time*task + (1 | subj), data = imp_go, family = binomial)
summary(imp_go_logmodel)
contrast(emmeans(imp_go_logmodel, ~ time*group*task),
         interaction = c("poly", "consec", "consec"))
# visualize 
emmip(imp_go_logmodel, task ~ time | group)
#get ANOVA-style table 
joint_tests(imp_go_logmodel)
# ANOVA-style table by group to check for efects within each group
joint_tests(imp_go_logmodel, by = "group")

# ANOVA-style table with grouping to check for time effects within the DBS group only per task
joint_tests(imp_go_logmodel, by = c("group", "task"))

###### response time correct trials (i.e., only go trials with correct response)
imp_rt <- filter(imp, imp$correct ==1 & (imp$response == "space" | imp$response == "space "))
#get mean and sd accuracy per group,task and time
imp_rt %>%
  group_by(task, group, time) %>%
  get_summary_stats(response_time, type = "mean_sd")

# build model
imp_rt_logmodel <- lmer(response_time ~ group*time*task+ (1 | subj), data = imp_rt,REML = T)
summary(imp_rt_logmodel)
# get ANOVA-style table for linear reg. model
anova(imp_rt_logmodel, type =3)
# should be the same as:
joint_tests(imp_rt_logmodel)
# visualize 
emmip(imp_rt_logmodel, task ~ time | group)

joint_tests(imp_rt_logmodel, by = c("group", "task"))

##### Secondary analysis emotional only  ################
###### commisson errors
imp_emo_nogo <- filter(imp_emo, imp_emo$correct_response == "None")
imp_emo_nogo_logmodel <- glmer(correct ~ group*time*task + (1 | subj), data = imp_emo_nogo, family = binomial)
summary(imp_emo_nogo_logmodel)
##### omission errors
imp_emo_go <- filter(imp_emo, imp_emo$correct_response == "space"| imp_emo$correct_response == "space ")
# build model
imp_emo_go_logmodel <- glmer(correct ~ group*time*task + (1 | subj), data = imp_emo_go, family = binomial)
summary(imp_emo_go_logmodel)
# visualize 
emmip(imp_emo_go_logmodel, task ~ time | group)
joint_tests(imp_emo_go_logmodel)

###### response time correct trials (i.e., only go trials with correct response)
imp_emo_rt <- filter(imp_emo, imp_emo$correct ==1 & (imp_emo$response == "space" | imp_emo$response == "space "))
# build model
imp_emo_rt_logmodel <- lmer(response_time ~ group*time*task + (1 | subj), data = imp_emo_rt,REML = T)
summary(imp_emo_rt_logmodel)
anova(imp_emo_rt_logmodel)

################## FACE IN THE CROWD ############################

fitc%>%
  group_by(task, group, time) %>%
  get_summary_stats(correct, type = "mean_sd")

fitc_logmodel <- glmer(correct ~ group*time*task + (1 | subj), data = fitc, family = binomial)
summary(fitc_logmodel)
# ANOVA-style table by group to check for time effects within each group
joint_tests(fitc_logmodel, by = c("group", "task"))

fitc_rt_logmodel <- lmer(response_time ~ group*time*task + (1 | subj), data = fitc)
summary(fitc_rt_logmodel)
# ANOVA-style table by group to check for time effects within each group
joint_tests(fitc_rt_logmodel, by = c("group", "task"))

##Secondary analysis emotional only 
fitc_emo <- filter(fitc, fitc$task == "fitc_emo")
fitc_emo%>%
  group_by(target, group, time) %>%
  get_summary_stats(correct, type = "mean_sd")

# compare angry vs. happy stimuli only
fitc_emo <- filter(fitc_emo, fitc_emo$target != "neutral")
fitc_emo_logmodel <- glmer(correct ~ group*time*target + (1 | subj), data = fitc_emo, family = binomial)
summary(fitc_emo_logmodel)
# visualize 
emmip(fitc_emo_logmodel, target ~ time | group)
joint_tests(fitc_emo_logmodel)
# ANOVA-style table by group to check for effects within each group
joint_tests(fitc_emo_logmodel, by = c("group", "target"))

#rt
fitc_emo_logmodel <- lmer(correct ~ group*time*target + (1 | subj), data = fitc_emo)
summary(fitc_emo_logmodel)

############# EMOTION RECOGNITION TASK #####################

emorecog_logmodel <- glmer(correct ~ group*time*task + (1 | subj), data = emorecog, family = binomial)
summary(emorecog_logmodel)
# visualize 
emmip(emorecog_logmodel, task ~ time | group)
joint_tests(emorecog_logmodel)
# ANOVA-style table by group to check for effects within each group
joint_tests(emorecog_logmodel, by = c("group", "task"))

## only emotional task
# get summary (mean, sd) per group, time and emotion 
emo%>%
  group_by(emotion, group, time) %>%
  get_summary_stats(correct, type = "mean_sd")%>% 
  print(n=42)

# afraid 
fear <- filter(emo, emo$emotion=="AFS")
fear_logmodel <- glmer(correct ~ group*time + (1 | subj), data = fear, family = binomial)
summary(fear_logmodel)
joint_tests(fear_logmodel)
joint_tests(fear_logmodel, by = "group")

disgust <- filter(emo, emo$emotion=="DIS")
disgust_logmodel <- glmer(correct ~ group*time + (1 | subj), data = disgust, family = binomial)
summary(disgust_logmodel)
joint_tests(disgust_logmodel)
joint_tests(disgust_logmodel, by = "group")

happiness <- filter(emo, emo$emotion=="HAS")
happiness_logmodel <- glmer(correct ~ group*time + (1 | subj), data = happiness, family = binomial)
summary(happiness_logmodel)
joint_tests(happiness_logmodel, by = "group")

sadness <- filter(emo, emo$emotion=="SAS")
sadness_logmodel <- glmer(correct ~ group*time + (1 | subj), data = sadness, family = binomial)
summary(sadness_logmodel)
joint_tests(sadness_logmodel, by = "group")

anger <- filter(emo, emo$emotion=="ANS")
anger_logmodel <- glmer(correct ~ group*time + (1 | subj), data =anger, family = binomial)
summary(anger_logmodel)
joint_tests(anger_logmodel, by = "group")

surprise <- filter(emo, emo$emotion=="SUS")
surprise_logmodel <- glmer(correct ~ group*time + (1 | subj), data =surprise, family = binomial)
joint_tests(surprise_logmodel)
joint_tests(surprise_logmodel, by = "group")

neutral <- filter(emo, emo$emotion=="NES")
neutral_logmodel <- glmer(correct ~ group*time + (1 | subj), data = neutral, family = binomial)
joint_tests(neutral_logmodel)
joint_tests(neutral_logmodel, by = "group")
