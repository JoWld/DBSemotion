### Setup ------------------------------------------------------------------------------------

rm(list = ls())
#install.packages("readr") #uncomment as necessary
#install.packages("openxlsx") #uncomment as necessary
library(readr)
library(openxlsx)

min_response_time = 100 #minimum response time to include
max_sd = 3  #how many standard deviations above mean to include 

### Functions & Definitions ---------------------------------------------------------------------

#using only last two letters to avoid encoding issues
dict = list('el' = 'DIS', #Ekel
            'ng' = 'SUS', #?berraschung
            'it' = 'HAS', #Fr?hlichkeit
            'er' = 'SAS', #Trauer
            'ck' = 'NES', #neutraler Gesichtsausdruck
            'ut' = 'ANS', #Wut
            'st' = 'AFS')#Angst

emotions = c("AFS", "ANS", "DIS", "HAS", "NES", "SAS", "SUS")

#substring extraction
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#returns fraction of correct answers for tests 1 to 4
correct1 = function(patient,number){
  
  #convert list to dataframe
  df = patient[[number]]
  
  #calculate mean/sd response times for responses where candidate pressed space
  response_time_mean = mean(df$response_time[df$response == "space"])
  response_time_sd = sd(df$response_time[df$response == "space"])
  
  #only keep rows where response is "None" or rows where response time is >min_response_time milliseconds AND <Mean+max_sd SDs
  df = df[(df$response_time<(response_time_mean+max_sd*response_time_sd) & df$response_time > min_response_time) | df$response == "None",]
  
  res = mean(df$correct) #fraction of correct answers, space when go-stimulus appears and none when no-go-stimulus appears 
  res_go = mean(df$correct[df$correct_response == "space"]) #fraction of correct answers for the go stimulus
  res_nogo = mean(df$correct[df$correct_response == "None"]) #fraction of correct answers for the no-go-stimulus
  
  res_time = mean(df$response_time[df$correct_response == "space" & df$response == "space"]) #mean response time, including only correct go-stimulus answers 
  
  return(c(res,res_go,res_nogo,res_time))
  
}

#returns fraction of correct answers for test 5
correct5 = function(patient,number){
  
  df = patient[[number]]
  
  response_time_mean = mean(df$response_time)
  response_time_sd = sd(df$response_time)
  
  df = df[(df$response_time<(response_time_mean+max_sd*response_time_sd) & df$response_time > min_response_time) | df$response == "None",]
  
  res = mean(df$correct) #fraction of correct answers
  res_anger = mean(df$correct[df$target == "anger"]) #fraction of correct answers when one face is angry
  res_happy = mean(df$correct[df$target == "happy"]) #fraction of correct answers when one face is happy
  res_odd = mean(df$correct[(df$target == "anger") | (df$target == "happy")]) #fraction of correct answers when one emotion is different
  res_neutral = mean(df$correct[df$target == "neutral"]) #fraction of correct answers when all faces are neutral
  
  res_time = mean(df$response_time[(df$response == "left" & df$target == "anger") | (df$response == "left" & df$target == "happy") | df$response == "right" & df$target == "neutral"])
  res_time_anger = mean(df$response_time[df$response == "left" & df$target == "anger"])
  res_time_happy = mean(df$response_time[df$response == "left" & df$target == "happy"])
  res_time_odd = mean(df$response_time[(df$response == "left" & df$target == "anger") | (df$response == "left" & df$target == "happy")]) 
  res_time_neutral = mean(df$response_time[df$response == "right" & df$target == "neutral"])
  
  return(c(res, res_anger, res_happy, res_odd, res_neutral, res_time, res_time_anger, res_time_happy, res_time_odd, res_time_neutral))
  
}

#returns fraction of correct answers for test 6
correct6 = function(patient,number){

  df = patient[[number]]
  
  response_time_mean = mean(df$response_time)
  response_time_sd = sd(df$response_time)
  
  df = df[(df$response_time<(response_time_mean+max_sd*response_time_sd) & df$response_time > min_response_time) | df$response == "None",]
  
  res = mean(df$correct) #total of correct answers
  res_odd = mean(df$correct[df$correct_response == "left"]) #fraction of correct answers when one individual is different
  res_equal = mean(df$correct[df$correct_response == "right"]) #fraction of correct answers when all images are the same individual
  res_time = mean(df$response_time[(df$correct_response == "left" & df$response == "left") | df$correct_response == "right" & df$response == "right"]) #response time when correct response
  res_time_odd = mean(df$response_time[df$correct_response == "left" & df$response == "left"]) #response time when correct and one individual is different
  res_time_equal = mean(df$response_time[df$correct_response == "right" & df$response == "right"]) #response time when correct and all images are the same indivudal
  
  return(c(res,res_odd,res_equal,res_time,res_time_odd,res_time_equal))
  
}

#returns fraction of correct answers for tests 7 & 9
#first changes all emotion responses to their last two letters (to avoid encoding issues), then replaces with dictionary to compare to correct emotion
correct2 = function(patient,number){
  
  for (m in 1:nrow(patient[[number]]["response"])){
    patient[[number]]["response"][m,] = substrRight(patient[[number]]["response"][m,],2)
    
  }
  
  for (j in 1:7){
    patient[[number]]["response"] <- unlist(
      replace(
        patient[[number]]["response"], 
        patient[[number]]["response"] == names(dict[j]), 
        dict[j])
      )
  }
  
  res = sum(patient[[number]]["response"] == patient[[number]]["emotion"])/nrow(patient[[number]]["response"])
  
  res_d = c()
  for (m in emotions){
    res_d = c(res_d, sum((patient[[number]]["response"] == patient[[number]]["emotion"]) * (patient[[number]]["emotion"] == m)) / sum(patient[[number]]["emotion"] == m))
  
    for (n in emotions){
      
      res_d = c(res_d, sum((patient[[number]]["response"] == n) * (patient[[number]]["emotion"] == m)))
      
    }
    
  }
  
  #print(res_d)
  
  return(c(res,res_d))
  
}

#returns fraction of correct answers for test 8
#counts what fraction of responses identify the correct gender
correct3 = function(patient,number){
  
  count = 0
  max = nrow(patient[[number]]["stimulus"])
  
  for (j in 1:max){
    if(grepl("Mann",patient[[number]]["response"][j,]) == grepl("AM",patient[[number]]["stimulus"][j,])){
      count = count + 1
      
    }
    
  }
  
  return(count/max)
  
}

### Setup-------------------------------------------------------------------------------------------------------

setwd() #Folder including all participant folders

patients_dirs = list.dirs(getwd(),recursive = FALSE)
patients_names = list.dirs(getwd(),recursive = FALSE, full.names = FALSE)

### Data read-in------------------------------------------------------------------------------------------------

for(h in 1:length(patients_dirs)){
  filenames = list.files(patients_dirs[h], pattern="*.csv", full.names=TRUE)
  
  for(k in 1:length(filenames)){
    assign(patients_names[h],lapply(filenames, read.csv, encoding="latin1"))
    
  }
  
    print(c("Read-in patient",patients_names[h],"concluded"))
  
}

### Analysis----------------------------------------------------------------------------------------------------

results_total = data.frame(matrix(NA,    # Create empty data frame
                          nrow = 147,
                          ncol = 0))

rownames(results_total) = c("1 Imp. X","1.1 Imp. X_GO","1.2 Imp. X_NOGO","1.3 Imp. X_GO_Time","2 Imp. Happy","2.1 Imp. Happy_GO","2.2 Imp. Happy_NOGO","2.3 Imp. Happy_GO_Time","3 Imp. K","3.1 Imp. K_GO","3.2 Imp. K_NOGO","3.3 Imp. K_GO_Time","4 Imp. Anger","4.1 Imp. Anger_GO","4.2 Imp. Anger_NOGO","4.3 Imp. Anger_GO_Time","5 FitCEmo Ges.","5.1 Anger","5.2 Happy","5.3 Odd","5.4 Neutral","5.5 FitCEmo Ges._Time","5.6 Anger_Time","5.7 Happy_Time","5.8 Odd_Time","5.9 Neutral_Time","6 FitC NonEmo","6.1 FitC NonEmo_Odd","6.2 FitC NonEmo_Equal","6.3 FitC NonEmo_time","6.4 FitC NonEmo_Odd_time","6.5 FitC NonEmo_Equal_time","7 EmoRecog1","7.1 Afraid","7.1.1 Afraid","7.1.2 Anger","7.1.3 Disgust","7.1.4 Happy","7.1.5 Neutral","7.1.6 Sad","7.1.7 Surprise","7.2 Anger","7.2.1 Afraid","7.2.2 Anger","7.2.3 Disgust","7.2.4 Happy","7.2.5 Neutral","7.2.6 Sad","7.2.7 Surprise","7.3 Disgust","7.3.1 Afraid","7.3.2 Anger","7.3.3 Disgust","7.3.4 Happy","7.3.5 Neutral","7.3.6 Sad","7.3.7 Surprise","7.4 Happy","7.4.1 Afraid","7.4.2 Anger","7.4.3 Disgust","7.4.4 Happy","7.4.5 Neutral","7.4.6 Sad","7.4.7 Surprise","7.5 Neutral","7.5.1 Afraid","7.5.2 Anger","7.5.3 Disgust","7.5.4 Happy","7.5.5 Neutral","7.5.6 Sad","7.5.7 Surprise","7.6 Sad","7.6.1 Afraid","7.6.2 Anger","7.6.3 Disgust","7.6.4 Happy","7.6.5 Neutral","7.6.6 Sad","7.6.7 Surprise","7.7 Surprise","7.7.1 Afraid","7.7.2 Anger","7.7.3 Disgust","7.7.4 Happy","7.7.5 Neutral","7.7.6 Sad","7.7.7 Surprise","8 N.-EmoRec","9 EmoRecog2","9.1 Afraid","9.1.1 Afraid","9.1.2 Anger","9.1.3 Disgust","9.1.4 Happy","9.1.5 Neutral","9.1.6 Sad","9.1.7 Surprise","9.2 Anger","9.2.1 Afraid","9.2.2 Anger","9.2.3 Disgust","9.2.4 Happy","9.2.5 Neutral","9.2.6 Sad","9.2.7 Surprise","9.3 Disgust","9.3.1 Afraid","9.3.2 Anger","9.3.3 Disgust","9.3.4 Happy","9.3.5 Neutral","9.3.6 Sad","9.3.7 Surprise","9.4 Happy","9.4.1 Afraid","9.4.2 Anger","9.4.3 Disgust","9.4.4 Happy","9.4.5 Neutral","9.4.6 Sad","9.4.7 Surprise","9.5 Neutral","9.5.1 Afraid","9.5.2 Anger","9.5.3 Disgust","9.5.4 Happy","9.5.5 Neutral","9.5.6 Sad","9.5.7 Surprise","9.6 Sad","9.6.1 Afraid","9.6.2 Anger","9.6.3 Disgust","9.6.4 Happy","9.6.5 Neutral","9.6.6 Sad","9.6.7 Surprise","9.7 Surprise","9.7.1 Afraid","9.7.2 Anger","9.7.3 Disgust","9.7.4 Happy","9.7.5 Neutral","9.7.6 Sad","9.7.7 Surprise")

for (k in patients_names){
  results = data.frame()
  
  j = get(k)
  
    #Analyse tests
  #Tests 1-4
  for(i in 1:4){
    for(m in 1:4){results = rbind(results, correct1(j,i)[m])}}
  
  #Test 5 
  for(i in 1:10){results = rbind(results, correct5(j,5)[i])}
  
  #Test 6
  for(i in 1:6){results = rbind(results, correct6(j,6)[i])}
  
  #Test 7  
  for(i in 1:57){results = rbind(results, correct2(j,7)[i])}
  
  #Test 8  
  results = rbind(results, correct3(j,8))
    
  #Test 9
  for(i in 1:57){results = rbind(results, correct2(j,9)[i])}
  
  #Result handling
  colnames(results) = k
  results_total = cbind(results_total, results)
  print(c("Analysis patient",k,"concluded"))
  
}

#Write complete results to Excel file
write.xlsx(results_total, "analysis.xlsx", rowNames=TRUE)

save(results_total,file="scores.Rda")

### Change score calc-------------------------------------------------------------------------------------------------------


output = data.frame(matrix(NA,    # Create empty data frame
                           nrow = 147,
                           ncol = 0))

rownames(output) = c("1 Imp. X","1.1 Imp. X_GO","1.2 Imp. X_NOGO","1.3 Imp. X_GO_Time","2 Imp. Happy","2.1 Imp. Happy_GO","2.2 Imp. Happy_NOGO","2.3 Imp. Happy_GO_Time","3 Imp. K","3.1 Imp. K_GO","3.2 Imp. K_NOGO","3.3 Imp. K_GO_Time","4 Imp. Anger","4.1 Imp. Anger_GO","4.2 Imp. Anger_NOGO","4.3 Imp. Anger_GO_Time","5 FitCEmo Ges.","5.1 Anger","5.2 Happy","5.3 Odd","5.4 Neutral","5.5 FitCEmo Ges._Time","5.6 Anger_Time","5.7 Happy_Time","5.8 Odd_Time","5.9 Neutral_Time","6 FitC NonEmo","6.1 FitC NonEmo_Odd","6.2 FitC NonEmo_Equal","6.3 FitC NonEmo_time","6.4 FitC NonEmo_Odd_time","6.5 FitC NonEmo_Equal_time","7 EmoRecog1","7.1 Afraid","7.1.1 Afraid","7.1.2 Anger","7.1.3 Disgust","7.1.4 Happy","7.1.5 Neutral","7.1.6 Sad","7.1.7 Surprise","7.2 Anger","7.2.1 Afraid","7.2.2 Anger","7.2.3 Disgust","7.2.4 Happy","7.2.5 Neutral","7.2.6 Sad","7.2.7 Surprise","7.3 Disgust","7.3.1 Afraid","7.3.2 Anger","7.3.3 Disgust","7.3.4 Happy","7.3.5 Neutral","7.3.6 Sad","7.3.7 Surprise","7.4 Happy","7.4.1 Afraid","7.4.2 Anger","7.4.3 Disgust","7.4.4 Happy","7.4.5 Neutral","7.4.6 Sad","7.4.7 Surprise","7.5 Neutral","7.5.1 Afraid","7.5.2 Anger","7.5.3 Disgust","7.5.4 Happy","7.5.5 Neutral","7.5.6 Sad","7.5.7 Surprise","7.6 Sad","7.6.1 Afraid","7.6.2 Anger","7.6.3 Disgust","7.6.4 Happy","7.6.5 Neutral","7.6.6 Sad","7.6.7 Surprise","7.7 Surprise","7.7.1 Afraid","7.7.2 Anger","7.7.3 Disgust","7.7.4 Happy","7.7.5 Neutral","7.7.6 Sad","7.7.7 Surprise","8 N.-EmoRec","9 EmoRecog2","9.1 Afraid","9.1.1 Afraid","9.1.2 Anger","9.1.3 Disgust","9.1.4 Happy","9.1.5 Neutral","9.1.6 Sad","9.1.7 Surprise","9.2 Anger","9.2.1 Afraid","9.2.2 Anger","9.2.3 Disgust","9.2.4 Happy","9.2.5 Neutral","9.2.6 Sad","9.2.7 Surprise","9.3 Disgust","9.3.1 Afraid","9.3.2 Anger","9.3.3 Disgust","9.3.4 Happy","9.3.5 Neutral","9.3.6 Sad","9.3.7 Surprise","9.4 Happy","9.4.1 Afraid","9.4.2 Anger","9.4.3 Disgust","9.4.4 Happy","9.4.5 Neutral","9.4.6 Sad","9.4.7 Surprise","9.5 Neutral","9.5.1 Afraid","9.5.2 Anger","9.5.3 Disgust","9.5.4 Happy","9.5.5 Neutral","9.5.6 Sad","9.5.7 Surprise","9.6 Sad","9.6.1 Afraid","9.6.2 Anger","9.6.3 Disgust","9.6.4 Happy","9.6.5 Neutral","9.6.6 Sad","9.6.7 Surprise","9.7 Surprise","9.7.1 Afraid","9.7.2 Anger","9.7.3 Disgust","9.7.4 Happy","9.7.5 Neutral","9.7.6 Sad","9.7.7 Surprise")


for (i in 1:ncol(results_total)){
  
  #only continue if column is a follow up
  #For simplicity we can assume if follow up exists, baseline will be column left of it
  if(substrRight((colnames(results_total)[i]),2) == "FU"){
    
    output = cbind(output,results_total[,i] - results_total[,i-1])
    colnames(output)[ncol(output)] = colnames(results_total[i])
    
  }
}

write.xlsx(output, "change_scores.xlsx", rowNames=TRUE)

save(output, file = "output.Rdata")


