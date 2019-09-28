#Run all ML model on data 
#apply 10-fold cross validation
#mix and match all possible combinations of parameters
#write results to excel file

AutoRunModel <- function(TimeStamp)
{
  library(tm)   #::Corpus, freqyency
  library(plyr) #::mutate()
  library(dplyr)#::select()
  
  Results = data.frame("Accuracy" = numeric() , "Model_name" = character() , 
                       "Stemmed" = logical(), "UseWords" = character(),
                       "Self_Centric" = logical(),"Feacture_Selector" = character(),
                       "idf" = logical(), "Account_Measures" = character(),
                       "Tweet_Sentm" = logical(), Synonym = logical(), stringsAsFactors = FALSE)

  UsersData = getAccountsData(TimeStamp)
  print("Users account measures loaded sucessfuly!!")

  #use only two categories of target
  #No Average depressed class...
  ignore = which(UsersData$Class == "Average Depressed")
  UsersData = UsersData  %>% filter(Class != "Average Depressed")
  UsersData$Class = droplevels(UsersData$Class)
  Tweets_corpus = CreateCorpus(TimeStamp, ignore = ignore)
  #OR...
  #Avg Merged with Depressed
  # ignore = NULL #use all users
  # library(stringr)
  # UsersData$Class =as.factor(as.character(stringr::str_replace_all(UsersData$Class, "Average Depressed", "Depressed")))
  # droplevels(UsersData$Class)
  # Tweets_corpus = CreateCorpus(TimeStamp, ignore = ignore)
  #OR, just keep all
  #Tweets_corpus = CreateCorpus(TimeStamp)
  
  print("Corpus Created!!")
  
  for( Model_name in c("SVM") ) #c("DS", "NB","SVM"))
    for (Stemmed in c(FALSE, TRUE))
      for (UseWords in c("Non-sparce", "DepSent"))
        for (self_cntr in c(TRUE, FALSE))
          for( idf in c(TRUE, FALSE))
            for(FSelector in c("None", "
                               
                               ", "MostFreq")) #"Best"
              for (AccMsr in c("as_is", "norm", "catg"))
                for (Sentm in c("None","Avg", "Mixed"))
                 #for(Synon in c(FALSE, TRUE))
                {
                  #10-fold cross Validation
                  Model_parms = data.frame(Model_name = Model_name,
                                      linear = TRUE, #tuned = False , 
                                      Stemmed = Stemmed,
                                      UseWords = UseWords,
                                      self_cntr = self_cntr, 
                                      FSelector = FSelector,
                                      idf = idf ,
                                      AccMsr = AccMsr,
                                      Sentm =  Sentm, 
                                      Synon = FALSE,
                                      stringsAsFactors = FALSE)
                  
                  PerRun = CrossValidate(TimeStamp = TimeStamp, TwtCorpus = Tweets_corpus, 
                                         UsersData = UsersData,
                                         Model_parms = Model_parms)
                  Results = rbind(Results, PerRun)
                  print(paste(PerRun$Model_name, PerRun$Accuracy)) #model, accuracy
                  
                  #run one more time for radial
                  if(Model_name == "SVM"){
                    Model_parms$linear = FALSE 
                    PerRun = CrossValidate(TwtCorpus = Tweets_corpus, 
                                           UsersData = UsersData,Model_parms = Model_parms)
                    Results = rbind(Results, PerRun)
                    print(paste(PerRun$Model_name, PerRun$Accuracy)) #model, accuracy
                  }
                  
                }#model run
  
 
  #write result data to file
  FileName = paste("Data/ModelResultsNN_", TimeStamp, ".csv", sep = "")
  write.csv(Results, file= FileName ,row.names=FALSE)
  print(paste("results stored in file ", FileName))
  
}

CrossValidate<- function(TimeStamp,TwtCorpus, 
                         UsersData,Model_parms){
  
  Accuracy <- rep(NA,10)
  
  for(n in 1:10)
  {
    Acc = RunModel(TimeStamp, 
                   TwtCorpus = TwtCorpus, 
                   UsersData = UsersData,
                   Model_name = Model_parms$Model_name, 
                   linear = Model_parms$linear, #tuned = False , 
                   Stemmed = Model_parms$Stemmed,
                   UseWords = Model_parms$UseWords,
                   self_cntr = Model_parms$self_cntr, 
                   FSelector = Model_parms$FSelector,
                   idf = Model_parms$idf, 
                   AccMsr = Model_parms$AccMsr,
                   Sentm = Model_parms$Sentm,Synon = FALSE)
    
    Accuracy[n] <- Acc
  }#loop
  
  Acc = max(Accuracy)
  if(Model_parms$Model_name == "SVM")
    Model_parms$Model_name = paste(Model_parms$Model_name, ifelse(Model_parms$linear, "-linear", "-radial"), sep = "")
  
  PerRun <- mutate(Model_parms, "Accuracy" = Acc)
  
  #return
  PerRun
}



