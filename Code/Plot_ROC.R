Plot_ROC <- function(TimeStamp, UseAccounts = "all")
  #which accounts to use!!
  #
{
  #TimeStamp = "02052018"
  library(ROCR)
  library(pROC)
  library(stringr) #::str_replace_all()
  
  #Load data
  UsersData = getAccountsData(TimeStamp)
  print("Users account measures loaded sucessfuly!!")
  
  if(UseAccounts == "all"){
    Tweets_corpus = CreateCorpus(TimeStamp)
  }
  else if (UseAccounts == "noAvg"){
    #use only two categories of target
    #No Average depressed class...
    ignoreAcc = which(UsersData$Class == "Average Depressed")
    UsersData = UsersData  %>% filter(Class != "Average Depressed")
    UsersData$Class = droplevels(UsersData$Class)
    Tweets_corpus = CreateCorpus(TimeStamp, ignore = ignoreAcc)
  }
  else if( UseAccounts == "merge" ){
    #Avg Merged with Depressed
    ignoreAcc = NULL #use all users
    library(stringr)
    UsersData$Class =as.factor(as.character(stringr::str_replace_all(UsersData$Class, "Average Depressed", "Depressed")))
    Tweets_corpus = CreateCorpus(TimeStamp, ignore = ignoreAcc)
  }
  print("Corpus Created!!")
  
  #fixed training data set
  N= dim(UsersData)[1] #no. of users in data set
  id_train <- sample(N, N*0.70)
  test_accounts = UsersData[-id_train,]$Class
  test_accounts = factorToNum(test_accounts)
  
  #run Models, get pred
  DS_pred = RunModel_Pred(TwtCorpus = Tweets_corpus,
                          UsersData = UsersData,
                          id_train= id_train, #training set
                          Model_name = "DS",
                          Stemmed = TRUE,
                          UseWords = "DepSent",
                          self_cntr = TRUE, #exclude
                          FSelector = "None",
                          idf = TRUE, 
                          AccMsr = "catg",
                          Sentm = "None",
                          Synon = TRUE)
  print("DS Model Done!!")
  
  NB_pred = RunModel_Pred(TwtCorpus = Tweets_corpus,
                          UsersData = UsersData,
                          id_train = id_train, #training set
                          Model_name = "NB",
                          Stemmed = TRUE,
                          UseWords = "DepSent",
                          self_cntr = TRUE, #exclude
                          FSelector = "None",
                          idf = TRUE, 
                          AccMsr = "catg",
                          Sentm = "None",
                          Synon = TRUE)
  print("NB Model Done!!")
  
  SVM_radial_pred = RunModel_Pred(TwtCorpus = Tweets_corpus,
                                  UsersData = UsersData,
                                  id_train = id_train, #training set
                          Model_name = "SVM",
                          linear = FALSE, #options for SVM
                          Stemmed = TRUE,
                          UseWords = "DepSent",
                          self_cntr = TRUE, #exclude
                          FSelector = "None",
                          idf = TRUE, 
                          AccMsr = "catg",
                          Sentm = "None",
                          Synon = TRUE)
  print("SVM Radial Model Done!!")
  
  SVM_Linear_pred = RunModel_Pred(TwtCorpus = Tweets_corpus,
                                  UsersData = UsersData,
                          id_train = id_train, #training set
                          Model_name = "SVM",
                          linear = TRUE, 
                          Stemmed = TRUE,
                          UseWords = "DepSent",
                          self_cntr = TRUE, #exclude
                          FSelector = "None",
                          idf = TRUE, 
                          AccMsr = "catg",
                          Sentm = "None",
                          Synon = TRUE)
  
  print("SVM Linear Model Done!!")
  
  #convert factor values to numbers
  DS_pred = factorToNum(DS_pred)
  NB_pred = factorToNum(NB_pred)
  SVM_Linear_pred = factorToNum(SVM_Linear_pred)
  SVM_radial_pred = factorToNum(SVM_radial_pred)
  
  if(UseAccounts != "all")
  {
    # List of predictions
    preds_list <- list(DS_pred, NB_pred , SVM_Linear_pred,SVM_radial_pred)
    # List of actual values (same for all)
    m <- length(preds_list)
    actuals_list <- rep(list(test_accounts), m)
    
    # Plot the ROC curves
    pred <- prediction(preds_list, actuals_list)
    rocs <- performance(pred, "tpr", "fpr")
    plot(rocs, col = as.list(1:m), main = paste(UseAccounts, " ROC Curves"))
    legend(x = "bottomright", 
           legend = c("Decision Tree", "Naive Bayse", "SVM_Linear", "SVM_radial"),
           fill = 1:m)
    
    #print measures for each model
    #only on binary output!!!!
    print("The Decision Tree:")
    PerdMeasures(test_accounts, DS_pred)
    print("The Naive Bayse:")
    PerdMeasures(test_accounts, NB_pred)
    print("The Liear SVM:")
    PerdMeasures(test_accounts, SVM_Linear_pred)
    print("The Radial SVM:")
    PerdMeasures(test_accounts, SVM_radial_pred)
    
    #AUC
    #roc(response, predictor)
    myRoc_DS =pROC::roc(test_accounts, DS_pred)
    myRoc_NB = pROC::roc(test_accounts, NB_pred)
    myRoc_SVML =pROC::roc(test_accounts, SVM_Linear_pred)
    myRoc_SVMR = pROC::roc(test_accounts, SVM_radial_pred)
  }
  else{
    myRoc_DS = multiclass.roc(test_accounts, DS_pred)
    myRoc_NB = multiclass.roc(test_accounts, NB_pred)
    myRoc_SVML = multiclass.roc(test_accounts, SVM_Linear_pred)
    myRoc_SVMR = multiclass.roc(test_accounts, SVM_radial_pred)
    
  }
  #print AUC
  print(paste( "AUC DS = ", myRoc_DS$auc))
  print(paste( "AUC NB = ", myRoc_NB$auc))
  print(paste( "AUC SVML = ", myRoc_SVML$auc))
  print(paste( "AUC SVMR = ", myRoc_SVMR$auc))
  
  
  # #res = ifelse(test_accounts == DS_pred, 1, 0))
  # pred = DS_pred
  # T = which(test_accounts==1)
  # TP = sum(ifelse(test_accounts[T] == pred[T], 1, 0))
  # FP = sum(ifelse(test_accounts[-T] == pred[-T], 1, 0))
  # FN = sum(ifelse(test_accounts[T] != pred[T], 1, 0))
  # 
  # #CM--> May be the answer for multi-class!!
  # x= table(test_accounts,pred,dnn=c("Obs","Pred"))
  # TP = sum(aq1)
}

factorToNum<- function(V){
  lvl = levels(V)
  n = length(lvl)
  
  for(i in 1:n)
    V = stringr::str_replace_all(V, lvl[i], as.character(i-1))

  V = as.numeric(V) #return
  
}
PerdMeasures <- function(Obs, Model_Pred){
  #CM--> May be the answer for multi-class!!
  CM = table(Obs,Model_Pred,dnn=c("Obs","Pred"))
  if(dim(CM)[1] > 2){
    print("Measures are not available for multi-class prediction")
    return()
  }
  TN = CM[1,1];  TP = CM[2,2]
  FP= CM[1,2];   FN = CM[2,1]
  
  #precision
  #What proportion of positive identifications was actually correct?
  P = TP/(TP+FP)
  #recall
  #What proportion of actual positives was identified correctly?
  R = TP/(TP+FN)
  #fmeasure
  F1 = (2*P *R)/(P+R)
  
  #print results..
  print("The CM matrix is...")
  print(CM)
  print(paste("The precision = ", P))
  print(paste("The recall = ", R))
  print(paste("The fmeasure = ", F1))
  
}
