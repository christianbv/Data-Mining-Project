# Name = Christian Venneroed - 45974465
# Oppgave 3 - classification

# 3.1) Loading in data and randomly partitions into 70% trainingdata and 30% testdata
loadData = function(filename){
  return(readRDS(filename))
}

# Partitioning:
partition_data <- function(totalData){
  sampleNumbers = sample(nrow(totalData),nrow(totalData)*0.7)
  trainingData = totalData[sampleNumbers,]
  testData = totalData[-sampleNumbers,]
  return(list(trainingData,testData))
}

# Function to normalize dataframe using minMax:
normalize = function(dataframe){
  return ((dataframe-min(dataframe)) / (max(dataframe)-min(dataframe)))
}

# 3.2) Learning a classification-tree using default parameters:
classifyDefault = function(trainingData,testData){
  library(party)
  set.seed(45)
  

  model <- ctree(Class~.,data = trainingData)

  jpeg(filename="Plot/task3_2.jpg",width=2000,height=2000,res=300)
  plot(model,type="simple")
  dev.off()
  
  print(model)
  pred = predict(model,testData)
  confusion = table(testData$Class,pred)
  print(confusion)
  
  cat("Accuracy: ",accuracy(confusion),"\n")
  cat("Precision: ",precision(confusion),"\n")
  cat("Recall: ", recall(confusion),"\n")
  cat("F-measure: ",f_measure(confusion),"\n")
}

#Helping methods:
accuracy <- function(x){
  sum(diag(x))/(sum(rowSums(x)))
}

recall <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}

precision <- function(x){
  x[2,2]/(x[2,2]+x[1,2])
}

f_measure <- function(x){
  2*(precision(x)*recall(x))/(precision(x)+recall(x))
}

# 3.3 ) Tuning classification-tree
# Bruke p-verdi - sannsynligheten for at dataen er manipulert randomly
classifyWithTuning <- function(trainingData,testData){
  library(party)
  library(Boruta)
  set.seed(45)
  
  # Finding important variables using the Boruta package
  boruta_train <- Boruta(Class~.,data = trainingData)
  print(boruta_train)

  print(nrow(trainingData))
  patients = nrow(subset(trainingData,Class ==1))
  nonPatients = nrow(subset(trainingData,Class==0))
  cat("Number of Patients:",patients," Number of non-patients:",nonPatients)
  
  model <- ctree(Class~Age+TB+Alkphos,data = trainingData, controls = ctree_control(mincriterion = 0.89,maxdepth = 3,minbucket = 20))
  
  
  jpeg(filename="Plot/task3_3.jpg",width=2000,height=2000,res=300)
  plot(model,type="simple")
  dev.off()
  
  # print(nodes(model,1))
  
  pred = predict(model,testData)
  confusion = table(testData$Class,pred)
  print(confusion)
  cat("Accuracy: ",accuracy(confusion),"\n")
  cat("Precision: ",precision(confusion),"\n")
  cat("Recall: ", recall(confusion),"\n")
  cat("F-measure: ",f_measure(confusion),"\n")
  
  
  
  return()
}

# 3.4) 
# Må endre gender til tall, female til 0 og male til 1 feks
k_nearestNeighbour <- function(totalData,k){
  set.seed(45)
  sampleNumbers = sample(nrow(totalData),nrow(totalData)*0.7)
  trainingData = totalData[sampleNumbers,]
  testData = totalData[-sampleNumbers,]
  
  
  target_category = totalData[sampleNumbers,11]
  test_category = totalData[-sampleNumbers,11]
  
  trainingData = subset(trainingData,select=-c(Class))
  testData = subset(testData,select=-c(Class))
  
  
  
  library(class)
  tree = knn(trainingData,testData,cl=target_category,k=k)
  confusion = table(test_category,tree)
  print(confusion)

  accuracy <- function(x){sum(diag(x))/(sum(rowSums(x)))}
  recall <- function(x){x[2,2]/(x[2,2]+x[1,2])}
  precision <- function(x){x[2,2]/(x[2,2]+x[2,1])}
  f_measure <- function(x){2*(precision(x)*recall(x))/(precision(x)+recall(x))}
  
  cat("Accuracy: ",accuracy(confusion),"\n")
  cat("Precision: ",precision(confusion),"\n")
  cat("Recall: ", recall(confusion),"\n")
  cat("F-measure: ",f_measure(confusion),"\n")
  
}



main <- function(){
  # 3.1) Preparing data: 
  df = loadData("ilpd_processed.Rda")
  df = within(df,Gender <- as.numeric(Gender)) # converting gender to numeric
  classCol = subset(df,select=c(Class))
  df = subset(df,select=-c(Class))             # removing class - factor
  df = as.data.frame(lapply(df,normalize))     # normalizing dataframe
  df = cbind(df,classCol)

  # Partitioning:
  dfs = partition_data(df)
  trainingData = as.data.frame(dfs[1])
  testData = as.data.frame(dfs[2])
  
  # 3.2) 
  #classifyDefault(trainingData,testData)
  
  # 3.3) 
  classifyWithTuning(trainingData,testData)

  # Task 3.4:
  for(j in 1:5){
    set.seed(45)
    #k_nearestNeighbour(df,j)
  }
}

main()
