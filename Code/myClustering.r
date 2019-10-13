# Name = Christian Venneroed - 45974465
# Task 2 - Clustering:

# Loading in dataframe: 
load_dataframe <- function(filename){
  df = readRDS(filename)
  
  # Checking if properly loaded:
  if(!class(df) == class(data.frame())){
    message("IKKE RIKTIG type, skal være dataframe")
    return()
  }else{
    return(df)
  }
}

# 2.1) Removing age, gender, Ag_ratio and class columns: 
remove_columns <- function(df){
  return(subset(df,select=-c(Age,Gender,AG_Ratio,Class)))
}

# 2.2) Rescaling all values in every column to the range(0,1):
normalize <- function(dataframe){
  return ((dataframe-min(dataframe)) / (max(dataframe)-min(dataframe)))
}

# 2.3) Clustering into k=2 clusters using k-means. Plotting in 2d plot with x = Alkphos and y = TP:
kmeans_two <- function(dataframe,init_df){
  # Must set seed since kmeans chooses initial centroids randomly in order to get reproducible results.
  set.seed(45)
  clusters = kmeans(dataframe,2)

  library(ggplot2)
  dataframe$cluster = factor(clusters$cluster)
  
  # Just saving the plot
  jpeg(filename="Plot/task2_3.jpg",height=2000,width=2000,res=300)
  
  a = ggplot(data=init_df,aes(x=Alkphos,y=TP,color=dataframe$cluster))
  b = geom_point()
  c = labs(color="Cluster:",title="Kmeans with k = 2 centroids:")
  plot(a+b+c)
  ggplot()
  
  dev.off()
}

# 2.4) Plotting again, this time coloring each point according to col class:
kmeans_two_colourized <- function(dataframe,init_df){
  set.seed(45) # Same as before.
  clusters = kmeans(dataframe,2)
  
  library(ggplot2)
  dataframe$cluster = factor(clusters$cluster)
  
  
  jpeg(filename="Plot/task2_4.jpeg",height=2000,width=2000,res=300)
  
  a = ggplot(data = init_df,aes(x=Alkphos,y=TP,color=Class))
  b = geom_point()
  c = labs(color="Class:",title="Plot with colour using k-means with k = 2: ")
  plot(a+b+c)
  
  dev.off()
}

# 2.5) Compare the plots from 2.3 and 2.4. Do they visually represent the patient vs non-patient classes?
kmeans_two_comparer <- function(dataframe,init_df){
  set.seed(45) # Same as before.
  clusters = kmeans(dataframe,2)
  
  library(ggplot2)
  dataframe$cluster = factor(clusters$cluster)
  
  
  jpeg(filename="Plot/task2_5.jpeg",height=2000,width=2000,res=300)
  
  a = ggplot(data = init_df,aes(x=Alkphos,y=TP,color=dataframe$cluster,shape=Class))
  b = geom_point()
  c = labs(color="Cluster:",title="Plot with colour using k-means with k = 2: ",shape="Actual class")
  plot(a+b+c)
  
  dev.off()
}

# 2.6 + 2.7) Clustering into more than 2 clusters and plotting the results. Also getting SSE: 
kmeans_generic <- function(df,k,init_df){
  set.seed(45)

  clusters = kmeans(df,k) 
  df$cluster = factor(clusters$cluster)
  cat("The SSE of this cluster using kmeans with k =",k,"is:",clusters$tot.withinss,"\n")
  
  string = paste("K-means with k =",k,"centroids: ")

  file = paste("Plot/task2_6_",k,".jpeg",sep="")
  jpeg(filename = file,height = 2000, width = 2000, res = 300)
  
  a = ggplot(data=init_df,aes(x=Alkphos,y=TP,color=df$cluster,shape=Class)) 
  b = geom_point()
  c = labs(title=string,color="Cluster:",shape="Actual Class:")
  plot(a+b+c)
  
  dev.off()
  
} 

# 2.7) From 2.6, we get the SSE when clustering using k-means with a given k.
sse_finder <- function(df){
  
  fn = "Plot/task2_7_SSE.jpeg"
  jpeg(filename =fn,height = 2000,width=2000,res = 300)
  library(factoextra)
  f = fviz_nbclust(df,kmeans,method="wss") # Plots SSE as k increases
  plot(f)
  
  dev.off()

  fn2 = "Plot/task2_7_Silhouette.jpeg"
  jpeg(filename = fn2, height= 2000, width = 2000, res = 300)
  f2 = fviz_nbclust(df,kmeans,method="silhouette") # Plots Silhouette as k increases
  plot(f2)
  
  dev.off()

}

# 2.8) Applying hierarchical clustering using hclust and plotting for k = (2,3,4,5), and validating:
hclust_generic <- function(dataframe,num_samples,init_df,k){
  set.seed(45)
  
  sampleNumbers <- sample.int(nrow(df),num_samples)
  sampleDataFrame <- dataframe[sampleNumbers,]
  init_sampleDataFrame <- init_df[sampleNumbers,]
  
  distanceMatrix <- dist(sampleDataFrame)
  
  h <- hclust(distanceMatrix,method=method)
  cut_avg <- cutree(h,k=k)

  # Comparing with ground truth:
  t = table(cut_avg,init_sampleDataFrame$Class)
  print(t)
  
  # Validating the clustering using external index:
  val = cluster_validation(init_sampleDataFrame,cut_avg,method,k)

  file = paste("Plot/task2_8_",k,".jpeg",sep="")
  jpeg(filename = file,width = 2000, height = 2000, res = 300)
  
  plot(h,main=val,cex=0.6)
  rect.hclust(h,k=k,border=2:4)
  dev.off()
  return(subset(as.data.frame(cut_avg),cut_avg == 1)) # for 2.9)
}

# 2.9) Interesting plot (x = DB, y = TB) and in hclust!
kmeans_subtype <- function(dataframe, init_df){
  # Must set seed since kmeans chooses initial centroids randomly in order to get reproducible results.
  set.seed(45)
  clusters = kmeans(dataframe,3)
  
  library(ggplot2)
  dataframe$cluster = factor(clusters$cluster)
  
  # Just saving the plot
  jpeg(filename="Plot/task2_9.jpg",height=2000,width=2000,res=300)
  
  a = ggplot(data=init_df,aes(x=TB,y=DB,color=dataframe$cluster,shape=Class))
  b = geom_point()
  c = labs(color="Cluster:",title="Kmeans with k = 3 centroids:",shape="Class")
  plot(a+b+c)
  ggplot()
  
  dev.off()
  
  s = clusters$cluster
  res = as.data.frame(s)
  res <- subset(res,s == 1)
  return(res)
}

compareRows <- function(df1,df2){
  a = as.numeric(row.names(df1))
  b = as.numeric(row.names(df2))
  t = intersect(a,b)
  print(length(t)) # 34 instances! Subtype??
}

# 2.10) Testing different methods and validating:
hclust_generic_method <- function(dataframe,num_samples,init_df,k,method){
  set.seed(45)
  
  sampleNumbers <- sample.int(nrow(df),num_samples)
  sampleDataFrame <- dataframe[sampleNumbers,]
  init_sampleDataFrame <- init_df[sampleNumbers,]
  
  distanceMatrix <- dist(sampleDataFrame)
  
  h <- hclust(distanceMatrix,method=method)
  cut_avg <- cutree(h,k=k)
  
  # Comparing with ground truth:
  t = table(cut_avg,init_sampleDataFrame$Class)
  print(t)
  
  # Validating the clustering using external index:
  val = cluster_validation(init_sampleDataFrame,cut_avg,method,k)
  
  
  file = paste("Plot/task2_10_",method,".jpeg",sep="")
  jpeg(filename = file,width = 2000, height = 2000, res = 300)
  
  plot(h,hang = -0.1,cex=0.1,main=val)
  rect.hclust(h,k=k,border=2:4)
  dev.off()
  
}

# Help-method for 2.8 and 2.10)
cluster_validation <- function(init_df,cut,metaMethod,metaK){
  ss = 0
  dd = 0
  sd = 0
  ds = 0
  
  # Combining clusterId with actual Clusters:
  result_clusters = as.data.frame(cut)
  res_df = as.data.frame(cbind(init_df$Class,result_clusters))
  
  # Iterating through res_df, creating incidence matrix and incrementing ss,dd etc:
  for(i in 1:nrow(res_df)){
    colId = res_df[i,1]
    clusterId = res_df[i,2]
    
    for(j in (i+1):nrow(res_df)-1){
      compare_ColId = res_df[j,1]
      compare_clusterId = res_df[j,2]
      
      # Incrementing different values for ss,dd,sd and ds: 
      p = (colId == compare_ColId)
      c = (clusterId == compare_clusterId)
      
      if(p & c){
        ss = ss +1
      }else if(!p & !c){
        dd = dd +1
      }else if(c & !p){
        sd = sd + 1
      }else{
        ds = ds +1
      }
    }
  }

  
  # Finding purity of cluster:
  accuracy_table = table(cbind(result_clusters,init_df$Class))
  purity = 0
  for(j in 1:nrow(accuracy_table)){
    purity = purity + max(accuracy_table[j,1],accuracy_table[j,2])
  }

  # Hence, we have computed Rand-index, Jaccard and Purity for this cluster: 
  rand = signif((ss+dd)/(ss+dd+sd+ds), digits = 5)
  jaccard = signif((ss)/(ss+ds+sd),digits = 5)
  purity = signif(purity/sum(accuracy_table), digits = 5)
  
  #cat("Rand-index of clustering with",metaK,"clusters using the",metaMethod,"method is:",rand,"\n")
  #cat("Jaccard-index of clustering with",metaK,"clusters using the",metaMethod,"method is:",jaccard,"\n")
  #cat("Purity of clustering with",metaK,"clusters using the",metaMethod,"method is:",purity,"\n")
  #print(" ")
  
  r1 = c(paste("Cluster Dendrogram with method:",metaMethod,"\n"),paste("Rand-index:",rand," Jaccard:",jaccard),paste("\n Purity:",purity))
  r2 = c(paste("Cluster Dendrogram \n"),paste("Rand-index:",rand," Jaccard:",jaccard),paste("\n Purity:",purity))
  
  return(r2)
}

main <- function(){
  init_df = load_dataframe("Data/ilpd_processed.Rda")
  df = remove_columns(init_df)
  norm_df = as.data.frame(lapply(df,normalize))
  
  # Task 2.3)
  kmeans_two(norm_df,init_df)
  
  # Task 2.4)
  kmeans_two_colourized(norm_df,init_df)
  
  # Task 2.5)
  kmeans_two_comparer(norm_df,init_df)
  
  # Task 2.6)
  for(i in 3:5){
    kmeans_generic(norm_df,i,init_df)
  }

  # Task 2.7) 
  sse_finder(norm_df)

  
  # Task 2.8:
  hclust_generic(norm_df,nrow(init_df),init_df,2) # Using entire dataset - slow.
  for (i in 2:5){
    hclust_generic(norm_df,100,init_df,i)  # Sampling with 100 samples
  }

  # Task 2.9:
  resDf1 = kmeans_subtype(norm_df,init_df)
  resDf2 = hclust_generic(norm_df,nrow(init_df),init_df,3)
  compareRows(resDf1,resDf2)
  
   # Task 2.10:
  hclust_generic_method(norm_df,100,init_df,2,"average")
  hclust_generic_method(norm_df,100,init_df,2,"single")
  hclust_generic_method(norm_df,100,init_df,2,"complete")
}

main()











