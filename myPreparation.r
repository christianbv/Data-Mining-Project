# Name = Christian Venneroed - 45974465
# Task 1:


# Task 1.1) - Extracting data into R-frame:
fileReader <- function(filename){
  df = read.csv(filename,header=FALSE)
  if(!is.data.frame(df)){
    message("WARNING - rading of file failed.")
    return()
  }
  return(df)
}

# Task 1.2) - Changing the column names:
change_col_names <- function(df){
  newNames = c("Age","Gender","TB","DB","Alkphos","Sgpt","Sgot","TP","Albumin","AG_Ratio","Class")
  i = 1
  for(name in newNames){
    string = paste("V",i,sep="")
    names(df)[names(df)==string] = name
    i = i+1
  }
  return(df)
}

# Task 1.3) - Handling missing values in colum AG_ratio, replacing with mean
fix_AG_RatioCol <- function(df){
  # Computes the median of col AG_Ratio, excluding all NA-values
  medii = median(df$AG_Ratio,na.rm=TRUE) 

  # Replaces all NA values with the median
  df$AG_Ratio[is.na(df$AG_Ratio)]=medii   
  
  return(df)
}

# 1.4) - Replace all 2's in col Class with 0, indicating Non-patient:
fix_ClassCol <- function(df){
  df$Class[df$Class==2]=0
  return(df)
}

# Task 1.5) - Changing type of column Class to Factor:
change_class_type <- function(df){
  df$Class = as.factor(df$Class)
  
  # Checking that conversion happened succesfully:
  if(class(df$Class) != class(factor())){ 
    message("Conversion of column class from numeric to factor failed...")
    return()
  }else{
    return(df)
  }
}

# Task 1.6) - Saving the dataframe into the file "ilpd_processed.Rda":
save_data_frame <- function(df){
  processedFileName = "Data/ilpd_processed.Rda"
  saveRDS(df,processedFileName)
  return(message("File saved successfully!"))
}


filename = "Data/Indian_Liver_Patient_Dataset_(ILPD).csv"
dataframe = fileReader(filename)
dataframe = change_col_names(dataframe)
dataframe = fix_AG_RatioCol(dataframe)
dataframe = fix_ClassCol(dataframe)
dataframe = change_class_type(dataframe)
dataframe = save_data_frame(dataframe)



