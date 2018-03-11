mass.shootings.data_clean <- function(rawdata){
  # Select the feature we use (which are not text information)
  feature = c("Fatalities","Injured","Total.victims","Mental.Health.Issues","Race","Gender","Latitude","Longitude")
  data = rawdata[,feature]
  
  #Transform the Date feature
  Month = sapply(rawdata$Date,function(x){strsplit(x,"/")[[1]][1]})
  Day = sapply(rawdata$Date,function(x){strsplit(x,"/")[[1]][2]})
  Year = sapply(rawdata$Date,function(x){strsplit(x,"/")[[1]][3]})
  data = cbind(data,Year,Month,Day)
  
  # Changing "Mental.Health.Issues", "Race", "Gender" columns' classes to factors:
  cols_to_factors<-c("Mental.Health.Issues", "Race", "Gender")
  data[,cols_to_factors]<-lapply(data[,cols_to_factors], as.factor)
  
  #Standardizing levels for factors "Gender", "Race" and "Mental Health Issues": Gender:
  
  #Gender
  #levels(data$Gender)
  levels(data$Gender)[levels(data$Gender)=="M"] <- "Male"
  levels(data$Gender)[levels(data$Gender)=="M/F"] <- "Male/Female"
  levels(data$Gender)
  #sum(table(data$Gender))==nrow(data)
  #plot(data$Gender)
  
  #Race
  #levels(data$Race)
  levels(data$Race)[levels(data$Race)==""]<-"Unknown"
  levels(data$Race)[levels(data$Race)=="black"|levels(data$Race)=="Black"
                    |levels(data$Race)=="Black American or African American/Unknown"]<-"Black American or African American"
  levels(data$Race)[levels(data$Race)=="white"|levels(data$Race)=="White"]<-"White American or European American"
  levels(data$Race)[levels(data$Race)=="Some other race"]<-"Other"
  levels(data$Race)[levels(data$Race)=="Asian"
                    |levels(data$Race)=="Asian American"
                    |levels(data$Race)=="Asian American/Some other race"]<-"Asian or Asian American"
  levels(data$Race)[levels(data$Race)=="White American or European American/Some other Race"]<-"White American or European American"
  levels(data$Race)[levels(data$Race)=="Two or more races"]<-"Other"
  
  #table(data$Race)
  
  #"Mental Health Issues"
  #evels(data$Mental.Health.Issues)
  levels(data$Mental.Health.Issues)[levels(data$Mental.Health.Issues)=="Unknown"
                                    |levels(data$Mental.Health.Issues)=="Unclear"
                                    |levels(data$Mental.Health.Issues)=="unknown"]<-"Unknown"
  #levels(data$Mental.Health.Issues)
  #table(data$Mental.Health.Issues)
  #plot(data$Mental.Health.Issues)
  return(data)
}

mass.shootings.missing_value <- function(rawdata){
  data = rawdata
  levels(data$Race)[levels(data$Race)=="Unknown"]<-NA
  levels(data$Mental.Health.Issues)[levels(data$Mental.Health.Issues)=="Unknown"]<-NA
  levels(data$Gender)[levels(data$Gender)=="Unknown"]<-NA
  return(data)
}

# Function SVM_CV
# Usage: SVM_CV(X,Y,C,kernel,nfold)
# X: Input matrix, each row is an observation vector 
# Y: Response variable
# C: a sequence of tuning parameters for "cost"
# kernel: a sequence of kernel
# nfold: the fold of cross validation
# Output(result): A matrix of the sequence of tuning parameters
#                 the corresponding cross validation accurracy
SVM_CV <-function(X,Y,C,kernel,nfold){
  # Reorder the data
  random_index = sample(dim(X)[1])
  X = X[random_index,]
  Y = Y[random_index]
  cv_acc = matrix(NA,length(kernel),length(C))
  p = dim(X)[2]
  for(m in 1:length(C)){
    for(n in 1:length(kernel)){
      acc = rep(NA,nfold)
      size = floor(dim(X)[1]/nfold)
      for(j in 1:nfold){
        # Split the data into train and validation part
        index = ((j-1)*size+1):(j*size)
        train_x = X[-index,]
        train_y = Y[-index]
        train_y_factor = matrix(as.factor(train_y))
        validation_x = X[index,]
        validation_y = Y[index]
        # Fit the model with e1071 package
        svm.fit <- svm(train_y ~ ., data = data.frame(train_x,train_y), 
                       type='C-classification', kernel=kernel[n],
                       scale=FALSE, cost = C[m])
        # Calculate the accurracy on the validation data with e1071 package
        acc[j] =  sum(predict(svm.fit,validation_x) == 
                            validation_y) / length(validation_y)
      }
      # Calculate the cross validation accurracy
      cv_acc[n,m] = mean(acc)
    }
  }
  rownames(cv_acc) = kernel
  colnames(cv_acc) = C
  return(cv_acc)
}

# Function confusion_matrix
# Usage: confusion_matrix(y,svm_pred_y)
# y: The truth y value
# svm_pred_y: Predict value of y
# Output(result): A list of confusion matrix,specificity,sensitivity

confusion_matrix<-function(y,svm_pred_y){
  TP = sum(y[svm_pred_y=="Yes"]=="Yes")
  TN = sum(y[svm_pred_y== "No"]== "No")
  FP = sum(y[svm_pred_y=="Yes"]== "No")
  FN = sum(y[svm_pred_y=="No"]=="Yes")
  sensitivity = TP/(TP+FN)
  specificity = TN/(TN+FP)
  sensitivity
  specificity
  result = matrix(c(TP,FN,FP,TN),2,2)
  rownames(result) = c("Predicted condition positive","Predicted condition negative")
  colnames(result) = c("Condition positive","Condition negative")
  return(list(result = result,specificity= specificity,sensitivity= sensitivity))
}