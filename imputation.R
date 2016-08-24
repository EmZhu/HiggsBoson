library(dplyr)
library(mice)
library(missForest)
library(caret)
training=read.table('training.csv',header=TRUE,sep=',')

name_list=names(training)

name_list=name_list[-c(1,length(name_list))]

missing_frame=training[name_list]

head(missing_frame)

for (i in name_list)
{
  missing_frame[i][missing_frame[i]==-999.0]=NA
}

cross_val_frame=missing_frame[complete.cases(missing_frame),]

impute_frame=missing_frame[!(complete.cases(missing_frame)),]

error_list=c()

for (i in name_list)
{
  error_rate=nrow(impute_frame[is.na(missing_frame[i]),])/nrow(impute_frame)
  error_list=c(error_list,error_rate)
}

error_level=unique(error_list)

for (i in error_level)
{
  if (i!=0.0)
  {
    index=c()
    for (j in c(1:length(error_list)))
    {
      if (error_list[j]==i)
      {
        index=c(index,j)
      }
    }
    subframe=impute_frame[index]
    print (i)
    print (index)
    error=sum(complete.cases(subframe))/nrow(subframe)
   # print (head(subframe[complete.cases(subframe),]))
    
  }
}

index=c(1,24,25,26)
sum(complete.cases(impute_frame[index]))/nrow(impute_frame)

0.5493166/0.9756496

complete_rate=sum(complete.cases(missing_frame))/nrow(missing_frame)

m = 8
n = 3
mat = matrix(0, nrow = m, ncol = n)
result_dat = as.data.frame(mat)

for (i in c(1:3))
{
  new_cross=cross_val_frame
  temp_cross_val=new_cross
  cross_index=c((i-1)*as.integer(complete_rate*nrow(temp_cross_val)):(i)*as.integer(complete_rate*nrow(temp_cross_val)))
  valid_index=setdiff(c(1:nrow(temp_cross_val)),cross_index)
  sub_valid_1=sample(valid_index,as.integer(0.2095488*length(valid_index)))
  sub_valid_2=sample(valid_index,as.integer(0.9756496*length(valid_index)))
  sub_valid_3=sample(sub_valid_2,as.integer(0.5493166*length(valid_index)))
  temp_cross_val[sub_valid_1,1]=NA
  temp_cross_val[sub_valid_2,c(5,6,7,13,27,28,29)]=NA
  temp_cross_val[sub_valid_3,c(24,25,26)]=NA
  
  
  imputed_Data <- mice(temp_cross_val, m=5, maxit = 50, method = 'pmm')
  print ('aaa')
  completeData <- complete(imputed_Data,2)
  error_frame=abs((completeData-new_cross)/new_cross)
  error_sum=0
  for (j in names(error_frame))
  {
    print (j)
    error_sum=error_sum+sum(error_frame[[j]]*is.finite(error_frame[[j]]),na.rm = TRUE)/(nrow(error_frame)*ncol(error_frame))
  }
  error_sum=error_sum
  result_dat[1,i]=error_sum
  
  forest_Data=missForest(temp_cross_val)
  error_frame=abs((forest_Data$ximp-new_cross)/new_cross)
  error_sum=0
  for (j in names(error_frame))
  {
    error_sum=error_sum+sum(error_frame[[j]]*is.finite(error_frame[[j]]),na.rm = TRUE)/(nrow(error_frame)*ncol(error_frame))
  }
  error_sum=error_sum
  result_dat[2,i]=error_sum
  
  for (j in c(1:(as.integer(sqrt(ncol(temp_cross_val)))+1)))
  {
    imputed=preProcess(x=temp_cross_val,method='knnImpute',k=j)
    predict_frame=predict(imputed,temp_cross_val)
    error_frame=abs((predict_frame-new_cross)/new_cross)
    error_sum=0
    for (k in names(error_frame))
    {
      error_sum=error_sum+sum(error_frame[[k]]*is.finite(error_frame[[k]]),na.rm = TRUE)/(nrow(error_frame)*ncol(error_frame))
    }
    error_sum=error_sum
    result_dat[2+j,i]=error_sum
  }
}



