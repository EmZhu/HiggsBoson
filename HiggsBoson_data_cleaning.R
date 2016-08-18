#--------------------------------------------
# load data
#--------------------------------------------
setwd('~/Google Drive/DataScienceAcademy/Projects/Project4Kaggle')
data = read.csv('training.csv', header = T)

#--------------------------------------------
# EDA
#--------------------------------------------
str(data)
### 250,000 observations 
### 33 variables -- all variables are numerical except 'PRI_jet_num'(integer), 'EventId'(Integer), 'Label'(Factor) 
head(data)

#--------------------------------------------
# find missing data (i.e. any -999.0 values)
#--------------------------------------------
library(VIM)

# convert -999 to NA
data[data == -999] = NA

# try to visualize missingness in data
md.pattern(data)
### not helpful because column names are not shown

# find which columns have missing values
missing_columns = names(data)[which(sapply(data, function(x)any(is.na(x))) == T)]
missing_columns
### total 11 columns -- all of numeric type

# #--------------------------------------------
# # Try to impute missing values
# #--------------------------------------------
# # visualize distribution of non-missing values in columns with missing values
# n_missing = length(missing_columns)
# for (i in 1:n_missing) {
#   hist(data[, missing_columns[i]], main = paste('Histogram of ', missing_columns[i]))
# }
# 
# # try impute using knn 
# k = sqrt(nrow(data))
# train_knn = kNN(data, k = k)

#--------------------------------------------
# Create validation set
#--------------------------------------------
# randomly sample 80% of 'data' as training and 20% as validation
# set.seed(10)
# train_index = sample(1:nrow(data), 8*nrow(data)/10)
# train = data[train_index, ]
# validation = data[-train_index, ]
# 
# # save training and validation to file
# write.csv(train, 'train_data.csv', row.names=F)
# write.csv(validation, 'validation_data.csv', row.names=F)


#--------------------------------------------
# Try models
#--------------------------------------------







