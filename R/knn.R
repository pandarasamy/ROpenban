knn.train<- function(data) {
  # just store the data and training data
  
  library(rjson)
  
  json_obj = fromJSON(data)
  train = data.frame(json_obj)
  
  return (train)  
}

knn.test<- function(data,model) {
  # Model contains the actual training data frame
  # Last column is the target variable
  # TODO: error handling
  # have a look at - ?https://github.com/openmhealth/dpu.ptsd/blob/master/R/correlation.R
  
  library(rjson)
  library(class)
  
  json_obj = fromJSON(data)
  test = data.frame(json_obj)
  
  # TODO: remove this
  len   = length(test)  
  test = test[,1:len-1]
    
  # model is a training data frame
  len   = length(model)  
  train = model[,1:len-1]
  gt    = model[,len]
  
  result = knn(train, test, gt, k=3, prob=TRUE )
  return (data.frame(result))
}