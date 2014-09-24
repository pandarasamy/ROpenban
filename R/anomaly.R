anomaly.train<- function(data) {
  # Last column is the target variable
  # TODO: error handling
  # have a look at - ?https://github.com/openmhealth/dpu.ptsd/blob/master/R/correlation.R
  
  #reference : http://ww2.coastal.edu/kingw/statistics/R-tutorials/logistic.html
  
  library(rjson)
  library(stats)
  
  json_obj = fromJSON(data)
  data_frame = data.frame(json_obj)
  
  attribs = names(json_obj) 
  len   = length(attribs)
  
  # print(formula_str)
  # print(formula_obj)
  return (data_frame)  
}

anomaly.test<- function(data,model) {
  # Last column is the target variable
  # TODO: error handling
  # have a look at - ?https://github.com/openmhealth/dpu.ptsd/blob/master/R/correlation.R
  
  library(rjson)
  library(party)
  
  json_obj = fromJSON(data)
  data_frame = data.frame(json_obj)
  
  predicted = predict(model, newdata=data_frame)
  # print(formula_str)
  # print(formula_obj)
  return (data.frame(predicted))
}
