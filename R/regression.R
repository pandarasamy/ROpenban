regression.train<- function(data) {
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
  tvar  = attribs[len]    
  ivars = paste(attribs[1:len-1], collapse = " + ")
  
  formula_str = paste(tvar, ivars, sep = " ~ " )
  formula_obj = as.formula(formula_str)
  
  model = glm(formula_obj, data=data_frame)
  # print(formula_str)
  # print(formula_obj)
  return (model)  
}

regression.test<- function(data,model) {
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

