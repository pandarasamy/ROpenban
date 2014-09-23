nnet.train<- function(data) {
  # Last column is the target variable
  # TODO: error handling
  # have a look at - ?https://github.com/openmhealth/dpu.ptsd/blob/master/R/correlation.R
  
  library(rjson)
  library(nnet)
  #library(neuralnet)
  
  json_obj = fromJSON(data)
  data_frame = data.frame(json_obj)
  
  attribs = names(json_obj) 
  len   = length(attribs)
  tvar  = attribs[len]    
  ivars = paste(attribs[1:len-1], collapse = " + ")
  
  formula_str = paste(tvar, ivars, sep = " ~ " )
  formula_obj = as.formula(formula_str)
  
  model = nnet(formula_obj, data=data_frame, size=2)
  # print(formula_str)
  # print(formula_obj)
  return (model)  
}

nnet.test<- function(data,model) {
  # Last column is the target variable
  # TODO: error handling
  # have a look at - ?https://github.com/openmhealth/dpu.ptsd/blob/master/R/correlation.R
  
  library(rjson)
  library(nnet)
  #library(neuralnet)
    
  json_obj = fromJSON(data)
  data_frame = data.frame(json_obj)
  
  predicted = predict(model, data_frame)
  #predicted = predict(model, newdata=data_frame)
  # print(formula_str)
  # print(formula_obj)
  return (data.frame(predicted))
}