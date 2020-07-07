### Function to test for differences between error distributions ----

### TO DO: ADD IN CHECK FOR NORMALITY OF ERROR DISTRIBUTIONS

### CHECK ON EFFECT SIZE CALCULATION. RESULTS SEEM REALLY LARGE

test_errors = function(errors) {
  
  methodNum = length(unique(errors$method))
  
  results = matrix(data = NA,nrow = choose(methodNum,2),ncol = 9)
  
  combinations = t(combn(x = unique(errors$method),m = 2))
  
  results[,1:2] = combinations
  
  conf = 1 - (0.05/methodNum)
  
  for(i in 1:nrow(results)){
    
    test = t.test(x = errors$errors[errors$method == results[i,1]],y = errors$errors[errors$method == results[i,2]],paired = T,conf.level = conf)
    eff.size = cohensD(x = errors$errors[errors$method == results[i,1]],y = errors$errors[errors$method == results[i,2]],method = "paired")
    
    results[i,3] = round(mean(errors$errors[errors$method == results[i,1]]),digits = 2)
    results[i,4] = round(mean(errors$errors[errors$method == results[i,2]]),2)
    results[i,5] = round(test$estimate,2)
    results[i,6] = round(test$conf.int[[1]],2)
    results[i,7] = round(test$conf.int[[2]],2)
    results[i,8] = ifelse(test = sum(sign(as.numeric(results[i,6])),sign(as.numeric(results[i,7]))) == 0,yes = "Not.Sig",no = "Sig.")
    results[i,9] = round(eff.size,digits = 2)
    
  }
  
  results = data.frame(results)
  colnames(results) = c("Method1","Method2","Method1_Mean","Method2_Mean","Difference","Confidence_Interval_Lower","Confidence_Interval_Upper", "Significant", "Eff.Size")
  # ena = error.df[error.df$method == "ena",]
  return(results)
}
