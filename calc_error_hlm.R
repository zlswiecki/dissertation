### Function to calculate error between regression predictions and gold standard when using hlm----

calc_error_hlm = function(regression.list){
  
  loss_vec = c()  
  for (i in 1:length(regression.list)){
    se = resid(regression.list[[i]])^2
    sse = sum(se)
    mse = sse/length(se)
    loss_vec[i] = sse ### FIND OUT IF YOU SHOULD USE SSE OR MSE
  }
  return(loss = loss_vec)
}


