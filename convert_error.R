### Function to convert errors for later analysis ---

convert_error = function(error.list){
  
  error.vec = data.frame(unlist(error.list))
  error.vec$method = row.names(error.vec)
  
  error.df = error.vec
  
  error.df = separate(data = error.vec,col = method,into = c("method","trash"),sep = "([\\d])",remove = T,extra = "drop")
  
  row.names(error.df) = NULL
  
  error.df$trash = NULL
  
  colnames(error.df) = c("errors","method")
  return(error.df)
}