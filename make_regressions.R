### Function to create a regression models for each ENA set in a list -----


make_regressions = function(ena.list, formula) {
  
  reg.list = list()
  for (i in 1:length(ena.list)) {
    set = ena.list[[i]]
    dat = set$points
    reg = lm(formula = formula, data = dat)
    reg.list[[i]] = reg
  }
  return(reg.list)
}