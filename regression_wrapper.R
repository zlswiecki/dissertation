### Wrapper function to make regression models for each type of ENA model

regression_wrapper = function(model.list, formula) {
  
  reg.names = c("ind","intra","inter")
  all.reg.list = list()
  for (j in 1:length(model.list)) {
    regs = make_regressions(model.list[[j]], formula)
    all.reg.list[[j]] = regs
    names(all.reg.list)[[j]] = reg.names[[j]]
  }
  
  return(all.reg.list)
  
}