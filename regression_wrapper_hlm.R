### Wrapper function to make hlm regression models for each type of ENA model

regression_wrapper_hlm = function(model.list, formula) {

  reg.names = c("ind","intra","inter")
  all.reg.list = list()
  for (j in 1:length(model.list)) {
    regs = make_regressions_glm_hlm(model.list[[j]], formula,reg.type = "hlm")
    all.reg.list[[j]] = regs
    names(all.reg.list)[[j]] = reg.names[[j]]
  }

  return(all.reg.list)

}
