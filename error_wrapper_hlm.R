### Wrapper function for calculated error from a list of regressions from different kinds of ENA models ----

error_wrapper_hlm = function(regressions.by.model) {

  error.list = list()
  names.list = c("ind","intra","inter")
  for(i in 1:length(regressions.by.model)){

    error = calc_error_hlm(regressions.by.model[[i]])
    error.list[[i]] = error
    names(error.list)[[i]] = names.list[[i]]

  }
  return(error.list)
}
