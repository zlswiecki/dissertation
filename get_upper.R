get_upper = function(code_mats){
  upper_list_all = list()
  #browser()
  for(i in 1:length(code_mats)){
    upper_list_team = list()
    for(j in seq.int(length(code_mats[[i]]))){
      upper = code_mats[[i]][[j]][upper.tri(code_mats[[i]][[j]])]
      upper_list_team[[j]] = upper
    }
    upper_list_all[[i]] = upper_list_team
  }
  return(upper_list_all)
}
