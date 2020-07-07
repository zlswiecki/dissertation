#fuction for calculating the percentage of lines having each code combination

val_getter = function(n_list,matrix){
  val_list = list()
  val_list_full = list()
  # browser()

  diag_vals = diag(matrix)

  for(i in 1:length(n_list)){
    if(i == 1){
      val_list[[i]] = prod(diag_vals)
      val_list_full[[i]] = val_list
    }else{
      val_list = list()
      sub_list = n_list[[i]]
      for(j in 1:length(sub_list)){
        intersection = prod(diag_vals[sub_list[[j]]])
        for(k in 1:(i-1)){
          ss_list = n_list[[k]]
          for(l in 1:length(ss_list)){
            if(all(sub_list[[j]] %in% ss_list[[l]])){
              intersection = intersection - val_list_full[[k]][[l]]
            }else{
              intersection = intersection
            }
          }
        }
        val_list[[j]] = intersection
      }
      val_list_full[[i]] = val_list
    }
  }
  tot = union_prob(diag_vals)
  val_sum = sum(unlist(val_list_full))
  if(identical(round(tot,3),round(val_sum,3)) == FALSE){
    print(tot);print(tot);print("you fucked up")
  }
  return(val_list_full)
}
