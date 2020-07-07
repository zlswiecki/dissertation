### function for randomizing data by team (or conversation)


randomize_by_columns = function(og_data, columns){
  
  # set.seed(100)
  split_dat = split(og_data, og_data[,columns])
  dat_list = list()
  
  for (i in 1:length(split_dat)){
    
    dat = split_dat[[i]]
    dat_ran_rows = sample(nrow(dat))
    dat = dat[dat_ran_rows,]
    dat_list[[i]] = dat
  }
  
  random_dat = bind_rows(dat_list)
  return(random_dat)
}



