### Function for spliting engineering dataset by site and subsetting the data by specific rooms --------------------


split_eng = function(dat, roomlist, by){
  
  split.dat = split(x = dat,f = by)
  
  split.dat.new = list()
  
  
  for(i in 1:length(split.dat)){
    
    sub = split.dat[[i]]$roomName %in% roomlist
    
    split.dat.new[[i]] = split.dat[[i]][sub,]
    
  }
  names(split.dat.new) = names(split.dat)
  return(split.dat.new)
}