deflator = function(mat, codes, lines, baserates = TRUE){
  names_list = list()
  for (i in seq.int(codes)){
    names_list[[i]] = combn(length(codes),i,simplify = FALSE)
  }
  names_list = rev(names_list)
  val_list_names = flatten(names_list)
  # browser()
  all_vals = val_getter(n_list = names_list,matrix = mat)
  all_vals = unlist(all_vals)
  # browser()
  all_lines <- lapply(seq(length(all_vals)), function(m) {
    if(baserates) {
      rows <- as.integer(lines * all_vals[m], 0)
    } else {
      rows <- all_vals[m]
    }
    # browser() #expr = { l == 6 })
    if(rows > 0) {
      df <- data.frame(matrix(0, ncol=ncol(mat), nrow=rows, dimnames=list(NULL, codes)))
      df[, val_list_names[[m]]] = 1
      df
    }
  })
  count = 0
  if(every(all_lines,is.null)){   #### figure out how to just set all rows equal to 0
    # print("all_lines is NULL, setting lines to zero")
    count = count + 1
    df <- data.frame(matrix(0, ncol=ncol(mat), nrow=lines, dimnames=list(NULL, codes)))
    df[, val_list_names[[1]]] = 0
    df
    all_lines = df
  }
  if(is.data.frame(all_lines) == FALSE){
    unit_lines = do.call(rbind, all_lines)
  }else{
    unit_lines = all_lines
  }
  n_unit_lines = nrow(unit_lines)
  missing_lines = lines - n_unit_lines

  if(missing_lines > 0){
    vec = rep(0,ncol(unit_lines))
    zero_rows = t(replicate(missing_lines,vec))
    colnames(zero_rows) = colnames(unit_lines)
    unit_lines = rbind(unit_lines,zero_rows)
  }
  # if(count > 0){
  #   print(paste(count," ","units with no coded lines:"))
  # }
  # browser()
  return(list(unit_lines = unit_lines,zero_count = count))
}
