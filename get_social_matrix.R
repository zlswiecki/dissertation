#function to get social probablity matrix for a given time. data sent in must have one column that uniquely identifies the units

get_social_matrix_old = function(dat,unit,window_size){

  newCols = unique(rENA:::merge_columns_c(dat,unit))
  dat_sna = recode.SNA.zs(dat, unit, newCols)
  cols_sna = c(unit,dat_sna$cols)
  # dat_sna_coded = data.frame(dat_sna$data[,..cols_sna])
  dat_sna_coded = data.frame(dat_sna$data[, c(cols_sna)])
  colnames(dat_sna_coded)[-length(unit)] = unique(dat_sna_coded[,unit])

  # coded = rowSums(dat[,codes])
  # coded = ifelse(coded > 0,1,0)

  blanks = matrix(rep(0,window_size*ncol(dat_sna_coded)),window_size,ncol(dat_sna_coded))
  colnames(blanks) = colnames(dat_sna_coded)

  unit_number = length(unique(dat_sna_coded[,unit]))

  codes_sna = unique(dat_sna_coded[,unit])

  # dat_sna_coded[,codes_sna] = dat_sna_coded[,codes_sna] * coded

  dat_sna_coded = rbind(blanks,dat_sna_coded)

  start_line = window_size + 1
  social_mat = matrix(rep(0,unit_number*unit_number),unit_number,unit_number)

  for (i in start_line:nrow(dat_sna_coded)){

    ref_line = dat_sna_coded[i,codes_sna]

    ref_window = dat_sna_coded[(i - window_size):(i - 1),codes_sna]

    window_sum = colSums(ref_window)

    # social_mat = as.numeric(ref_line) %o% as.numeric(window_sum) + social_mat

    social_mat_nonb = as.numeric(ref_line) %o% as.numeric(window_sum)

    social_mat_b = ifelse(social_mat_nonb >0,1,0) # converting to binary values...

    social_mat = social_mat_b + social_mat
  }

  colnames(social_mat) = codes_sna
  rownames(social_mat) = codes_sna

  social_mat = social_mat[order(rownames(social_mat)),order(colnames(social_mat))]

  sub_units = split(dat_sna_coded, dat_sna_coded[,unit]) #### i'm dividing by the wrong number here bc the units aren't in the same order

  unit_rows = map(sub_units,nrow)
  # remove_blank
  unit_rows = unit_rows[-1]

  social_mat_prob = social_mat/unlist(unit_rows)

  return(list(raw = social_mat,prob = social_mat_prob))

}

