#### updated sequence function

### needs to take in a list of matrices and output a list of sequences and line counts for each unit

get_sequences_new = function(mat_list, seq_length){  ### seq_lenght equal the number of lines I want each team to have.... (could randomize later)

  seq_list = list()
  count_list = list()

  count = 0

  while(count < length(mat_list)){  ### update later to handle lists...

    mat = mat_list[[1]]
    # browser()
    statesNames =  as.character(seq.int(ncol(mat)))
    mark_mat = new("markovchain", states = statesNames,transitionMatrix = mat)
    seq_ = markovchainSequence(n = seq_length, markovchain = mark_mat, t0 = sample(statesNames,1,FALSE))
    seq_ = as.numeric(seq_)
    counts = as.list(table(seq_))
    if(length(counts) == nrow(mat)){
      seq_list[[count + 1]] = seq_
      count_list[[count + 1]] = counts
      count = count + 1
    }else{

      seq_list[[count + 1]] = seq_
      missing_units = which(!statesNames %in% names(counts))
      counts = append(counts,rep(0,length(missing_units)))
      names(counts)[-1] = missing_units
      counts = counts[order(names(counts))]
      count_list[[count + 1]] = counts
      count = count + 1
    }
    seq_list = compact(seq_list) #compact prob not needed anymore
    count_list = compact(count_list)
  }
  return(list(sequences = seq_list, counts = count_list))
}

