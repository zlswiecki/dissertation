#### updated sequence function

### needs to take in a list of matrices and output a list of sequences and line counts for each unit

get_sequences = function(mat_list, seq_length,team_size){  ### seq_lenght equal the number of lines I want each team to have.... (could randomize later)

  seq_list = list()
  count_list = list()

  count = 0

  while(count < length(mat_list)){  ### update later to handle lists...

    mat = mat_list[[1]]
    # browser()
    statesNames =  as.character(seq.int(ncol(mat)))
    mark_mat = new("markovchain", states = statesNames,transitionMatrix = mat)
    seqs_list = list()
    for(i in 1:team_size){
      seq_ = markovchainSequence(n = seq_length/team_size, markovchain = mark_mat, t0 = statesNames[i],include.t0 = TRUE)
      seqs_list[[i]] = as.numeric(seq_)
    }
    seqs_ = unlist(seqs_list)
    counts = as.list(table(seqs_))
    if(length(counts) == nrow(mat)){
      seq_list[[count + 1]] = seqs_
      count_list[[count + 1]] = counts
      count = count + 1
    }else{
      seq_list[[count + 1]] = NULL
      count_list[[count + 1]] = NULL
    }
    seq_list = compact(seq_list)
    count_list = compact(count_list)
  }
  return(list(sequences = seq_list, counts = count_list))
}

