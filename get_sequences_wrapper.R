### wrapper funciton for generating data sequence and line count per unit


get_sequences_wrapper = function(type,team_size,matrix_num,seq_length,soc_delta){
  mats = generate_sna_matrices(type = type,team_size = team_size,n = matrix_num,soc_delta = soc_delta)
  seqs = get_sequences(mat_list = mats,seq_length = seq_length,team_size = team_size)
  return(list(matrices = mats, sequences = seqs$sequences, counts = seqs$counts))
}
