
inter_by_team <- function(B, S, C,check = FALSE, norm = TRUE) {
	code_names <- colnames(C[[1]])

	# browser()


	I <- array(dim = c(length(code_names),length(code_names),nrow(B)))

	units <- rownames(S)

	for( u in seq(units)) {
		for( r in seq(code_names)) {
			for ( s in seq(code_names)) {
				unit_cell <- B[u, r]
				co_cell <- C[[u]][r, s]
				send_col <- B[, r]
				soc_row <- S[u, ]

				parts <- c()
				for( i in seq(send_col)) {
				  if(check){
				    res <- 1 - ( unit_cell * (( 1 - ( 1 - send_col[i])) ^ soc_row[i]) ) ### may be inflating here
				    parts <- c(parts, res)
				  }else{
				    res <- 1 - ( unit_cell * send_col[i] * soc_row[i] ) ### removing the effect of being in window more than once
				    parts <- c(parts, res)
				  }
				}

				I[r, s, u] <- 1 - ( prod(parts) * ( 1 - co_cell ) )
			}
		}
	}

	V <- matrix(0, nrow = length(units), ncol = choose(length(code_names), 2))
	indices <- rENA:::triIndices(length(code_names)) + 1

	for ( u in seq(units)) {
		for ( ind in seq(ncol(indices)) ) {
			i <- indices[, ind]
			#V[u, ind] <- I[ i[1], i[2], u] + I[ i[2], i[1], u]
			V[u, ind] <- sum_probs(e1 = (I[ i[1], i[2], u]), e2 = (I[ i[2], i[1], u])) ### need to check this output
		}
	}

	if(norm){
	  vnorm <- rENA:::fun_sphere_norm(V)
	}else{
	  vnorm = V
	}

	rownames(V) = units
	rownames(vnorm) = units

	return(list(
		matrices = I,
		vectors = V,
		vectors_normed = vnorm
	))
}


