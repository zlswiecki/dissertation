### Generate ind, intra, and inter ena models for TADMUS and NTX datasets


make_ena_to_compare_real_2 = function(resamples,
                                      codeNames,
                                      units,
                                      convo.ind,
                                      convo.intra,
                                      convo.inter,
                                      window.size.inter,
                                      window.size.intra,
                                      meta,
                                      context,
                                      proj_set,
                                      centered = TRUE
){

  ind.ena = list()
  intra.ena = list()
  inter.ena = list()

  for (i in 1:length(resamples)){

    dat = resamples[[i]]

    if(context == "TADMUS"){

      ind = make_ena_tadmus(df = dat,
                            codeCols = codeNames,
                            unitCols = units,
                            convoCols = convo.ind,
                            metaCols = meta,
                            # windowType = "Conversation",
                            windowType = "MovingStanzaWindow",
                            windowSizeFor = 0,
                            windowSizeBack = 1,
                            meansRotate = FALSE,
                            projSet = proj_set,
                            removeOther = TRUE)

      intra = make_ena_tadmus(df = dat,
                              codeCols = codeNames,
                              unitCols = units,
                              convoCols = convo.intra,
                              metaCols = meta,
                              windowType = "MovingStanzaWindow",
                              windowSizeFor = 0,
                              windowSizeBack = window.size.intra,
                              meansRotate = FALSE,
                              projSet = proj_set,
                              removeOther = TRUE)

      inter = make_ena_tadmus(df = dat,
                              codeCols = codeNames,
                              unitCols = units,
                              convoCols = convo.inter,
                              metaCols = meta,
                              windowType = "MovingStanzaWindow",
                              windowSizeFor = 0,
                              windowSizeBack = window.size.inter,
                              meansRotate = FALSE,
                              projSet = proj_set,
                              removeOther = TRUE)
    }else{

      ind = make_ena_ntx(df = dat,
                         codeCols = codeNames,
                         unitCols = units,
                         convoCols = convo.ind,
                         metaCols = meta,
                         # windowType = "Conversation",
                         windowType = "MovingStanzaWindow",
                         windowSizeFor = 0,
                         windowSizeBack = 1,
                         meansRotate = FALSE,
                         projSet = proj_set)

      intra = make_ena_ntx(df = dat,
                           codeCols = codeNames,
                           unitCols = units,
                           convoCols = convo.intra,
                           metaCols = meta,
                           windowType = "MovingStanzaWindow",
                           windowSizeFor = 0,
                           windowSizeBack = window.size.intra,
                           meansRotate = FALSE,
                           projSet = proj_set)

      inter = make_ena_ntx(df = dat,
                           codeCols = codeNames,
                           unitCols = units,
                           convoCols = convo.inter,
                           metaCols = meta,
                           windowType = "MovingStanzaWindow",
                           windowSizeFor = 0,
                           windowSizeBack = window.size.inter,
                           meansRotate = FALSE,
                           projSet = proj_set)

    }

    # browser()

    if(centered == TRUE){
      ind.ena[[i]] = ind
      intra.ena[[i]] = intra
      inter.ena[[i]] = inter
    }else{

      ind_raw = ind$connection.counts
      # ind_raw = ind_raw[order(ind_raw$ENA_UNIT), ]
      unit_names = ind_raw$ENA_UNIT
      ind_raw = as.matrix(ind_raw)

      intra_raw = intra$connection.counts
      # intra_raw = intra_raw[order(intra_raw$ENA_UNIT), ]
      intra_raw = as.matrix(intra_raw)

      inter_raw = inter$connection.counts
      # inter_raw = inter_raw[order(inter_raw$ENA_UNIT), ]
      inter_raw = as.matrix(inter_raw)

      ind_norm = rENA:::fun_sphere_norm(ind_raw)
      intra_norm = rENA:::fun_sphere_norm(intra_raw)
      inter_norm = rENA:::fun_sphere_norm(inter_raw)

      ind.ena[[i]] = ind_norm
      intra.ena[[i]] = intra_norm
      inter.ena[[i]] = inter_norm
    }
  }
  return(list(ind.ena = ind.ena, intra.ena = intra.ena, inter.ena = inter.ena, unit_names = unit_names))
}

