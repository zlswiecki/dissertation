### function for calculating normed adjacency vectors from real data

get_normed_adj_real = function(data_,
                              units,
                              convo.ind,
                              convo.intra,
                              convo.inter,
                              window.size.intra,
                              window.size.inter,
                              codes,
                              meta,
                              context){






  if(context == "TADMUS"){

    ind = make_ena_tadmus(df = data_,
                          codeCols = codes,
                          unitCols = units,
                          convoCols = convo.ind,
                          metaCols = meta,
                          # windowType = "Conversation",
                          windowType = "MovingStanzaWindow",
                          windowSizeFor = 0,
                          windowSizeBack = 1,
                          meansRotate = FALSE,
                          projSet = NULL,
                          removeOther = FALSE)


    intra = make_ena_tadmus(df = data_,
                            codeCols = codes,
                            unitCols = units,
                            convoCols = convo.intra,
                            metaCols = meta,
                            windowType = "MovingStanzaWindow",
                            windowSizeFor = 0,
                            windowSizeBack = window.size.intra,
                            meansRotate = FALSE,
                            projSet = NULL,
                            removeOther = FALSE)


    inter = make_ena_tadmus(df = data_,
                            codeCols = codes,
                            unitCols = units,
                            convoCols = convo.inter,
                            metaCols = meta,
                            windowType = "MovingStanzaWindow",
                            windowSizeFor = 0,
                            windowSizeBack = window.size.inter,
                            meansRotate = FALSE,
                            projSet = NULL,
                            removeOther = FALSE)
  }else{

    ind = make_ena_ntx(df = data_,
                       codeCols = codes,
                       unitCols = units,
                       convoCols = convo.ind,
                       metaCols = meta,
                       # windowType = "Conversation",
                       windowType = "MovingStanzaWindow",
                       windowSizeFor = 0,
                       windowSizeBack = 1,
                       meansRotate = FALSE,
                       projSet = NULL)

    intra = make_ena_ntx(df = data_,
                         codeCols = codes,
                         unitCols = units,
                         convoCols = convo.intra,
                         metaCols = meta,
                         windowType = "MovingStanzaWindow",
                         windowSizeFor = 0,
                         windowSizeBack = window.size.intra,
                         meansRotate = FALSE,
                         projSet = NULL)

    inter = make_ena_ntx(df = data_,
                         codeCols = codes,
                         unitCols = units,
                         convoCols = convo.inter,
                         metaCols = meta,
                         windowType = "MovingStanzaWindow",
                         windowSizeFor = 0,
                         windowSizeBack = window.size.inter,
                         meansRotate = FALSE,
                         projSet = NULL)

  }

  # team = "team"
  # unit = "unit"

  # browser()

  unit_names = ind$connection.counts$ENA_UNIT
  new_order = order(ind$connection.counts$ENA_UNIT)
  ind_raw = as.matrix(ind$connection.counts)
  ind_raw = ind_raw[new_order,]

  new_order = order(intra$connection.counts$ENA_UNIT)
  intra_raw = as.matrix(intra$connection.counts)
  intra_raw = intra_raw[new_order,]

  new_order = order(inter$connection.counts$ENA_UNIT)
  inter_raw = as.matrix(inter$connection.counts)
  inter_raw = inter_raw[new_order,]

  ind_norm = rENA:::fun_sphere_norm(ind_raw)
  intra_norm = rENA:::fun_sphere_norm(intra_raw)
  inter_norm = rENA:::fun_sphere_norm(inter_raw)

  return(list(ind = ind_norm, intra = intra_norm, inter = inter_norm, unit_names = sort(unit_names),meta = ind$meta,ind_set = ind))
}



