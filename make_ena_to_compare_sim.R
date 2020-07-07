### Generate ind, intra, and inter ena models for simulated datasets (deprecated)

make_ena_to_compare_sim = function(dataList,
                                    codeNames,
                                    units,
                                    convo.ind,
                                    convo.intra,
                                    convo.inter,
                                    window.size,
                                    meta
){

  ind.ena = list()
  intra.ena = list()
  inter.ena = list()

  for (i in 1:length(dataList)){

    dat = dataList[[i]]

    ind = make_ena_tadmus(df = dat,
                          codeCols = codeNames,
                          unitCols = units,
                          convoCols = convo.ind,
                          metaCols = meta,
                          windowType = "Conversation",
                          windowSizeFor = NULL,
                          windowSizeBack = NULL,
                          projSet = NULL)

    intra = make_ena_tadmus(df = dat,
                            codeCols = codeNames,
                            unitCols = units,
                            convoCols = convo.intra,
                            metaCols = meta,
                            windowType = "MovingStanzaWindow",
                            windowSizeFor = 0,
                            windowSizeBack = window.size,
                            projSet = NULL)

    inter = make_ena_tadmus(df = dat,
                            codeCols = codeNames,
                            unitCols = units,
                            convoCols = convo.inter,
                            metaCols = meta,
                            windowType = "MovingStanzaWindow",
                            windowSizeFor = 0,
                            windowSizeBack = window.size,
                            projSet = NULL)

    ind.ena[[i]] = ind
    intra.ena[[i]] = intra
    inter.ena[[i]] = inter

  }
  return(list(ind.ena = ind.ena, intra.ena = intra.ena, inter.ena = inter.ena))
}


