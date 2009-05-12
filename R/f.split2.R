f.split2 <-
function(ugr,grenze=3)
  {
    triugr <- triangulate(ugr,method="r")
    jt <- jTree(triugr)
    laenge <- unlist(lapply(jt$separators,length))
    
    index <- which(laenge<=grenze)[-1]

    #nehme gr<U+00F6>sstm<U+00F6>glichste Clique zum Abspalten
    index <- index[which.max(laenge[index])]
    
    if(length(index)>0)
      {
        #Knoten ohne separators
        gr1 <- setdiff(jt$nodes,unlist(jt$separator[index]))
        
        test <- querygraph(ugr,"subgraph",gr1)
        test2 <- querygraph(test,"connectedComp")
     
        gr1 <- na.omit(c(test2[[1]],jt$separators[index][[1]]))
        gr2 <- na.omit(c(unlist(test2[-1]),jt$separators[index][[1]]))
        toret <- list(gr1=gr1,gr2=gr2,sep=jt$separator[index][[1]],laengen=c(length(gr1),length(gr2)))
      }
    else
      toret <- FALSE
    toret
  }

