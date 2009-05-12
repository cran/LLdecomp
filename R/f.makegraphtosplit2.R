f.makegraphtosplit2 <-
function(knoten,admatsym)
  {
    ## Nimmt knoten als integer und admatsym und gibt ungerichteter
    ## Graf raus von admatsym mit entsprechenden Knoten
    admatsym <- admatsym[knoten,knoten]
    le <- as.character(knoten)
    colnames(admatsym) <- rownames(admatsym) <- le

    igr <- graph.adjacency(admatsym,mode="undirected")
    ugr  <- igraph.to.graphNEL(igr)
  }

