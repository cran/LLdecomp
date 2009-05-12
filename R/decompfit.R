decompfit <-
function(data,smax=3,...)
  {
                      
    total <- f.data2impmat(data,...)
    mat01 <- f.rang2finalmatrix(total)
    a <- f.grenze2active(mat01,smax)
    activeint <- a[[1]]
    sepisaint <- a[[2]]
    list(cliques=activeint,separators=sepisaint)
  }

