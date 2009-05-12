f.rang2finalmatrix <-
function(total)
{
  ##Werte die man anschauen muss
  werte <- sort(unique(total[[1]][upper.tri(total[[1]])]),decreasing=FALSE)
  ##Liste von Adjacency Matrizen, die entstehen k<U+00F6>nnen
  mat01 <- list()
  zaehler <- 0
  
  ##mat01 sind die verschiedenen Matrixen die entstehen k<U+00F6>nnen, ausgehend von der vollen Matrix
  
  for(i in werte)
    {
      zaehler <- zaehler+1
      matemp <- matrix(0,nrow=total[[2]],ncol=total[[2]])
      matemp[total[[1]]>=i] <- 1
      mat01[[zaehler]] <- matemp
    }
  mat01
}

