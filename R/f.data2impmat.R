f.data2impmat <-
function(data1,...)
  {
    nr   <- ncol(data1)
    importancemat <- matrix(NA,nrow=nr,ncol=nr)
    for(i in 1:nr)
      {
        x <- data1[,-i]
        y <- as.factor(data1[,i])
        
        rfobj <- randomForest(x=x,y=y,...)
        importancemat[i,-i] <- (rfobj$importance)
      }
    rangsumme <- t(apply(importancemat,1,rank))
    total <- t(rangsumme)+rangsumme
    diag(total) <- NA
    list(total,nr)
  }

