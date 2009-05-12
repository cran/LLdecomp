f.grenze2active <-
function(mat01,grenze)
{
  b <- FALSE
  i <- 0
  
  while(is.list(b)==FALSE)
    {
      i <- i+1
      b <- f.split2(f.makegraphtosplit2(1:nrow(mat01[[1]]),mat01[[i]]))
    }
  
  active <- b[1:2]
  sepisa <- b[3]
  j <- i

  #length active
  la <- lapply(active,length)
  
  while(any(la>grenze))
    {
      ## Das wird sp<U+00E4>ter das neue active
      activetemp <- list()
      sepisatemp <- list()

      for(i in 1:length(la))
        {
          ## Falls jetzt schon kleiner, nichts machen...
          if(la[[i]]<=grenze)
            activetemp <- c(activetemp,list(active[[i]]))
          
          else
            {
              u1 <- f.makegraphtosplit2(as.integer(active[[i]]),mat01[[j]])
              splitter <- f.split2(u1,grenze)
              
              if(!is.logical(splitter))
                {
                  sepisatemp <- c(sepisatemp,splitter[3])
                  activetemp <- c(activetemp,splitter[1:2])
                }
              else
                activetemp <- c(activetemp,list(active[[i]]))

            }
        }

      ## Falls man keinen Split machen konnte
      if(length(sepisatemp)==0)
        j <- j+1
      else
        {
          sepisa <- c(sepisa,sepisatemp)
          active <- activetemp
          la <- lapply(active,length)
        }
    }
  activeint <- lapply(active,as.integer)
  sepisaint <- lapply(sepisa,as.integer)
  list(activeint,sepisaint)
}

