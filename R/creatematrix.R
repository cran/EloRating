creatematrix <- function(eloobject, from="start", to="end", drawmethod=c("omit"), onlyinteracting=FALSE) {
  if(from == "start") from <- min(eloobject$truedates)
  if(to == "end") to <- max(eloobject$truedates)
  
  dataseq <- eloobject$logtable
  dataseq$xdate <- eloobject$truedates[1] - 1 + dataseq$Date

  #restrict to date range
  dataseq <- dataseq[which(dataseq$xdate >= as.Date(from) & dataseq$xdate <= as.Date(to)), ]
  
  # create epmty matrix based on presence
  pmat <- eloobject$pmat[which(eloobject$truedates == from):which(eloobject$truedates == to), ]
  IDS <- sort(colnames(pmat)[which(colSums(pmat)>0)])
  
  mat <- matrix(ncol=length(IDS), nrow=length(IDS), 0)
  colnames(mat) <- rownames(mat) <- IDS
  mat1 <- mat;
  
  # transform factors into characters...
  dataseq$winner <- as.character(dataseq$winner)
  dataseq$loser <- as.character(dataseq$loser)
  
  # add decided interactions
  xdata <- dataseq[dataseq$draw==FALSE, ]
  xdata <- table(xdata$winner, xdata$loser)
  mat[rownames(xdata), colnames(xdata)] <- xdata
  
  
  # separate by tied and decided interactions (if present in the data)
  if(sum(dataseq$draw)>0) {
    
    xdata <- dataseq[dataseq$draw==TRUE, ]
    xdata <- table(xdata$winner, xdata$loser)
    if(drawmethod=="0.5") {
      xdata <- xdata/2
      mat1[rownames(xdata), colnames(xdata)] <- xdata
      mat1 <- mat1 + t(mat1)
      mat <- mat + mat1
    }
    
    #mat1[rownames(xdata), colnames(xdata)] <- xdata
    
    if(drawmethod=="1") { 
      mat1[rownames(xdata), colnames(xdata)] <- xdata
      mat1 <- mat1 + t(mat1)
      mat <- mat + mat1
    }
    
  }
  
  # if "only interacting" was selected: remove those individuals from the matrix that havent interacted...
  if(onlyinteracting) {
    empty <- as.numeric(which(colSums(mat) + rowSums(mat) ==0))
    if(length(empty) > 0) mat <- mat[-empty, -empty]
  }
    
  return(mat)
  
}




