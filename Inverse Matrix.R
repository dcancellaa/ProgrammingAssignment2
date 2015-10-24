
makeCacheMatrix <- function(mat = matrix()){
  InvMat <- NULL
  
  FSet <- function(NewMat){
    mat <<- NewMat
    InvMat <<- NULL
  }
  FGet <- function(){
    mat
  }
  FSetInv <- function(NewInvMat){
    InvMat <<- NewInvMat
  }
  FGetInv <- function(){
    InvMat
  }

  list( Set=FSet,
    Get=FGet,
    SetInv=FSetInv,
    GetInv=FGetInv)
}

cacheSolve <- function(mat, ...){
 # InvMat <- NULL
  InvMat <- mat$GetInv()
  if(!is.null(InvMat)){
    message("Inverted matrix output is a cached value.")
    return(InvMat)
  }
  MatData <- mat$Get()
  InvMat <- solve(MatData, ...)
  mat$SetInv(InvMat)
  InvMat
}


#more test
D_Matrix <- makeCacheMatrix(matrix(c(11:14),nrow = 2,ncol = 2))
D_Matrix$Get()
D_Matrix$GetInv()
cacheSolve(D_Matrix)
