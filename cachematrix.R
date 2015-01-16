## The following functions serve to calculate the inverse of the matrix
## and store them, so that if they are needed again, they can easily be
## retrieved instead of recalculating it, which might take a while.

## Next is the function that caches the matrix. It has creates a list 
## that containts function to get and set the value of the matrix and
## get an set the value of the inverse of that matrix.

makeCacheMatrix <- function(X = matrix()) {
  
      M=NULL
      setmatrix<-function(y){
            X<<-y
            M<<-NULL
      }
      getmatrix<-function() X
      setinverse<-function(solve) M<<-solve
      getinverse<-function() M
      
      list(setmatrix=setmatrix, getmatrix=getmatrix,
           setinverse=setinverse, getinverse=getinverse)

}


## The following function first checks if the inverse of the
## matrix is already in cache, and if so, the function gets
## the inverse from there, if not, it calculates the inverse.

cacheSolve <- function(X, ...) {
        
      M<-X$getinverse()
      if(!is.null(M)){
            message("got from the cached data")
            M
            ##this is complete alternative of 'return(m)' if
            ##the rest of the function is embedded in 'else'.
      }
      else{
            data<-X$getmatrix()
            M<-solve(data,...)
            X$setinverse(M)
            M
      }
      
}
