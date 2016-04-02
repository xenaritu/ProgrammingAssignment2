## This R function is able to cache potentially time-consuming computation of matrix inversion 
##so that when we need the value again, it can be looked up in the cache rather than recomputed

## This function creates a list of four functions to set a matrix , get the value of matrix 
##set the inverse of matrix and get the value of matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
  }
  
    get <- function() x
    setInv <- function(INV) I <<- INV
    getINV <- function() I
    list(set = set, get = get,
       setInv = setInv,
       getINV = getINV)
}


## This function calculates the value of Inverse of matrix if it has not been calulated earlier 
## else it caches the value of the inverse directly

cacheSolve <- function(x, ...) {
       
  I <- x$getINV()
  if(!is.null(I)) {
      message("getting cached data")
      return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInv(I)
  I
}
