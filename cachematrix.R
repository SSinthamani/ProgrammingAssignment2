## A pair of functions that cache the inverse of a matrix
## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialise the Inverse Matrix
  inv <- NULL
  ## 1) Set the matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## 2) Get the matrix value
  get <- function(){
    x 
  }
  ## 3) Set the value of the inverse Matrix
  setinv <- function(inverse) {
    inv <<- inverse 
  }
  ## 4) Get the Value of the inverse matrix
  getinv <- function() {
    inv 
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

#Computing the inverse of a square matrix can be done with the solve function in R.
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.
#inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
## Finally returns the inverse matrix
## Test Run:
##x = rbind(c(1,-1/4),c(10,-1/4))
##> m = makeCacheMatrix(x)
##> m$get()
##[,1]  [,2]
##[1,]    1 -0.25
##[2,]   10 -0.25
##> cacheSolve(m)
##[,1]      [,2]
##[1,] -0.1111111 0.1111111
##[2,] -4.4444444 0.4444444
##> cacheSolve(m)
##getting cached data
##[,1]      [,2]
##[1,] -0.1111111 0.1111111
##[2,] -4.4444444 0.4444444
##> 