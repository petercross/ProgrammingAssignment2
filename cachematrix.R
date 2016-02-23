## Functions for creating and using inverted matrices which caching ability


## Creates cacheable matrix for inputting to
## cacheSolve() function which sets and gets 
## the cached values

makeCacheMatrix <- function(x = matrix()) {
  
  # data type check
  if (!is.matrix(x)) {
    stop("not a matrix")
  }
  
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  

  get <- function() x
  setinverse <- function(inv) inverse <<- inverse
  getinverse <- function() inverse
  
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
  
}


## finds the inverse from previous function
## checks for inverse already present
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}