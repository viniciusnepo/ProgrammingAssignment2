## These are functions that cache the inverse of a matrix.
## In order to use it, you must create an object using the
## "makeCacheMatrix" function, and then retrieve it's inverse
## with the "cacheSolve" function.


## This is the function that creates an special matrix
## containing the functions for cache

makeCacheMatrix <- function(x = matrix()) {
  cachedReverse <- NULL
  set <- function(y) {
    x <<- y
    cachedReverse <<- NULL
  }
  get <- function() x
  setreverse <- function(reverse) cachedReverse <<- reverse
  getreverse <- function() cachedReverse
  list(set = set, get = get,
       setreverse = setreverse,
       getreverse = getreverse)
}


## This function computes the inverse matrix and cache it
## using the special matrix functions

cacheSolve <- function(x, ...) {
  reverse <- x$getreverse()
  if(!is.null(reverse)) {
    message("getting cached data")
    return(reverse)
  }
  data <- x$get()
  reverse <- solve(data, ...)
  x$setreverse(reverse)
  reverse
}

# Example:
# x <- makeCacheMatrix(matrix(c(2,4,8,6,1,3,7,7,1,3,9,9,0,1,5,8), 4, 4))
# cacheSolve(x)