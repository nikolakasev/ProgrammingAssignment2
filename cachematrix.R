## Put comments here that give an overall description of what your
## functions do

## The function expects a matrix as input and returns a structure
## that is used as a bulding block for caching

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function receives a matrix and checks if it's inverse is already computed
## Returns from cache of the inverse is known
## Computes the inverse, caches and returns the value

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix_to_invert <- x$get()
  i <- solve(matrix_to_invert)
  x$setinverse(i)
  i
}
