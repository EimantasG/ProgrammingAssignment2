## The following functions are designed in order to cache and retrieve "matrix"
## objects - both primary matrix and its inverse version.

## This function creates a special "matrix" object, which is actually a list.
## This list may include objects of both primary matrix and its inverse
## version in the cache.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) inv <<- inverse
  get.inverse <- function() inv
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)

}

## This function computes the inverse of the special "matrix" object returned by
## makeCacheMatrix function and caches this inverse matrix. If the cached inverse
## matrix is already present, the function simply returns the inverse matrix from
## the cache.
cacheSolve <- function(x, ...) {
  inv <- x$get.inverse()
  if(!is.null(inv)) {
    message("Getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set.inverse(inv)
  inv
}