## This file contains the function written to inverse and cache a matrix 'x'.
## Coursera R Programming Assignment 2
## 28 Aug 2016

## makeCacheMatrix creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i<<- inv 
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the function should retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("retrieving cached data!")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
