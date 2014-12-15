## Programming Assignment 2 - By Venkata Narasimhan [14-12-2014]
## Submitted on [14-12-2014] to git hub.

## This function [makeCacheMatrix] creates a special "matrix" 
##object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
  {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(mean) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already
##been calculated (and the matrix has not changed), then 
##cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
