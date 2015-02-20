## This file contains functions for caching matrix inversion.
## makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.
## cacheSolve function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve retrieves the inverse from the cache.

## Creates a special "matrix", which is
## really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix inverse
## 4.  get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  # Variable for holding solve result
  s <- NULL
  set <- function(y) {
    # Reset cache in case if matrix change
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the setsolve
## function.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  # Call solve for inversion of the matrix
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
