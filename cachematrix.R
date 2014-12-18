## cachematrix contains two functions
## 1. makeCacheMatrix() that creates a special matrix
## that can cache its inverse, in order to avoid 
## recomputing when needed and the matrix has not 
## changed
## 2.cacheSolve() that solves or computes the inverse
## of the matrix created by makeCacheMatrix()

## makeCacheMatrix() creates a special matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_m <<- inv
  getinv <- function() inv_m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve computes the inverse of the matrix 
## returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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