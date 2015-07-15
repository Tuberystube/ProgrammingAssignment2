## These two functions are co-operating with each other in order to keep
## track if the matrix operation in need is already performed 

## MakeCacheMatrix creates a special "vector", actually a list of functions to:
##
## 1. Set the value of a square-matrix (assumed to be invertible)
## 2. get the value of the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  invert <- function(x) solve(x, diag(ncol(x)))
  get <- function() x
  setinversion <- function(invert) inv <<- invert
  getinversion <- function() inv
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## CacheSolve function solves the inverse matrix of given square-matrix
## applying built-in solve function in function written above. It however,
## first checks if the inverse matrix has been already calculated. If this
## is the case, then inverse matrix solving is skipped and earlier calculated
## value from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinversion()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, diag(ncol(data)))
  x$setinversion(inv)
  inv
}