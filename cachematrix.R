## These functions compute the inverse of a matrix or retrieve the cached matrix inversion if already computed. 

## makeCacheMatrix creates a list of functions for inverting, cacheing and retrieving a matrix.
## input x is a square matrix.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve uses the list created in makeCacheMatrix to invert a matrix or retrieve a previously cahced solution.
## Input x is the list created using makeCacheMatrix.
cacheSolve <- function(x,...) {
  
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
  
}
