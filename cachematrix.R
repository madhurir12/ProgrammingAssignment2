## This file has two functions makeCacheMatrix & cacheSolve
## which can be used to create a special "matrix" object that can cache its inverse
## and a function to look up the cached inverse value for the matrix

## This function creates a special "matrix" object that can cache it's inverse
## It takes a "matrix" object as input and provides methods to get, set it's value
## as well as get , set it's inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. Executing this method will return the inverse from the
## cache if it's available otherwise calculates the inverse which will be cached

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  data <- x$get()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  i <- solve(data)
  x$setInverse(i)
  i
}
