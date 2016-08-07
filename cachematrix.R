## Cachematrix.r provides a set of functions to allow
## the user to cache the results of matrix inversion

## Make CacheMatrix creates an object that can
## 'Set' - store a matrix
## 'Get'- return the stored matrix
## 'Set inverse' - cache the inverse of the matrix 'set'
## 'Get inverse' - return the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks to see if the makeCacheMatrix object
## has a inverse matrix cached. If there is an inverse matrix
## cached then it returns it, else it solves and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
