## The two functions below will take a square, invertible
## matrix, solve it's inverse and cache the results

## This function takes a matrix, initilize the inverse to NULL,
## and provides setters and getters for both the matrix object passed
## and its inverse. It returns a special object through a list 

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) invMat <<- inv
  getinverse <- function() invMat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes an object created from the above function.
## It solves the inverse of the matrix from the object passed in
## the parameter, by first checking if there is a valid cached
## inverse, so it returns the cached value. If not, it solves the
## inverse, sets the value of inverse in the cache, and returns
## the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMat <- x$getinverse()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  theMatrix <- x$get()
  invMat <- solve(theMatrix)
  x$setinverse(invMat)
  invMat
}