## These functions are used to compute the inverse of a matrix and cache the result.
## makeCacheMatrix creates a list containing the matrix and methods to store and retrieve
## a cached inverse matrix. 
## cacheSolve gets the inverse if it has been computed before,
## or computes it and stores it if it hasn't.

## Wraps a matrix in a list that can set or get the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the inverse of the matrix referenced within a makeCacheMatrix list.
## If the inverse is already computed is retrieved from the list,
## otherwise it is computed and stored in the list.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
