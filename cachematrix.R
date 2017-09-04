## The makeCachematrix and cacheSolve functions work in tandem to minimize recalculating
## matrix inverses by first checking if the most recent result is stored in cache. The
## makeCachematrix function contains four functions in its local environment which set 
## and get the matrix input as well as its inverse, making use of R's lexical scoping. 
## The cacheSolve matrix then accepts an object of type makeCacheMatrix and proceeds to
## calculate the matrix inverse if it is not already stored in cache.

## The makeCachematrix function stores a matrix inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y) {
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinverse = setinv,
       getinverse = getinv)
}


## The cacheSolve function attempts to retrieve a cached copy of the matrix inverse
## and if not found, calculates it

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.na(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
