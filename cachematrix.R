## These functions help storing calculation of inversed matrices, in order not to repeat calculation twice

## makeCacheMatrix creates a list containing functions to set and get your matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse=matrix()) inv <<- as.matrix(inverse)
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## First tries to look for the inverse matrix if already in cache, else calculates inverse and stores it in cache
## When this function is run twice consecutively on the same CacheMatrix, it will definitely read the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
