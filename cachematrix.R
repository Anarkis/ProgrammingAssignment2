## This function creates a special "matrix" object that can
## cache its inverse.
## The special matrix is stored in other environment
## we access through the different methods defined inside

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function return the inverse of the special
## "matrix" returned by makeCacheMatrix
## if the matrix hasn't inverse, it will calculate it
## and store in the cache to be used in next time.
## if already have it, it will return the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
