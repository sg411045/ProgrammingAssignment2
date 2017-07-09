## This script creates a matrix which can cache its inverse
## This is a performance optimized version of the matrix where
## computation time is saved recalculating the inverse if the matrix has not changed


## This function creates a matrix which has a cacheable inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This a updated version of the solve function to return a inverse
## from cache if the inverse is already pre-calculated
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    # use cached inverse
    print("using cached copy")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  mat$setinv(m)
  m
}
