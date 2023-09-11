## makeCacheMatrix creates a special matrix object whose inverse can be cached.
## cacheSolve fetches the inverse of matrix either from cache or computes it.

## sets the matrix, inverse matrix and fetches the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseX <- matrix()
  set <- function(matr) {
    x <<- matr
    inverseX <<- matrix()
  }
  get <- function() x
  setinverse <- function(inv) inverseX <<- inv
  getinverse <- function() inverseX
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## returns the inverse of matrix either from cache or after computing it

cacheSolve <- function(x, ...) {
        invMatrix <- x$getinverse()
        if(!is.na(invMatrix)) {
          message("getting cached data")
          return(invMatrix)
        }
        mat <- x$get()
        invMatrix <- solve(mat)
        x$setinverse(invMatrix)
        invMatrix
}
