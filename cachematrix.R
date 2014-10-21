## The makeCacheMatrix and cacheSolve functions cache the inverse of a matrix to avoid calculating it repeatedly.

## makeCacheMatrix creates a list of the following functions:
      ## Sets the matrix
      ## Gets the matrix
      ## Sets the inverse
      ## Gets the invers

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) s <<- solve
      getinverse <- function() s
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## cacheSolve calculates the inverse of the matrix created with the makeCacheMatrix function. However, if this has already been calculted, it gets this from the cache and skips the computation.

cacheSolve <- function(x, ...) {
            s <- x$getinverse()
            if(!is.null(s)) {
                  message("getting cached data")
                  return(s)
            }
            data <- x$get()
            s <- solve(data, ...)
            x$setinverse(s)
            s
}