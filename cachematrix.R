## These functions are to cache the inverse of a matrix, where caching is used
## in instances where the computations are time-consuming
## Besides solve, the function "ginv" from the MASS package can also be used.

## This function creates a special matrix that can cache the inverse of of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y ##assign y to x (x in the makeCacheMatrix environment)
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve ##m receives the value of solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has been calculated (and matrix has not changed), the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve() #calls from makeCacheMatrix
    if(!is.null(m)) {   ##if the inverse exists, retrieves from cache.
      message("getting cached data")
      return(m)
    }
    data <- x$get() ##if the inverse exists, retrieves from cache.
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
