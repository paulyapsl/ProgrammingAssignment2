## These functions are to cache the inverse of a matrix, where caching is used
## in instances where the computations are time-consuming
## Besides solve, the function "ginv" from the MASS package can also be used.

## This function creates a special matrix that can cache the inverse of of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ##set m (solve in this case) to NULL
    set <- function(y) {  ##set function sets x to the argument y
      x <<- y ##assign y to x (x in the makeCacheMatrix environment)
      m <<- NULL ##sets M to NULL
    }
    get <- function() x ##get returns the value of x (argument of MakeCacheMatrix)
    setsolve <- function(solve) m <<- solve ##m receives the value of solve
    getsolve <- function() m  ##getsolve returns the value of m (from makeCacheMatrix)
    list(set = set, get = get,  ##returns a labeled vector of functions set, get, setsolve and getsolve
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has been calculated (and matrix has not changed), the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve() #calls from makeCacheMatrix (if it was calculated previously)
    if(!is.null(m)) {   ##if the inverse exists, retrieves from cache.
      message("getting cached data") ##returns message if the inverse exists
      return(m) ##gets value from cache and returns the value to the cacheSolve function
    }
    data <- x$get() ##if the inverse exists, retrieves from cache.
    m <- solve(data, ...) ##calculates the inverse matrix
    x$setsolve(m) ##set m in x to the calculated solve
    m #returns m
}
