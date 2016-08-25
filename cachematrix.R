## This pair of functions creates an object that is a list of functions that can
## store a matrix and cache its inverse. 

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invr <- NULL
    set <- function(y) {
        x <<- y
        invr <<- NULL
    }
    get <- function() x
    setinv <- function(invers) invr <<- invers
    getinv <- function() invr
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, the cached inverse is retrieved.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invr <- x$getinv()
    if(!is.null(invr)){
        message("getting cached data")
        return(invr)
    }
    data <- x$get()
    invr <- solve(data, ...)
    x$setinv(invr)
    invr
}
