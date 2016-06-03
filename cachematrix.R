## These functions permit to cache a matrix and its inverse
## so to use the cached version whenever is possible

## Create a special matrix that can cache matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
 

## Invert the special matrix created with makeCachedMatrix using cached value 
## if possible.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
