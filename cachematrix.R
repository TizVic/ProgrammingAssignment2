## These functions permit to cache a matrix and its inverse
## so to use the cached version whenever is possible

## Create a "special matrix" (list) that can cache matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
 

## Invert the special matrix created with makeCachedMatrix using cached value 
## if possible.

cacheSolve <- function(x, ...) {
    
        # Is cached?
        inv <- x$getsolve()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        
        # compute the inverse
        data <- x$get()
        inv <- solve(data, ...)
        
        # cache the inverse
        x$setsolve(inv)

        ## Return a matrix that is the inverse of 'x'
        inv
        
}
