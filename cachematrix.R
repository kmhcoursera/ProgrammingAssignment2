## This file contains functions to invert a matrix and 
## cache the inverted matrix such that repeated invocations
## will use the cached value
##
## Example
##
##   A = matrix(c(2,3,4,5), 2, 2)
##   cA = makeCacheMatrix(A)
##   B = cacheSolve(cA)
##
## B will now be the inverse of A (which can be checked by
## multiplying A and B:
##   A %*% B
## )

## Create a wrapper for a matrix (that works with cacheSolve below).
## The matrix is assumed to be square and invertible
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(value) inv <<- value
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Given a matrix wrapped by "makeCacheMatrix", return the
## inverse of the matrix. If called multiple times, a cached
## value will be returned
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
