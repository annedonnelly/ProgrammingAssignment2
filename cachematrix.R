## A pair of functions to allow for the caching of a potentially
## time-consuming operation, in this case the inverse of a matrix.
## 
## Example usage:
##          mdat <- matrix(c(1,2, 11,12), nrow = 2, ncol = 2)
##          mCache <- makeCacheMatrix(mdat)
##          solveCache <-cacheSolve(mCache) 
##
## Assumes the matrix is square
##

## Creates a list of functions to get and set a matrix and to calculate 
## and return its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Checks whether the supplied matrix has already been inverted
## Existing inverse is returned if it exists, otherwise it is
## calculated, cached for later use and returned

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
