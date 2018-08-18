## Matrix inversion is usually a costly computation in terms of time.
## In fact, literature online states that caching the inversion can cut
## run time significantly.  The two functions below will do as the
## assignment requests.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ix = NULL
        set = function(y) {
                x <<- y
                ix <<- NULL
        }
        get = function() x
        setix = function(inverse) ix <<- inverse 
        getix = function() ix
        list(set=set,
             get=get,
             setix=setix,
             getix=getix)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ix = x$getix()
        if (!is.null(ix)){
                message("getting cached data")
                return(ix)
        }
        mat.data = x$get()
        ix = solve(mat.data, ...)
        x$setix(ix)
        return(ix)
}
