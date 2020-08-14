## This function computes the inverse of a matrix but first 
## checks to see if the computation has already been done and cached.
## If the calculation has been done, it sources the solution from the cache.
## If the calculation has not been done, it computes the inverse and
## caches the solution.

## This function serves to store the cached solutions.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function searches for a solution already cached, if none exist it 
## solves it and caches the data.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
