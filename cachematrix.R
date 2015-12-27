## The following functions create a cache for a matrix and for its inverse. 
## They also allow for the calculation of the inverse and/or returning the 
## pre-calculated inverse from cache

## Create a cache store for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    cachedinv <- NULL
    set <- function(y) {
        x <<- y
        cachedinv <<- NULL
    }
    get <- function() x
    setinverse <- function(mtx) cachedinv <<- mtx
    getinverse <- function() cachedinv 
    list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Calculate the inverse of the matrix if not already calculated.
## If calculated, return the cached result

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getmatrix()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setmatrix(inv)
    inv
}
