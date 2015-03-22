## Creates a matrix object that supports caching of its inverse
## Input is the original matrix of which we build this inverse
## cached matrix object
makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL

        set <- function(y) {
                x <<- y
                mInv <<- NULL
        }

        get <- function() x
        setInv <- function(inv) mInv <<- inv
        getInv <- function() mInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Returns a matrix that is the inverse of 'x'. The returned value
## is either the cached value stored inside the object "cachedMatrix"
## or it solves (i.e. computes the inverse), caches that object and
## returns that inverse
cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
