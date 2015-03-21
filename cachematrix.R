## The goal of the two current functions is to compute
## and caching the inverse of matrix

## The following function creates a special matrix object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinv <- function(inv) n <<- inv
        getinv <- function() n
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function computes the inverse of the special
## matrix returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getinv()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setinv(n)
        n
}
