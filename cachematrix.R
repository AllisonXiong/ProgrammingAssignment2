## This file contains two functions: makeCacheMatrix which return a list of functions,
## and cacheSolve, which inverse the input matrix calling sub-functions from makeCacheMatrix


## This function creates a special "matrix" object that can cache its inverse

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


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    f <- makeCacheMatrix(x)
    m <- f$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    f$set(x)
    data <- f$get()
    m <- solve(data, ...)
    f$setsolve(m)
    m

        ## Return a matrix that is the inverse of 'x'
}
