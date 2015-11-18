## By: Hristiyan Manolov
## Date: 11/17/2015
## Code: These two function are intended to create a matrix that could store its
## inverse in cache and retrive it from memory if it has already been computed once

## This function creates a matrix that can store its inverse in cache

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## The cacheSolve function takes a matrix or multiple matricies as input 
## and if their inverse is already been calcualted then it gets recalled from cache.
## If they are not found then they are computed and stored in cache.

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m    
    
    ## Return a matrix that is the inverse of 'x'
}
