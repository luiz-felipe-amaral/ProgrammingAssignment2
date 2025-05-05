## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates takes an argument that is a matrix and makes an object
## "CacheMatrix" with two attributes: the matrix itself and its inverse. But the
## inverse is initially set as NULL and only updated later.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, 
         get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function takes an object "cacheMatrix" and returns its inverse. If the 
## inverse has not been previously computed, it computes the inverse and updates
## the cacheMatrix so that its inverse is not NULL anymore. If the inverse has 
## been previously computed, it prints "getting cached data" and returns the 
## previously computed inverse.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}

