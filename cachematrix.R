## These functions create a matrix and a function to calculate its inverse 
## both rely on the fact that the matrix is designed to cache inverse calculations

## Creates matrix object that stores inverse 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inv) inverse <<- inv
    
    getInverse <- function() inverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Returns inverse of a matrix made with makeCacheMatrix() 
cacheSolve <- function(x, ...) {
    
    x_inverse <- x$getInverse() 
    
    if(!is.null(x_inverse)) {
        message("getting cached inverse")
        return(x_inverse)
    }
    
    x$setInverse(solve(x$get(), ...))
    x$getInverse()
}
