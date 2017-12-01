## Matrix the Inverse of a Matrix

## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly

## Below are pair of functions that cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" 
## object that can cache its inverse containing the following:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
        invrse <- NULL
        set <- function(y) {
                x <<- y
                invrse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invrse <<- inverse
        getInverse <- function() invrse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix function.
## If the inverse has already been calculated
## (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse fromthe cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrse <- x$getInverse()
        if(!is.null(invrse)){
                message("getting cached data!")
                return(invrse)
        }
        data <- x$get()
        invrse <- solve(data)
        x$setInverse(invrse)
        invrse
}
