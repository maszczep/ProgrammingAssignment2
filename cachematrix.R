## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly. 
## In this assignment, I wrote a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # stores the cached value or NULL if nothing is cached
        # initially set as NULL
        inverse <- NULL
        
        # stores a matrix
        setMatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # returns matrix
        getMatrix <- function() {
                x
        }
        
        # cache argument
        setInverse <- function(inv) {
                inverse <<- inv
        }
        
        # get argument from catche
        getInverse <- function() {
                inverse
        }
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # get argument from catche
        inverse <- x$getInverse()
        
        # return it, if it exists...
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # ...or calculate it and store in catche
        data <- x$getMatrix()
        inverse <- solve(data)
        x$setInverse(inverse)
        
        inverse
}
