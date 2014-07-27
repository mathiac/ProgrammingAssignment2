## The two following functions can be used to store a Matrix
## and its inverse Matrix in order not to compute the inverse
## each time one wants to use it

## This function create an object which stores a Matrix and
## its inverse and has some methods to use it.
makeCacheMatrix <- function(x = matrix()) {
    
    # by default the inverse is not computed
    inverse <- NULL
    
    # method to set the x matrix, inverse not computed by default
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # method to get the x matrix
    get <- function() x
    
    # method to set the inverse matrix
    setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
    
    # method to get the inverse matrix
    getInverse <- function() inverse
    
    # the object is a list
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}


## This function returns the inverse matrix of x, using the
## previous computed value if available, and setting it if
## not.
## There is no error-checking, it assumes that x is invertible.
## 'x' should be a matrix object created by makeCacheMatrix function.
cacheSolve <- function(x, ...) {
    
    # Try to use the cached value of the inverse.
    inverse <- x$getInverse()
    
    # If it exists then returns it and displays a message.
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    
    # If it doesn't exist then computes it ...
    matrixData <- x$get()
    inverse <- solve(matrixData)
    
    # and stores it in the object for future use ...
    x$setInverse(inverse)
    
    # and returns it.
    inverse
}
