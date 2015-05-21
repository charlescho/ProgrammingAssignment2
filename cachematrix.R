## The following functions are used to solve and for and cache the innverse of a matrix.

## This function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    getMatrix <- function () x
    setMatrix <- function(m) {
        x <<- m
        inv <<- NULL
    }
    
    getInverse <- function() inv
    setInverse <- function(inverse) inv <<- inverse
    
    list(getMatrix = getMatrix, setMatrix = setMatrix, setInverse = setInverse, getInverse = getInverse)
}


## This function will return the inverse of the special cache matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        if(!is.null(inv)){
            message("returning inverse from cache.")
            return(inv)
        }
        
        mtx = x$getMatrix()
        inv = solve(mtx)
        x$setInverse(inv)
        inv
}
