## matrix inversion is a costly computation so having a means to cache an 
## inversion rather than recalculate it is valuable. These two functions 
## work together to invert a matrix and save it outside the function so their
## values can be fetched instead of recalculated if the original exists

## This is a function that creates and stores a list of functions
## by using the <<- operator to assign variables to a separate environment
## It returns a list of 4 getting and setting functions in a separate environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## sets original matrix
        setmatx <- function(y) {
                ## create variables in separate environment
                x <<- y
                ## set to null so we know it has not been calc'd already
                m <<- NULL
        }
        getmatx <- function() x
        setinverse <- function(matx) m <<- matx
        getinverse <- function() m
        ## return list of functions
        list(setmatx = setmatx, getmatx = getmatx,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function uses the functions created in makeCacheMatrix() to determine if it should
## compute the inverse of the matrix or return the cached version if it already exists.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        ## see if inverted matrix m has been calc'd before and return it if it has
        if(!is.null(m)) {
                return(m)
        }
        ## matrix not already calc'd, so get it, invert it, and return it
        matricks <- x$getmatx()
        m <- solve(matricks, ...)
        x$setinverse(m)
        m
}
