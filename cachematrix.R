## makeCacheMatrix creates a special type of matrix that allows
## caching of the inverse of the matrix

## x is the input matrix

## 4 internal functions are defined that are not evaluated
## but used when cacheSolve() is called

## returns a list of methods to be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        setMat <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMat <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(setMat = setMat, getMat = getMat,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## cacheSolve finds the inverse of an input matrix
## that is defined by makeCacheMatrix()

## x is the input matrix

## if the inverse of the matrix is already cached
## it returns the cached inverse matrix.
## If there is no cached inverse, calls the function
## from makeCacheMatrix() to calculate the inverse.

## returns a the inverse matrix and a prompt if the
## output is cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        dataMat <- x$getMat()
        m <- solve(dataMat, ...)
        x$setInverse(m)
        m
        
}
