## These functions create a cached matrix.  The structure contains a matrix 
## and its inverse.  If the matrix is changed, then the inverse is re-computed
##  prior to being returned.  If not, then the cached inverse is returned.
##
##  They facilitate computing the inverse of the matrix for when it is needed
##  for repetitive calculations. The inverse is computed the first time
##   'cacheSolve' is called.  A copy of the inverted matrix is
##   stored inside the structure which had been defined by the
##   'makeCacheMatrix' function.  Subsequent calls for the inverse
##   return the cahce version rather than re-computing it.
##
##  To use these function, follow these steps:
##  1. Define your matrix.  It is assumed to be a square matrix which has
##     a non-zero determinant.
##  2. Run the 'makeCacheMatrix' function, passing it matrix as an argument.
##     the return value from this function should be stored in a new variable.
##  3. Run the 'cacheSolve' function with the return value from 
##     'makeCacheMatrix'. The value returned is the inverse.
##  4. To change the matrix styroed in the cached structure, use the 'set'
##     function.
##  5. To obtain the matric whose inverse is cached, use the 'get' function.

## Note that the first time you call 'cacheSolve' with a new matrix stored 
## in the cached matrix, you can pass additional arguments to the Solve
## routine which computes the inverse.  In general, these won't be needed.

## makeCacheMatrix
##----------------
## Accepts a matrix as an input.
## Creates a special 'list' return value which contains the names of functions
## to create and access the inverse.
## The variable 'inverse' contains the inverted matrix.  It is stored in the
## environment for this function so it is available to the sub-functions
## created within it.
##
## The output from 'makeCacheMatrix' is a list object containing four 
##  functions.  They are used are follows (x is assumed to be the output from
##  makeCacehMatrix):
##
## x$set(x1):        Sets the matrix stored in the special cached matrix to 
##                   x1. Using this function forces re-calculation of the
##                   inverse;
## x$get():          Returns the matrix whose inverse is cached
## x$setinverse(x2): Sets the cached inverse to the argument
## x%getinverse():   Returns the value of the cached inverse.  This function
##                   returns NULL if the inverse needs re-computing.
##

makeCacheMatrix <- function(original = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        original <<- y
        inverse <<- NULL
    }
    
    get <- function() {
        original
    }
    
    setinverse <- function(inv) {
        inverse <<- inv
    }
    
    getinverse <- function() {
        inverse
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
##-----------
## This function returns the inverse matrix stored in a cached matrix.
##  FIRST, you must run 'makeCacheMatrix' to create the cached matrix
##  structure.
##
## This function accepts an argument from 'makeCacheMatrix'.  It will return 
## the inverse of the matrix stored in the cached matrix strucutre.  If
## needed, the inverse is re-computed.  Otherwise, the cached inverse is
## returned.
##
## This function will accept additional arugments to be passed to 'solve'.
## These only apply when the inverse is re-computed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getinverse()
    mat <- x$get()
    if(!is.null(inverse)) {
      ##  message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setinverse(inverse)
    inverse
}
