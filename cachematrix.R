## This R file contains two functions that operate on a supplied matrix:
##      1. makeCacheMatrix - Takes an invertible matrix as input and returns 
##         a list of four functions that operate on that matrix.  
##      2. cacheSolve - Takes the list returned in makeCacheMatrix as input and
##         returns the inverse of the matrix.

## Use these functions to store in cache and retreive from cache the inverse 
## of the supplied matrix. Storing the inverse in cache reduces computational time
## in cases where the inverse is used multiple times e.g. a loop.  

##Example:
## > mymatrix <- matrix(c(4,3,3,2),2,2)
## > mymatrix
##      [,1] [,2]
## [1,]    4    3
## [2,]    3    2
# 
# > k<-makeCacheMatrix(mymatrix)

# > cacheSolve(k)
#       [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4


## makeCacheMatrix creates pointers to the passed matrix, gets and stores the inverse
#  of the passed matrix in cache.
##      input: invertible matrix x
##      output: List of four functions: list(setMatrix, getMatrix, setInverse, getInverse)
##              setMatrix - sets a pointer to the passed matrix and resets the 
#                       the cached inverse to NULL
#               getMatrix - returns a pointer to the matrix passed in makeCacheMatrix.
#               setInverse - stores in cache the passed matrix inverse.
#               getInverse - returns from cache the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ##initialize variable for storing inverse matrix
        
        setMatrix <- function(y) { 
                x <<- y ##reset the pointer to the matrix x with the new matrix y
                m <<- NULL ##reset the inverse to NULL
        }
        getMatrix <- function() x  ##return pointer to matix x
        setInverse <- function(mInverse) m <<- mInverse ##set the inverse to cache
        getInverse <- function() m ##return cahed inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve gets the cached version of the matrix inverse, if it is not NULL.
#  If the cached inverse is NULL, it calculates the inverse, stores it in cache,
#  and returns the calculated inverse.
#       input: list returned from makeCacheMatrx
#       output: the inverse of the cached matrix

cacheSolve <- function(x, ...) {
        
        m <- x$getInverse() #get the inverse from cache
        if(!is.null(m)) { #test to see if it is not NULL
                message("getting cached data")
                return(m) #return the cached inverse
        }
        data <- x$getMatrix() #inverse is NULL, so get the original matrix
        m <- solve(data, ...) #calculate the inverse
        x$setInverse(m) #set the inverse to cache
        m #return the inverse
}
