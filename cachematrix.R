## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## In fact, the new object is store some information:
## 1. The input matrix. 
## 2. Inverse matrix of the input matrix.
##
## In general, this clone matrix is really a list of functions to:
#  1. set the value of the matrix
#  2. get the value of the matrix
#  3. set the value of its inverse matrix (if any).
#  4. get the value of its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(ma) inv_matrix <<- ma
    getInvMatrix <- function() inv_matrix
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cacheSolve will retrieve the inverse from 
## the cache. Otherwise, it will calculate the inverse matrix and store this
## inverse matrix to retrieve later.
##
## Note: Computing the inverse of a square matrix can be done with the solve
## function in R.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_matrix <- x$getInvMatrix()
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    data <- x$get()
    inv_matrix <- solve(data, ...)
    x$setInvMatrix(inv_matrix)
    inv_matrix
}
