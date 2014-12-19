## Functions makeCacheMatrix() & cacheSolve() performs matrix inversion
## through caching the inverse of the matrix which serves to save 
## computation time.

## Function: makeCacheMatrix(x), x is a matrix.
##
## Notes:
##   1. x = original matrix
##   2. function computes m which is the inverse matrix of x 
##   3. returns a LIST of 4 functions:
##      - set    : set the values of the matrix x
##      - get    : get the values of the matrix x
##      - setInv : set the values of inverse matrix m
##      - getInv : get the values of inverse matrix m 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
       x <<- y # store original matrix 
       m <<- NULL # initialise to NULL matrix
    }
    get <- function() x
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    list(set=set, get=get,setInv=setInv,getInv=getInv)
}


## Function: cacheSolve() returns m which is the inverse matrix of x
## 
## Notes:
##   1. x = original matrix
##   2. function computes m which is the inverse matrix of x 
##   3. function checks if the Inverse of x has already been calculated:
##        if calculated, it returns m,
##        otherwise it computes the Inverse m through solve(x)
##   4. returns a matrix m that is the inverse of 'x'

cacheSolve <- function(x) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("Getting cached data ...")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInv(m)
    m   
}
