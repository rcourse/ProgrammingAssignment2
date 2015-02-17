## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    ## Set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Get the value of the matrix
    get <- function() x
    
    ## Set the value of the inversed matrix
    setinv <- function(inverse) m <<- inverse
    
    ## Get the value of the inversed matrix
    getinv <- function() m
    
    ## Return list of defined functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        
}
