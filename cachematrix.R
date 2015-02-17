## Functions below are used to create a special object
## that stores a matrix and cache's its inverse form

## Function makeCacheMatrix creates a special
## "matrix" object that can cache its inverse

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

## Function cacheSolve computes the inverse of the special
## "matrix" returned by function makeCacheMatrix;
## if the inverse has already been calculated and the matrix
## has not changed, then the cacheSolve should retrieve
## the inverse matrix from the cache

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    
    if(!is.null(m)) {
        ## Return the inversed matrix from the cache
        message("--> getting cached inverse matrix <--")
        return(m)
    }
  
    ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    ## Function solve is used to inverse the matrix
    m <- solve(data, ...)
    x$setinv(m)
    m
}
