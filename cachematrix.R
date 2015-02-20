## Functions below are used to create a special object
## that stores a matrix and cache's its inverse form

## Function makeCacheMatrix creates a special
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    ## Set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Get the value of the matrix
    get <- function() x
    
    ## Set the value of the inversed matrix
    setinv <- function(inverse) inv <<- inverse
    
    ## Get the value of the inversed matrix
    getinv <- function() inv
    
    ## Return list of defined functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function cacheSolve computes the inverse of the special
## "matrix" returned by function makeCacheMatrix;
## if the inverse has already been calculated and the matrix
## has not changed, then the cacheSolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        ## Return the inversed matrix from the cache
        message("--> getting cached inverse matrix <--")
        return(inv)
    }
  
    data <- x$get()
    
    ## Test for matrix singularity
    if(det(data) == 0) {
        message("Warning: singular matrix - NA value was set")
        inv <- NA
    } else {
        ## Function solve is used to inverse the matrix
        inv <- solve(data, ...)      
    }

    ## Set the value of the inversed matrix to the cache
    x$setinv(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
