## These functions tries to cache matrix inverse calculation using global environment.

## This function creates container object that stores matrix and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv_par) inv <<- inv_par
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns inverse of matrix returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached inverse of matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
