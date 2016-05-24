## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## init the inv
    inv <- NULL
    ##set
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ##get
    get <- function() x
    ##setsolve
    setsolve <- function(solve) inv <<- solve
    ##getsolve
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ##get original inv of matrix
    inv <- x$getsolve()
    ##if the original inv isn't NULL
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ##calc the new inv
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}