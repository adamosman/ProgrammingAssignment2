## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function takes advantages of R's lexical scoping and creates a function that four functions to be used by cacheSolve. It passes an invertible matrix and produces an object that contains the four necessary functions.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
# This function passses the object created by the makeCacheMatrix function and produces the inverse of the matrix originally passed to makeCacheMatrix. If the inverse was already created, then the function returns the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
