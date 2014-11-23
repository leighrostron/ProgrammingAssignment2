## Put comments here that give an overall description of what your

## This function returns the inverse of a matrix and caches it to memory

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {x}
    setinverse <- function(solve) {m <<- solve}
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## this function will first check if an inverse of function has already been solved by
## prior function and return the contents. If not already solved, it will go ahead and 
## find the inverse

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
