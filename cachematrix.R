## These functions support creating a mechanism to cache the inverse of a
## matrix. Then, subsequent calls to retrieve the inverse of the matrix 
## will return the cached inverse. If the inverse has not been cached,
## then it will calculate the inverse and store it in the cache.

## Create a specialized matrix data structure that contains functions
## for storing and retrieving the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return the inverse of the x. Use the cached inverse if it
## exists. Otherwise, calculate the inverse and cache it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
