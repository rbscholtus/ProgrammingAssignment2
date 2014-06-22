## The below two functions are used to create a special "matrix" object, and to
## determine its inverse. The inverse of the matrix is stored in a cache to
## improve performance.


## The first function, makeCacheMatrix creates a special "matrix". This is
## really a list that stores a matrix and its inverse, as well as functions to
##  * set the value of the matrix, and empty the cache
##  * get the value of the matrix
##  * set the value of the inverse
##  * get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function determines the inverse of the special "matrix" created
## with the above function. It checks the cache to see if the inverse has
## already been calculated. If this is the case, it returns the inverse from
## the cache, skipping the computation. Otherwise, it calculates the inverse of
## the data and puts the inverse in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    inv <- solve(x$get())
    x$setinverse(inv)
    inv
}
