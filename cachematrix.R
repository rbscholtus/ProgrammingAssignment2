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
    
    ## the inverse of matrix x
    inv <- NULL
    
    ## set matrix x to a new value and clear the cache
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## return matrix x
    get <- function() x
    
    ## set the inverse of matrix x
    setinverse <- function(inverse) inv <<- inverse
    
    ## return the inverse of matrix x
    getinverse <- function() inv
    
    ## return special matrix object
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
    
    ## get the inverse from given special matrix object x
    inv <- x$getinverse()
    
    ## if the inverse exists, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## if it is not in the cache, determine the inverse
    inv <- solve(x$get())
    
    ## set the inverse in the cache
    x$setinverse(inv)
    
    ## return the inverse
    inv
}
