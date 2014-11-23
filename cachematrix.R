## Create and store a matrix and its inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # sets cached inverse to NULL
    set <- function(y) {
        x <<- y # changes matrix to new input
        i <<- NULL # if matrix is changed, resets cached inverse to NULL
    }
    get <- function() x # returns matrix
    setInvs <- function(invs) {
        i <<- invs # sets cached inverse 
    }
    getInvs <- function() i # returns cached inverse
    list(set = set, get = get,
         setInvs = setInvs,
         getInvs = getInvs) # returns list of functions
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then it retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInvs() # retrieves stored cached inverse
    if(!is.null(i)) { # if cached inverse is not NULL
        message("getting cached data")
        return(i) # returns cached inverse
    }
    matrix <- x$get() # retrieves stored matrix value
    i <- solve(matrix) # calculates inverse
    x$setInvs(i) # caches inverse
    i # returns inverse
}
