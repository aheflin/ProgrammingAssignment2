## It is good to cache results so that your function doesnt have to rerun large data sets.
##My functions will allow me to cache the inverse of a matrix.

## This is my function to store my data

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setInverse = function(inverse) inv <<- inverse
        getInverse = function() inv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## This function returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        inv = x$getInverse()
        if (!is.null(inv)){
                message("Retrieving Data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
