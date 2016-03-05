## The makeCacheMatrix function creates a special 'matrix' object that can cache its inverse.
## This function constructs the makeCacheMatrix and sets the 'inv' matrix to be the inverse of the matrix 'x'.
## Setter and Getter functions are available for writing to and reading from 'inv': setInverse, getInverse.
## The caching operation is carried out in the cacheSolve function.

makeCacheMatrix <- function (x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special 'matrix' returned by makeCacheMatrix above.
## If the inverse has already been computed and the matrix hasn't changed (i.e. 'inv' is not NULL), then 
## the function returns the inverse from the cache (i.e. 'inv').
## For the first time invocation of cacheSolve, 'inv' is NULL and therefore, this function explicitly calls solve().
## For subsequent invocations of cacheSolve, 'inv' is NOT NULL, and inv is returned as-is (from the cache).

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)){
                message("Getting cached data...")
        }
        else{
                data <- x$get()
                inv <- solve(data,...)
                x$setInverse(inv)
        }
        inv
}
