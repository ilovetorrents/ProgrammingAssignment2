## A couple of functions that simulate 
## caching (of a matrix inverse)

## Not much to say about this function
## It is pretty much identical to the one that
## the professors provided as an example

## create 4 functions and put them in a list
## in the end return that list

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setMatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(i) inverse <<- i
        getInverse <- function() inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Again, not much going here either except that solve function is used
## and not mean in order to calculate the inverse

## check if there's a cached value
## if yes then return that value
## if not retrieve the matrix, calculate the inverse and cache it
## return the inverse

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        matrix <- x$getMatrix()
        inverse <- solve(a = matrix, ... = ...)
        x$setInverse(inverse)
        inverse
}
