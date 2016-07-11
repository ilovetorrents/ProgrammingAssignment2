
makeCacheMatrix <- function(x = matrix()) {

        # This function simulates object-oriented paradigm by 
        # encapsulating a matrix (x) and providing with certain functionality for manipulating that matrix.
        #
        # Args:
        #   x: Matrix to be encapsulated.
        #
        # Returns:
        #   A list of functions that manipulate that matrix
        #       setMatrix(y) - store another matrix, matrix y, overwriting the previous one
        #       getMatrix - get the stored matrix
        #       getInverse - get the stored matrix inverse
        #       setInvers(i) - store a new value for the inverse, value i

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


cacheSolve <- function(x, ...) {

        # This function is used to calculate a matrix inverse of the CacheMatrix "object".
        #
        # It does so in a way intended to speed up the calculation by simulating caching.
        # It checks to see if the inverse has been stored, if not then calculate it, "cache" it
        # and return the inverse.
        #
        # Args:
        #   x: list returned by the makeCacheMatrix function
        #
        # Returns:
        #   value of the inverse calcualted for the matrix encapsulated within the CacehMatrix "object"


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
