## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	##This function creates a special "matrix" object that can cache its inverse.

        invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setInverse <- function() invMat <<- solve(x)
        getInverse <- function() invMat
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

        invMat <- x$getInverse()
        if(!is.null(invMat)) {
                message("getting cached data")
                return(invMat)
        }
        sqMat <- x$get()
        invMat <- solve(sqMat, ...)
        x$setInverse(invMat)
        invMat
}
