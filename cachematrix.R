## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	##This function creates a special "matrix" object that can cache its inverse.

	invMat <- NULL

	##set the value of the vector
	set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
	
	##get the value of the vector
	get <- function() x
      
	##set the value of the invere
	setInverse <- function() invMat <<- solve(x)
      
	##get the value of the invere
	getInverse <- function() invMat
      list(set = set, get = get,
      setInverse = setInverse,
      getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	## Gets the value of the inverse
	invMat <- x$getInverse()
      
	## If it isn't null, get the data from cache
	if(!is.null(invMat)) {
		message("getting cached data")
            return(invMat)
     	}
      ## Since it is not cached, recalculate the inverse
	sqMat <- x$get()
      invMat <- solve(sqMat, ...)
      x$setInverse(invMat)
      invMat
}
