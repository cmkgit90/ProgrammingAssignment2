
## Returns a list with four elements
## [1] sets the matrix that will be inverted
## [2] returns the matrix that will be inverted
## [3] sets the inverse in the cache
## [4] returns the inverse that has been cachedy

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse<- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## 
## Call this function with an argument that is a list
## computed with makeCacheMatrix
## If the inverse is already cached, then return it
## Otherwise, get the original matrix
## Compute the inverse
## and store in the cache
##

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
