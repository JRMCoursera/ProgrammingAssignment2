## The following functions define a custom matrix object, capable of 
## storing its inverse in memory.

## makeCacheMatrix:
##
## This function creates a matrix object which can cache its inverse.
##
## Four functions are defined for the matrix object:
##    1. Setting the value of the matrix
##    2. Getting the value of the matrix
##    3. Setting the value of the inverse
##    4. Getting the value of the inverse
##
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}

## cacheSolve
##
## This function calculates the inverse for a matrix defined through the 
## provided makeCacheMatrix function.  If the inverse was previously
## calculated, it is obtained from memory.  Otherwise, the inverse is
## calculated using the R solve function, then stored in memory.
##
cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
