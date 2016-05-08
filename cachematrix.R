## The "makeCacheMatrix" and "cacheSolve" functions below allow
## to cache the inverse of a matrix, so that when needed again
## the inverse matrix can be looked up in the cache rather than
## recomputed.


## makeCacheMatrix creates a list containing a function to:
##    a) set / get the value of the matrix, and
##    b) set / get the value of the corresponding inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y)   {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created with
## the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m))   {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)  ## Return a matrix that is the inverse of 'x'
	x$setinverse(m)
	m
}