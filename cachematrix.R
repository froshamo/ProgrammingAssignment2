## This file contains a pair of functions that cache the inverse of a matrix.

## "makeCacheMatrix" function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL

	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	
	get <- function(){
		x
	}

	setinverse <- function(inv){
		inverse <<- inv
	}

	getinverse <- function(){
		inverse
	}

	list( set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse )
}


## "cacheSolve" function computes the inverse of the special "matrix" returned
## by makeCacheMatrix.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)){
		message("getting cached data")
		return(inverse)
	}
	mat <- x$get()
	inverse <- solve(mat, ...)
	x$setinverse(inverse)
	inverse
}
