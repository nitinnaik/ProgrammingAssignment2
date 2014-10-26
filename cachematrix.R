## Creates a special "matrix" object that can cache its inverse
##	which is really a list containing a function to
##		1. set the value of the matrix
##		2. get the value of the matrix
##		3. set the value of the inverse
##		4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get,
		setinverse=setinverse,
		getinverse=getinverse)
}

## Returns the inverse of a Matrix
## 	1. from cache, if the inverse is already calculated and matrix is not changed
##	2. using solve() and sets the value in cache, if not
## Assumption: input matrix is square and invertible

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}