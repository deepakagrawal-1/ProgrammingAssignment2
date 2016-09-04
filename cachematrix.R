## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix function takes a matrix with assumption that it is invertible matrix and sets the environment and provides setters and getters 
## for matrix and inverse matrix as detailed in the function description. This functions returns all 4 functions in a list that can be used for 
## updating matrix or recalculating matrix inverse. Key point here is that if the matrix is updated or set again then inverse is reset too and 
## inverse must be recalculated.
##
## cacheSolve function is to calculate the matrix inverse only if it has not calculated before. If the matrix inverse has alreay been calculated it just 
## returns the previosly saved version from the cache to make the processing efficient.

## Write a short comment describing this function
## This function makeCacheMatrix takes a matrix that is invertible and returns 4 functions 
## set - reset the matrix in the environment
## get - get the matrix from the environment
## setinv - inverse the matrix 
## getinv - get the inverted matrix


makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
	set <- function(y) {
		x <<- y
		xinv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) xinv <<- solve
	getinv <- function() xinv
	list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## Write a short comment describing this function
## This function gets the inverted matrix from the environment in xinv. If this is not null matrix then we already have the inverted
## matrix and can just return the inverted matrix in xinv. Otherwise, we get the matrix to be inverted in data, invert the matrix and
## set the inverted matrix in environment so that next time we will get the inverted matrix and will not have to recalculate.
## Lastly, return inverted matrix.

cacheSolve <- function(x , ...) {
        ## Return a matrix that is the inverse of 'x'
	## Get inverted Matrix
	xinv <- x$getinv()
	if (!is.null(xinv)) {
		message("getting matrix inverse from cache")
		return(xinv)
	}
	## get the matrix to be inverted
	data <- x$get()
	## get inverse of the matrix
	xinv <- solve(data, ...)
	## Save the inverted matrix in environment
	x$setinv(xinv)
	## return inverted matrix
	xinv
}
