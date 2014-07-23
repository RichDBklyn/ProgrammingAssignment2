## This code sets up a matrix, solves for its inverse 
## and stores the result in a cache.

## Create makeCacheMatrix "object" with four functions that
## set and get the values of a matrix and
## set and get the values of the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      ## define function 1
      get <- function() x
      ## define function 2
      setinverse <- function(solve) i <<- solve
      ## define function 3
      getinverse <- function() i
      ## define function 4
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}
## Solve for inverse of matrix
## after checking to see whether an inverse is already cached

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	## get inverse from the "object" x created by makeCacheMatrix
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	## if retrieved inverse is not NULL, return its value
	data <- x$get()
	## assign matrix from x to "data"
	i <- solve(data, ...)
	x$setinverse(i)
	## solve and set the inverse of the matrix in "data"
	i
	## return this solution   
}
