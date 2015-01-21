## Caching a matrix
## functions do

## makeCacheMatrix creates a list of functions for a matrix used to hold 
## - the matrix (get) 
## - the inverse of that matrix (getinverse).
## and also allows to set
## - the matrix (set)
## - the inverse of that matrix (setinverse).
## manually.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL                                               # initialise the variable holding the inverse
	set <- function(y) {                                    # define the set function
		x <<- y                                             # store the given matrix y in the parent environment
		i <<- NULL                                          # reset the inverse in case it is set on previous call
	}
	get <- function() x                                     # define the get function returning the default function "matrix"
	setinverse <- function(solve) i <<- solve               # define the setinverse function "solve"; store the result in the calling environment
	getinverse <- function() i                              # define the getinverse function which returns i where the inverse matix is stored
	list(set = set,                                         # return the list of defined functions
	     get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## cacheSolve uses a matrix and a set of functions to calculate and cache the inverse of that matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()                                 # calculate the inverse of x
		if (!is.null(i)) {                                  # if it is already calculated use cached value
			message("getting cached data")
			return(i)
		}
		data <- x$get()                                     # get the matrix data
		i <- solve(data, ...)                               # calculate the inverse of that matrix
		x$setinverse(i)                                     # store that result in cache
		i                                                   # return the inverse
}
