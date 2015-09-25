## The two functions in this file work together to create a cached matrix and it's inverse.
## makeCacheMatrix is a constructor function which takes a normal invertible matrix as an input parameter
## It returns a list of accessor functions which can be used to set and get the cached values.
##
## cacheSolve uses the accessor functions of makeCacheMatrix to retrieve the cached values, and if 
## the inverse has not yet been computed, cacheSolve will compute the matrix inverse and store that value


## makeCacheMatrix function creates a special "matrix" object that will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		# Create two cached matricies and accessor methods for those
		
		# Initialize the inverse as NULL to indicate that the it has not yet been computed
        inverse <- NULL
        
        # Define accessor methods to get and set the data matrix and inverse matrix
		# The set function will cache a new data matrix and set the inverse to NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse

		# Return a list of the accessor methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (inverse not NULL), 
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

		## Get the inverse matrix from the cache
        inverse <- x$getinverse()

		## If the inverse is not NULL, then print a message and return the inverse
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

		## The inverse needs to be calculated. Retrieve the data matrix from the cache
        data <- x$get()

		## Compute the inverse using solve() and store the value in it's cache
        inverse <- solve(data, ...)
        x$setinverse(inverse)

		## Return the inverse
        inverse
}


## This is a test method for the two functions above
inverseCacheMatrixTest <- function() {
		## construct a matrix to use for testing
		tm <- matrix(c(4,3,3,2), 2, 2)
		
		## construct the cached Matricies
		cm <- makeCacheMatrix()
		
		## Set the data matix
		cm$set(mm)
		
		## Test the get function for the data matrix
		message ("Getting the data matrix from cache")
		print(cm$get())
		
		## Verify that the inverse is NULL
		message ("Getting the inverse matrix from cache. Should be NULL")
		print(cm$getinverse())
		
		## Solve the inverse and cache it's value
		message ("Solving for the inverse and caching it value.")
		print(cacheSolve(cm))
		
		## retrieve the cached inverse
		message ("Retrieving the cached inverse.")
		cacheSolve(cm)				
}