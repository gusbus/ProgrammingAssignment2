## Coursera R Programming
##	Programming Assignment 2
##

## This function defines the sub-functions and sets the parent
## frame variables necessary to store a given matrix and cache
## the value of its inverse.  We return the list of functions
## to the calling environment.

makeCacheMatrix <- function(x = matrix()) {
	## Set local variable inv to NULL
        inv <- NULL

	## Create a function called set() that accepts
	## the given argument 'y' (which is this case
	## ends up being the 'x' argument that was
	## passed into this makeCacheMatrix function).
        set <- function(y) {
		## Set the parent frame (the environment
		## that called this makeCacheMatrix function)
		## variable called 'x' to be the matrix that
		## was passed in as an argument.
                x <<- y
		## Set the parent frame variable called 'inv'
		## to NULL
                inv <<- NULL
        }

	## Create a function called get() which returns
	## the matrix in variable 'x'
        get <- function() x

	## will take the value of the given argument
	## 'solve' and store it in the parent frame
	## variable called 'inv'
        setInverse <- function(solve) inv <<- solve

	## Create a function called getInverse() that
	## will return the value of the variable called
	## 'inv'
        getInverse <- function() inv

	## Create a list of four named functions and
	## this list will be returned which will make
	## these functions available to the calling
	## environment
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function will return the inverse of a matrix.  It will
## first look to see if the inverse has already been computed
## and stored, if so we return the cached value.  Otherwise we
## need to compute the inverse now and return that result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()

        if (!is.null(inv)) {
		## Since the value if 'inv' that was returned
		## from the call to getInverse on the 'x' matrix
		## was not NULL, it means we have already computed 
		## the inverse, so just return that value to the
		## caller.
                message("getting cached inverse")

		## Just return the already computed inverse
		## to the caller, and we are done.
                return(inv)
        }

	## The returned value for 'inv' above was NULL, this
	## means we haven't yet computed the inverse for this
	## so we will do it now.

	## First, get the matrix that we passed in as an argument
	## and store it locally
        data <- x$get()

	## Use the solve function to determine the inverse of
	## the matrix
        inv <- solve(data, ...)

	## Store the resulting inverse
        x$setInverse(inv)

	## Return the inverse of the matrix to the caller
        inv
}
