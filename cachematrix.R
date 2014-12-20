# Comments guidelines from https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
makeCacheMatrix <- function(m = matrix()) {
		# Creates a special matrix object that can cache its inverse.
		#
		# Args:
		#   m: Matrix whose inverse is to be cached
		#
		# Returns:
		#   List of getter and setter functions for the matrix
        inv <- NULL
		# Sets the given matrix to the internal matrix.
		# Inverse is set to null because a new matrix is being set.
        setMatrix <- function(y) {
				# <<- is used because the variable m is not in the scope
				# of this function
                m <<- y
                inv <<- NULL
        }
        getMatrix <- function() {
                m
        }
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        getInverse <- function() {
                inv
        }
		#List of setter and getter functions returned
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
		# Creates inverse of a matrix. Inverse is computed only one time.
		# Subsequent calls will return the cached Inverse.
		#
		# Args:
		#   x: Matrix object/functions created by makeCacheMatrix
		#
		# Returns:
		#   Inverse of the matrix in the special matric object
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
				# Return the inverse when its in the cache
                message("Getting cached inverse")
                return(inv) ## Return from the cache
        }
        
        data <- x$getMatrix()
		# Compute the inverse when the cached inverse is null.
        inv <- solve(data, ...)
        x$setInverse(inv) ## Cache the inverse
        inv
}
