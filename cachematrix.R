## Matrix inversion is consideres as costly operation in term of somputing resources.
## The following functions will create a matrix with capability to cache of its inversion.
## In case of first call ot will compute the matrix inversion & cache it.
## On next call onwards it will use it cache result instead of computing
## Assumption: 1. Input matrix should be always square.
## Assumption: 2. The input matrix should not be singular(Inverse operation as determinent will be 1/0).
## In case of Not square or singularity it will show the message "Either function is not square or it is singular matrix".
## For more detail of inevrse limitation check below link https://www.mathsisfun.com/algebra/matrix-inverse.html

## This function creates a special "matrix" object from input matrix that can cache its inverse.
##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix to other or same depending on input & reset inverse variable to null
##get the value of the matrix
##setInverse the value of the Inverse
##getInvese the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
		I<-NULL
		set <- function(y) {
				x <<- y
				I <<- NULL
			}
		get <- function() x
		setInverse <- function(inverse) I <<- inverse
		getInverse <- function() I
		list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}



##The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
		#solve() calulate the inverse.But if the matrix is not square or it is singular it throws error.tryCatch() catch the error and publish
		#customized message
        I <-tryCatch(solve(data),error=function(e){"Either function is not square or it is singular matrix"})
        x$setInverse(I)
        I
}