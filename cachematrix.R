## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) { 		# a list of 4 functions, taking a matrix as argument
        m <- NULL					# this line resets the value of m when function is called
        set <- function(y) {				# this function sets the argument as the matrix used
                x <<- y
                m <<- NULL
        }
        get <- function() x				# this function returns the matrix used
        setsolve <- function(solve) m <<- solve		# this function sets the inverse of the matrix used
        getsolve <- function() m			# this function returns the inverse of the matrix used
        list(set = set, get = get,			# lists the 4 functions in the overarching function
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) {			# returns the inverse of the matrix used
        m <- x$getsolve()				# check if inverse is already known
        if(!is.null(m)) {				# if inverse already known,
                message("getting cached data")		# 	prints a warning saying so and
                return(m)				#	returns that inverse and exits function
        }
        data <- x$get()					# if not, matrix is pulled then
        m <- solve(data, ...)				#	the inverse is resolved then
        x$setsolve(m)					#	that inverse is stored in memory then
        m						#	printed
}
