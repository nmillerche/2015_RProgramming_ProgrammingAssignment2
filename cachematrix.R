
## The following pair of functions are used to cache the inverse of a
## matrix input.

## The following function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## ensure the initial value of 'm' is NULL.
        set <- function(y) { ##set the value of the matrix object
                x <<- y
                m <<- NULL
        }
        get <- function() x ## get the value of the matrix object
        setmatrix <- function(solve) m <<- solve ## solve for the inverse function of the matrix
        getmatrix <- function() m ## get the result of the matrix inverse
        list(set = set, get = get, ## return a list containing retrieved values
             setmatrix = setmatrix,
             getmatrix = getmatrix) 
}


## The following function computes the inverse of the "matrix" returned by the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) { ##check whether 'm' has a NULL value.
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) ## sets m to the solved inverse of 'data'
        x$setmatrix(m)
        m  ## return solution (inverse) of matrix
}
