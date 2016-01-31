## Put comments here that give an overall description of what your
## functions do

## Get a matrix. See if it is the same as the cached (existing) matrix
## If new matrix, then calculate inverse. Update the cached value of inverse and matrix.
## If same as old matrix, do nothing

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function() inv <<- solve(x) #calculate the inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function can cache its inverse.
cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
        ## Remember x is 'fed in' to this function (is an input)
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Returning data from cache, rather than computing..")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        ## And the inverse of x, which is 'inv is 'fed out' of this function (is output)
        x$setInverse() 
        inv
}
