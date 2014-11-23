## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The first function, makeCacheMatrix creates a matrix containing a function to
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the inverse matrix using function solve(matrix)
#4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The function attmpts to get inverse function from cache if available
## Otherwice inverse is calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
