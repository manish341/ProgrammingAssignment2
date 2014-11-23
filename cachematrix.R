## Coursera - R Programing
## Week 3 - Assignment 2
## Lexical Scoping - Cache Matrix

## The function "makeCacheMatrix()" here creates cache-able matrices, 
##  we can cache matrix inverse and retrieve the same when needed without
##  calculating it again if the matrix has not changed.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function "cacheSolve()" is used to cache the matrix inverse of
## matrices created using the "makeCacheMatrix()" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        	i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting inverse matrix..")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
       	i
}
