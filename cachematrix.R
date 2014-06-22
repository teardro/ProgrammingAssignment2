## Put comments here that give an overall description of what your
## functions do:
## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.


## Write a short comment describing this function:
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inver <-NULL
        set <- function(y){
                x <<- y
                inver <- NULL
        }
        get <- function() x
        setinver <- function(solve) inver <<- solve
        getinver <- function()inver
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinver()
        if (!is.null(inver)){
                message("getting cached inverse")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data,...)
        x$setinver(inver)
        inver
}


## function verification
test <- makeCacheMatrix(matrix(2:5, nrow=2, ncol=2,byrow=1))
cacheSolve( test )
cacheSolve( test )
