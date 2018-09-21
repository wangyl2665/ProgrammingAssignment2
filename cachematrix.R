## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
        ## set inverse matrix to NULL
        ix <- NULL
        ## matrix setter:  set matrix value when makeCacheMatrix object reset 
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        ## matrix getter:  matrix x value
        get <- function() x
        ## inverse matrix setter: get return value from cacheSolve function
        setinverse <- function(inverse) ix <<- inverse
        ## inverse matrix getter : get inverse matrix value
        getinverse  <- function() ix
        ## create function list (funtion name = value) for $ access
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
        ## get makeCacheMatrix object inverse matrix 
        ix <- x$getinverse()
        ## determine if inverse matrix is NULL
        if(!is.null(ix)) {
                message("getting cached data")
                return(ix)
        }
        ## get makeCacheMatrix object  matrix x
        data <- x$get()
        ## calculate inverse matrix for x
        ix <- solve(data)
        ## set inverse matrix for makeCacheMatrix object
        x$setinverse(ix)
        ## return inverse matrix to parent environment
        ix
}
