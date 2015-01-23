
## Jeff Teeter
## Just trying to learn a little R 

## The first function, makeCacheMatric creates a special "matrix" object that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) {
	invr <- NULL 
    set <- function(y) { 
        x <<- y 
        invr <<- NULL 
    } 
    get <- function() x 
    setinverse <- function(inverse) invr <<- inverse 
    getinverse <- function() invr 
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 



## The second function, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
	invr <- x$getinverse() 
    if(!is.null(invr)) { 
		message("getting cached data.") 
        return(invr) 
	} 
    data <- x$get() 
    invr <- solve(data) 
    x$setinverse(invr) 
    invr 
}
