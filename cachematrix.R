## Description of makeCacheMatrix
## cachei <- NULL *set a place for the future vector
## set <- function(y) *define the function
## get <- function() x *return the vector
## setinverse <- function(inverse) *set the inverse matrix
## getinverse <- function() cachei *get the inverse matrix
## list *get a vector with all the functions

makeCacheMatrix <- function(x = matrix()) {
        
        cachei <- NULL
        set <- function(y) {
                x <<- y
                cachei <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cachei <<- inverse
        getInverse <- function() cachei
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

## Description of cacheSolve
## Return a new matrix that is the inverse

cacheSolve <- function(x, ...) {
        cachei <- x$getInverse()
        if(!is.null(cachei)) {
                message("getting cached data")
                return(cachei)
        }
        data <- x$get()
        cachei <- solve(data, ...)
        x$setInverse(cachei)
        cachei
}