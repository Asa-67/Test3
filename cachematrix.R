#This function creates a special "matrix" object that can store (cache) its inverse.It returns a list of functions to:
## - set a new matrix,
## - get the matrix,
## - set the inverse of the matrix,
## - get the cached inverse.

## to realise to function of makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
    
    set <- function(y) {
        x <<- y
        inv <<- NULL  
    }
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## to realise to function of cachesolve
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
  
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    data <- x$get()  
    inv <- solve(data, ...)  
    x$setInverse(inv)  

    inv
}
