#This function creates a special "matrix" object that can store (cache) its inverse.It returns a list of functions to:
## - set a new matrix,
## - get the matrix,
## - set the inverse of the matrix,
## - get the cached inverse.

## to realise to function of makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 

    ##basic funciton define
    set <- function(y) {
        x <<- y
        inv <<- NULL  
    }
    get <- function() x

    ## set the detailed inverse 
    setInverse <- function(inverse) inv <<- inverse
    ## to get it
    getInverse <- function() inv
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## to realise to function of cachesolve
cacheSolve <- function(x, ...) {
    #to get the reverse array
    inv <- x$getInverse()

    ##judge condition whether return 
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    data <- x$get()  
    inv <- solve(data, ...)  
    x$setInverse(inv)  

    inv
}
