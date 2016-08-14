##The function, makeCacheMatrix creates a special "matrix", which 
  ## sets the value of the matrix
  ## gets the value of the matrix
  ## sets the inverse of the matrix
  ## gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function calculates inverse of a matrix
## It checks to see if the inverse already exists.
## If it does it returns form the cache. 
## Else it calculates the inverse and returns it

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    
    if (!is.null(inv)) {
        message("getting cached data")
        
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data, ...)
    
    x$setinverse(inv)
    
    inv
}