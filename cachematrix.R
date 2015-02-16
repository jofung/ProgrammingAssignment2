## These functions are used to create a matrix and cache the matrix inverse.
## This allows the matrix inverse to be only calculated once.
## The input matrix must be an invertible square matrix

## makeCacheMatrix creates a matrix and allows the matrix inverse to be stored
## and retreived

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) inv <<- solve
   getinverse <- function() inv
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
   
}


## cacheSolve checks if the matrix inverse has already been cached
## if so, it returns the cached matrix, if not, it calculates the inverse
## and stores in the cache before returning it

cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   data <- x$get()
   inv <- solve(data)
   x$setinverse(inv)
   inv
}
