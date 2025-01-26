## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  # Set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getInverse <- function() inv
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## This function calculate the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
## Check if the inverse is already cached
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
 ## Compute the inverse if not cached
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
