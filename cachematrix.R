## Functions to a create special matrix and to compute its inverse. The special matrix is capable of storing its inverse.
## Computed Inverse is cached and will be computed again only if matrix has changed, else cached value is returned. 

## Creates a special matrix which is capable of storing(caching) its inverse. 
## When the matrix data changes, the inverse is reset.
makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL #Object to store inverse of the matrix
   set <- function(value) {
      x <<- value
      inverse <<- NULL ##reset the inverse since data has changed.
   }
   get <- function() x 
   setInverse <- function(inv) inverse <<- inv 
   getInverse <- function() inverse
   ##return a list object containing helper function to store and retrieve a matrix and its inverse.
   list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


##function to calculate the inverse of matrix created by function "makeCacheMatrix".
##Calculated inverse will be cached and the inverse will not be calculated if the cached inverse exists.
cacheSolve <- function(x, ...) {
   inverse <- x$getInverse()
   ##check if inverse is already cached, if inverse is already cached then
   ##return the cached inverse
   if (!is.null(inverse)) {
      message("returning cached data")
      return (inverse)
   }
   ##Inverse is not cached, compute and cache it.
   message("Computing inverse ...")
   data <- x$get()  
   inverse <- solve(data, ...)
   x$setInverse(inverse)
   
   inverse
}
