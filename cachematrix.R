## Put comments here that give an overall description of what your
## functions do

## Function does the following:
# Initializes the inv_x to NULL. This holds the inverse matrix in parent environment
# sets x based on the matrix passed to function
# defines getter function for x
# defines setter function for inv_x
# defines getter function for inv_x
# returns a list which has above 4 functions

makeCacheMatrix <- function(x = matrix()) {
  
  inv_x <- NULL
  
  # Setter Function For Matrix
  setMatrix <- function(y) {
    
    x <<- y
    inv_x <<- NULL
  }
  
  getMatrix <- function() x
  
  setMatrixInv <- function(inverse) inv_x <<- inverse
  
  getMatrixInv <- function() inv_x
  
  list( setmatrix = setMatrix,
        getmatrix = getMatrix,
        setmatrixinverse = setMatrixInv,
        getmatrixinverse = getMatrixInv )

}



## CacheSolve expects an input X of type makeCacheMatrix. It cannot be of type matrix else cacheSolve will return errror
#  This function will take input makeCacheMatrix and set the inverse in parent environment


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getmatrixinverse()
  
  
  print(inv)
  
  if (!is.null(inv)) {
    message("Inverse already existed incachemtx$getmatrixinverse() cache, returning from cache")
    return(inv)
  }
  
  # If we have got here it means inverese did not exist in cache 
  message("Inverse did not exist in cache, getting matrix from parent environment")
  matrx <- x$getmatrix()
  
  message("Solving and placing inverse in parent environment")
  inv <- solve(matrx)
  x$setmatrixinverse(inv)
  inv
}

