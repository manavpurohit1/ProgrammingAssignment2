## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function
## X is of type makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getmatrixinverse()
  
  
  print(inv)
  
  if (!is.null(inv)) {
    message("Inverse already existed in cache, returning from cache")
    return(inv)
  }
  
  # If we have got here it means inverese did not exist in cache 
  message("Inverse did not existed in cache, getting matrix from parent environment")
  matrx <- x$getmatrix()
  
  message("Solving and placing inverse parent environment")
  inv <- solve(matrx)
  x$setmatrixinverse(inv)
  inv
}

