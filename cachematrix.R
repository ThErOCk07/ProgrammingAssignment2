## A pair of functions that cache the inverse of a matrix.
## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ## returns the matrix
  get <- function(){ 
    x
  }
  ## sets the inverse of the matrix
  setInverse <- function(solveMatrix){
    inv <<- solveMatrix
  }
  ## returns the inverse of the matrix
  getInverse <- function() {
    inv
    }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## return the inverse if it's already present
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## get the matrix from our object
  data <- x$get()
  
  ## calculate the inverse of matrix
  inv <- solve(data)
  
  ## set the inverse
  x$setInverse(inv)
  
  ## return the inverse
  inv      
}