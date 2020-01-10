## This file contains the methods necessary to compute the inverse of a given matrix. 
## In order to optimize for speed, the methods implement a caching mechanism that will
## cache the calculated inverse for a given matrix, such that is the matrix is given to the 
## function again, it'll bypass the calculation. 


## This function returns a list of functions that allow you to operate on the given matrix. 
## It acts similarly to a class in OO languages, in that for a given matrix, it provides
## getters and setters for the matrix and its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function() x
  setMatrixInverse <- function(inverse) i <<- inverse
  getMatrixInverse <- function() i
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
  
  
}


## This function returns the inverse of the given matrix. It is different from the standard
## solve() function in R in that it uses the above method to check if this value is already cached. 
cacheSolve <- function(x, ...) {
  i <- x$getMatrixInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  originalMatrix <- x$getMatrix()
  i <- solve(originalMatrix)
  x$setMatrixInverse(i)
  i
}
