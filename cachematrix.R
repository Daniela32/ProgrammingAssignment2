#CacheMatrix - Assignment 2 R Programming
#Function makeCacheMatrix 
#This function creates a special matrix, which is a list containing a function to 
# set the value of the matrix, get the value of the matrix,
# set the inverse of the matrix, and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y #operator << assigns a value to an object from a different environment
    invMatrix <<- NULL
  }
  getMatrix <- function() x #get the value of the matrix
  setInverse <- function(inverse) invMatrix <<- inverse #set the inverse of the matrix
  getInverse <- function() invMatrix #get the inverse of the matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,setInverse=setInverse,getInverse=getInverse)
} 
#Function cacheSolve:
#This function computes the inverse of the special matrix returned by the function makeChacheMatrix
cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  #If the the inverse has already been caclulated (and the matrix has not change), then the cacheSolve 
  #retrieves the inverse from the cache
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix) 
  }
  MatrixData <- x$getMatrix() 
  invMatrix <- solve(MatrixData, ...) #if the matrix is a square invertible matrix,
  #then solve(matrix) returns its inverse; the matrix supplied is assumed always invertible
  x$setInverse(invMatrix) 
  return(invMatrix) 
}
