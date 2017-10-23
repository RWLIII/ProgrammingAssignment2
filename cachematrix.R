## NAME: Rick Lilley
## Assignment: R Programming Assingnment #2: 
## Description: 
## Create and write a pair of functions that cache
## the inverse of a function
## ----------------------------------------------------

## Description:
## The makeCacheMatrix function creates a matrix object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##Initialize variable to store inverse
  inverseMat <- NULL
  
  ##Set function 
  set <- function(y){
    x <<- y
    inverseMat <<- NULL
  }
  
  ##get function
  get <- function() {x}
  
  ##setInverse function
  setInverse <- function(inv) {inverseMat <<- inv}
  
  ##getInverse function
  getInverse <- function() {inverseMat}
  
  ##list of all functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}
##--------------------------------------------------

## Description:
## This Function computes the inverse fo the matrix 
## returned by cacheMatrix. If the inverse has been
## solved, it returns the inverse from the cache


cacheSolve <- function(x = matrix(), ...) {
  ##initialize storage matrix
  inv <- x$getInverse()
  
  ##check to see if the inverse is in cache
  if(!is.null(inv)) {
    message("Retrieving cached matrix")
    return(inv)
  }
  
  ##if the inverse does not exist, solve for it
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
##---------------------------------------------------