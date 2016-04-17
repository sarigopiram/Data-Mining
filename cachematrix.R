## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This is a temporary matrix to facilitate caching. 
## set() will set the input matrix to the output matrix
## get() will return the same input matrix
## setInverse() is defines to null here
## getInverse() will return null

makeCacheMatrix <- function(x = matrix()) {
  
  matInv = NULL
  set = function(y) {
    x <<- y
    matInv <<- NULL
  }
  get = function() x
  setInv = function(inverse) matInv <<- inverse 
  getInv = function() matInv
  list(set=set, get=get, setInv=setInv, getInv=getInv)

}


## Write a short comment describing this function

## here we calculate the inverse of the matrix
## if Inverse matrix ix already defined, then get the cached data.


cacheSolve <- function(x, ...) {
  matInv = x$getInv()
  if (!is.null(matInv)){
    message("getting cached data")
    return(matInv)
  }
  
  mat.data = x$get()
  matInv = solve(mat.data, ...)
  
  x$setInv(matInv)
  
  return(matInv)
}



