## Put comments here that give an overall description of what your
## functions do

## This function generates a list. 
## The variable corresponding to the function makeCacheMatrix stores 
## the specific matrix and its inverse in a cache.

 makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}

## This function reads the list generated from the previous function 
## and provides the inverse by looking if the matrix is already computed 
##via makeCacheMatrix
 
 cacheSolve <- function(x, ...) {
   m <- x$getmat()
   if(!is.null(m)) {
     message("getting cached data")
     return(m)
   }
   datamat <- x$get()
   m <- solve(datamat, ...)
   x$setmat(m)
   m
 }

