## Christina Bevevino
## Coursera-R Programming
## Week 3, Assignment 2:  Lexical Scoping

## These functions optimize computation of inverse for very matrices by caching the 
## inverse for re-use rather than recalculating.  They demonstrate how R scoping rules 
## may be applied and capitalized upon by making data generated from within a function available 
## at the command line and for successive functions at that command line.   

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL      
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix function.
## If the inverse has already been calculated and the matrix is unchanged, then the cachesolve 
## retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {     
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m          
}
