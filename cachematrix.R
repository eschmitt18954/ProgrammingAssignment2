## The two functions makeCacheMatrix and cacheSolve are useful whenever
## in the course of developing a complex script one might need to use the
## inverse of the same matrix multiple time (ie inside a loop) without
## the overhead of recomputing it.  The two functions are used together, 
## by first calling the function makeCacheMatrix, which creates a special list
## of functions pointing to the cached solution, or the function to compute
## and of course to store in cache any newly computed solution


## makeCacheMatrix takes as it's argument x, a matrix whose inverse is desired 
## to be returned, and retruns a special list of functions to handle the caching when called
## by the function solveCache
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setsolve <- function(solve) {
    m <<- solve
  }
  
  getsolve <- function() {
    m
  }
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## cacheSolve function takes as its arguement the list returned
## by makeCacheMatrix, and returns the inverse of the original matrix x
## If the inverse had been previously calculated the cached value is returned,
## else the inverse is calculated, stored (set) in cache for future use and
## returned
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setsolve(m)
  
  m
}