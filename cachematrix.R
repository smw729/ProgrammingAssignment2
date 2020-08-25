## These functions store the inverse of a matrix as an in-memory 
## object in order to avoid repeated calculations

## The function MakeCacheMatrix creates an R object that stores a 
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setsolve <- function(solve)m<<-solve
  getsolve <- function()m
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## The function cacheSolve retrieves the inverse from the cached 
## value that is stored in the makeVector object's environment

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
