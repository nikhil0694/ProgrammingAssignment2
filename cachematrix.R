## The following functions makeCacheMatrix and cacheSolve help in speeding 
## up the process of finding the inverse of a matrix through caching. 


##makeCacheMatrix stores a matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  inverse <- NULL
  
  setmatrix <- function(y) {
    m <<- y
    inverse <<- NULL
  }
  getmatrix <- function() m
  setinverse <- function(y) inverse <<- y
  getinverse <- function() inverse
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks if the inverse of the matrix has already been computed 
## by comparing the matrix with a previous copy of the matrix
## and if yes, returns the cached inverse.

cacheSolve <- function(x, mcm = makeCacheMatrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  if (identical(mcm$getmatrix(),x))
  {
    m <- mcm$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
  }
  mcm$setmatrix(x)
  data <- mcm$getmatrix()
  m <- solve(data)
  mcm$setinverse(m)
  m
}
