## Programming Assignment 2, R https://class.coursera.org/rprog-014/ 
## 2015-05-24 https://github.com/philpham00/ProgrammingAssignment2 
## Caching the Inverse of a Matrix
## If X is a square invertible matrix, then solve(X) returns its inverse. 
## For this assume the matrix supplied is always invertible. 


## This function creates a special "matrix" object that can cache its inverse.
## utilize <<- operator to assign value to an obj in an env different from 
## current env.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}
