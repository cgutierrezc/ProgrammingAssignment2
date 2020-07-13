## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix, gets its value
## set the value of the inverse matrix created with Solve function
## and gets its value 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## cacheSolve function calculates the inverse matrix  
## created with the makeCacheMatrix function.
## It first checks to see if the inverse matrix has already
## created. If so, it gets the inverse matrix from the cache
## and skips its creation. Otherwise, it create the
## inverse matrix of the data and sets the value of the inverse
## matrix in the cache using setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
