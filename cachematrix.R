## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix takes a matrix object as input arg, and returns a list with
# 4 functions as the list elements, to both "get" and "set" values for the matrix and
# its inverse, if it exists.
#
# cacheSolve takes as input the object assigned in running makeCacheMatrix(x), and then
# checks a "cache" variable from he environment where makeCacheMatrix was executed to see
# if the inverse matrix calculation already exists, and if it does, returns the cached
# value. Otherwise, cacheSolve computes the inverse matrix with the solve() function.


## Write a short comment describing this function

# makeCacheMatrix returns a list with 4 functions defined, given an input of a matrix.
# If the determinant of the matrix given as input equals Zero, there is no inverse
# so the function stops with an error message.

makeCacheMatrix <- function(x = matrix()) {
  # check if determinant of matrix is 0, and stop if yes  
  dt<-det(x)
  if(dt==0) {
    m<-NULL
    inverse<-NULL
    stop ("Matrix is singular; inverse does not exist")
  }
    m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve return a matrix that is the inverse of its input "x", where x is
## the object created by assigning <- makeCacheMatrix, not the matrix object itself.
cacheSolve <- function(x, ...) {
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
