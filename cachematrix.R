## The functions below write a pair of functions that cache the inverse of a matrix.

## This first function, makeCacheMatrix creates a special "matrix", 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  #This function creates a special "matrix" object that can cache its inverse.
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function "cacheSolve" calculates the inverse of the matrix created with the above function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via 
## the setinverse function

cacheSolve <- function(x, ...) {
  #This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- solve(x$get())
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}
