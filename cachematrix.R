## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly. These
## two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function that performs the following.
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of inverse of the matrix
## 4. gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) { # sets the value of the matrix
    x <<- y
    invs <<- NULL
  }
  get <- function() x # gets the value of the matrix
  setinverse <- function(inverse) invs <<- inverse # sets the value of inverse of the matrix
  getinverse <- function() invs # gets the value of inverse of the matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## `cacheSolve`: computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
# It first checks if the inverse has been computed. If it has, then it gets the result and skips 
# the computation. If has not been computed, it computes the inverse and sets the value in the 
# cache via setinverse function. This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("getting cached data.")
    return(inv)
  }
  dat <- x$get()
  invs <- solve(dat)
  x$setinverse(invs)
  invs
}
