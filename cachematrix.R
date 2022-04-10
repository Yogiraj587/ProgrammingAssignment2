## This is assignment done by Yogiraj Bhagavatula for Assignment submission.

## Description about makeCachematrix
## This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  get <- function() x
  SETINVERSE <- function(inverse) Inverse <<- inverse
  GETINVERSE <- function() Inverse

  list(set = set, get = get,
       SETINVERSE = SETINVERSE,
       GETINVERSE = GETINVERSE)
}

## Desciption about cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  Inverse <- x$GETINVERSE()
  if(!is.null(Inverse)) {
    message("Inverse of the matrix")
    return(Inverse)
  }
  data <- x$get()
  Inverse <- solve(data, ...)
  x$SETINVERSE(Inverse)
  Inverse
}

### Testing the code
## m<-matrix(rnorm(1:25),5,5)
## n<-makeCacheMatrix(m)
## cacheSolve(n)

## This will return the inverse from the cache

