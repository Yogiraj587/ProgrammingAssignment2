## This is assignment done by Yogiraj Bhagavatula for Assignment submission.

makeCacheMatrix <- function(x = matrix(sample(1:50,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  getsolve <- function() s
  list(set = set, get = get,
       SETsolve = SETsolve,
       GETsolve = GETsolve)
}

cacheSolve <- function(x, ...) {
  s <- x$GETsolve()
  if(!is.null(s)) {
    message("Inverse of the matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$SETsolve(s)
  s
}
