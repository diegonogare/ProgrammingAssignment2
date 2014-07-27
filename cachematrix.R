## This function is responsible to receive a matrix and set this value to Mat.
## next steps is define methods GET and SET, called GetSolve and SetSolve, used to 
## store result cached.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- Mat
    m <<- NULL
  }
  get <- function() x
  SetSolve <- function(solve) m <<- solve
  GetSolve <- function() m
  list(set = set, get = get, SetSolve = SetSolve, GetSolve = GetSolve)
}

## This second function inverse the matrix
## checking before processig if wasn't already inversed
cacheSolve <- function(x, ...) {
  m <- x$GetSolve()
  ## this code block check the inversed.
  if(!is.null(m)) {
    message("getting cached data [Recuperando dados do cache]") ## The message should be displayed only if the matrix alread solved.
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$SetSolve(m)
  m
}