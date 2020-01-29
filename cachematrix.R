## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function (makeCacheMatrix) will create a special matrix which will
## then cache it's inverse for the input

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) m <<- inverse
  getInv <- function()m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

## The cacheSolve function will compute the inverse of the special matrix above
## should the inverse had already been calculated, cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message ("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInv(m)
  m
}
