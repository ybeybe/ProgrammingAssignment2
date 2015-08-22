## The makeCacheMatrix function creates a special square matrix where the inverse of the matrix can be cached
## The cachesolve function returns a matrix that is the inverse of 'x'. If the inverse has already been cached, this function
# will retrive the inverse directly, if not, it will calcuate the inverse and cache via setInv
## These functions are useful when calculation needs to be conducted to a large object multiple times. 

## this function uses <<- to assign value to a different environment than the current environment, so that the assigned value can be 
#read by other environment (i.e. setInv, getInv)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## this function checks if the inverse has been cached, if so, it gets and returns the cached result. Otherwise, it calculates, sets and returns the inverse. 

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
