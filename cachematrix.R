## Cache the inverse of a matrix for later usage

## Setup the enviroment to cache value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Return a matrix that is the inverse of 'x'
## If the value was cached,use the cached value. 
## If not calculate the value using solve().

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  # return cached value if exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Calculate the inverse using solve()
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
