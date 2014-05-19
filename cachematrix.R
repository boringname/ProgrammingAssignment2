## The function makeCacheMatrix sets up a cache to store previous calculations
## The function cacheSolve first checks the cache to see if the matrix has been previously solved and
## if not , solves it and adds it to the cache

# Sets up empty cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsol <- function(solve) m <<- solve
  getsol <- function() m
  list(set = set, get = get,
       setsol = setsol,
       getsol = getsol)
}

# Checks cache; if empty solves matrix and updates cache, if present, uses cached value
cacheSolve <- function(x, ...) {
  m <- x$getsol()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } else {
  data <- x$get()
  m <- solve(data)  ## Return a matrix that is the inverse of 'x'
  x$setsol(m) ## Return a matrix that is the inverse of 'x'
  m ## Return a matrix that is the inverse of 'x'
  }
}