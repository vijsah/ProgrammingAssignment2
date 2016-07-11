makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  ## return: a list containing functions to
  ## 1. set the matrix
  ## 2. get the matrix
  ## 3. set the inverse
  ## 4. get the inverse
  ## this list is used as the input to cacheSolve()
  inv <- NULL
  set <- function(y) {
    # use '<<-' to assign a value to an object in an environment
    # different fromt the current environment.
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x,...) {
  ## x is the output of makeCacheMatrix()
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  #if the inverse has been already calculated
  if(!is.null(inv)) {
    # get it from the cache and skips the computation
    message("getting cached data")
    return(inv)
  }
  
  #if inverse is not calculated then, calculate it
  data <- x$get()
  inv <- solve(data, ...)
  
  # set the value of the inverse in the cache using setinverse function.
  x$setinverse(inv)
  inv
}
