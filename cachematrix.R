## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse
## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix above

## makeCacheMatrix creates a special "matrix" object containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from t

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
