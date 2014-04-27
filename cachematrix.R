## The below functions create special matrix that can 
## calculate the matrix inversion for the first time and cache it for future use

# The makeCachematrix function will create a special type of matrix that will be called from second function

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y=matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

#This function calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}