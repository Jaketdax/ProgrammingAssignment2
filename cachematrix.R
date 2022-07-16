##This first function is a list of function to, set, get the elements of the matrix and itsi nverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##Second function to calculate the inverse of the specified matrix
##If it already exists, then return the cached matrix

cacheSolve <- function(x, ...) {
  ## Return the inverse matrix of x
  m <- x$getInverse()
  if(!is.null(m)){
    ##Message saying that this matrix inverse is already in the cache
    message("getting cached data") 
    return(m) ##Returning the cached matrix
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m ##Returning the the new non cached matrix
}
