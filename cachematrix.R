## x is set as input for matrix

## m as null value

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


##  m is set to get inverse value, please check.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting inversed matrix")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
  }
  
}

bb <- makeCacheMatrix(matrix(1:10, 2, 5))
bb$get()
bb$getInverse()

