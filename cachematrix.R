## This function creates a matrix and caches the inverse of the matrix
## Input to this function is a square matrix
## Supports the following functions
## set: sets the value of the matrix
## get: gets the value of the matrix
## setInverse: caches the inverse of the matrix computed outside this function
## getInverse: returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(matrixInv) invMatrix <<- matrixInv
  getInverse <- function() invMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the matrix if not available in cache
## Input to this function is a cached matrix
## Returns the inverse of the matrix
## If the matrix was cached then it prints a message 'getting cached data'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseM <- x$getInverse()
  if(!is.null(inverseM)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseM <- solve(data)
  x$setInverse(inverseM)
  inverseM
}

matrixElements <- sample(1:36,36)
testMatrix <- matrix(matrixElements,nrow=3,ncol=3)


cacheMatrix <- makeCacheMatrix(testMatrix)
inverseMatrix <- cacheSolve(cacheMatrix)


