#cachematrix.R ---> by peranorm

#This function creates a matrix object that can cache its inverse in the following cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  invMatrix = NULL
  set = function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get = function() x
  setinverse = function(inverse) invMatrix <<- inverse
  getinverse = function() invMatrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#This function does the actual inversion computation of the matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix = x$getinverse()
  if(!is.null(invMatrix)) {
    message("getting data that are cached")
    return(invMatrix)
  }
  data = x$get()
  invMatrix = solve(data, ...)
  x$setinverse(invMatrix)
}

##Testing with 3x3 matrix:
testmatrix = matrix(data = 1:8, nrow = 3, ncol = 3, byrow=T,
                    dimnames = list(c("row1", "row2", "row3"), c("C.1", "C.2", "C.3")))
testmatrix

cacheMatrix = makeCacheMatrix(testmatrix)
cacheMatrix$get()
cacheMatrix$getinverse()
cacheSolve(cacheMatrix)
cacheMatrix$getinverse()
