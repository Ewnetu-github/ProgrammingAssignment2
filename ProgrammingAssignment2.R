#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  getmatrix <- x$get()
  m <- solve(getmatrix, ...)
  x$setinverse(m)
  m
}
#-------------------------------------------
# checking my functions 
#-------------------------------------------
# creating an invertible matrix
v1 <- c(3, 2, 5)
v2 <- c(2, 3, 2)
v3 <- c(5, 2, 4)
mat <- rbind(v1, v2, v3)
# compute the matrix 
mat2 <- makeCacheMatrix(mat)
cacheSolve(mat2)
