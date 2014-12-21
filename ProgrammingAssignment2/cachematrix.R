## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#   The first function, makeCacheMatrix creates a special "matrix", 
#   which is really a list containing a function to
#   
#   set the value of the matrix
#   get the value of the matrix
#   set the value of the inverse of the matrix
#   get the value of the inverse of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
    setinvMatrix <- function(invMatrix) inv <<- invMatrix
  getinvMatrix <- function() inv
  list(set = set, get = get,
       setinvMatrix = setinvMatrix,
       getinvMatrix = getinvMatrix)

}


## Write a short comment describing this function
#The following function calculates the inverse matrix
#it first checks to see if the inverse has already been calculated. 
#If so, it gets the cache and skips the computation. 
#Otherwise, it computesinverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvMatrix(m)
  m
}
#Test Run
# > x=matrix(c(2,4,0,7,1,9,2,7,1),nrow=3,ncol=3)
# > m=makeCacheMatrix(x)
# > m$get()
# [,1] [,2] [,3]
# [1,]    2    7    2
# [2,]    4    1    7
# [3,]    0    9    1
# > cacheSolve(m)
# [,1]    [,2]    [,3]
# [1,]  0.775 -0.1375 -0.5875
# [2,]  0.050 -0.0250  0.0750
# [3,] -0.450  0.2250  0.3250
# > cacheSolve(m)
# getting cached data
# [,1]    [,2]    [,3]
# [1,]  0.775 -0.1375 -0.5875
# [2,]  0.050 -0.0250  0.0750
# [3,] -0.450  0.2250  0.3250