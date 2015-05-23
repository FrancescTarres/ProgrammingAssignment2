## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a function that constructs a 'special matrix' containing a 
## list to functions that implement the following operations

## set:   Sets the value of the matrix. When a new matrix is 'set' the value of
##        of the inverse is deleted.
## get:   Recovers the value of the matrix which has been stored in the parent
##        environment
## setinv:Sets the value of the inverse matrix as a variable in the parent environment
## getinv:Returns the value of the inverse matrix (m_inv) which is found in the
##        parent environment


makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y){
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m_inv <<- inverse
  getinv <- function() m_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve returns the inverse value of a matrix created with makeCacheMatrix
##    function. The function tries to find the inverse in the cache (parent environmet).
##    If the inverse has been calculated previously then it returns its value from
##    the cache (parent environment). Otherwise, if the inverse is not found
##    it computes the inverse, stores it in the cache and returns its value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getinv()
  if(!is.null(m_inv)){
    message("getting cached data")
    return(m_inv)
  }
  mat <- x$get()
  m_inv <- solve(mat,...)
  x$setinv(m_inv)
  m_inv
}
