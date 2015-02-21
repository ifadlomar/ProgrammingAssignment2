## These two functions one called makeCacheMatrix  for cashing the the value of matrinx 
##and the other one cacheSolve for calculate the inverse of the matrix

## function makeCacheMatrix take a matrix and return alist of cashed value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL  
  }
  get <- function() x 
  setinvmatrix <- function(invmatrix) m <<- invmatrix
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
  
}



## function cacheSolve calculate the inverse of the input matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix ()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvmatrix(m)
  m
}