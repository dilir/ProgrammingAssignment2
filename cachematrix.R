## makeCacheMatrix() takes matrix as an argument
## stores the matrix in cache via the function set
## setinv calculates the inverse of the matrix and returns the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(s) m <<- s
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve calculates the inverse of the matrix and stores the inverse
## via the setinv function defined in makeCacheMatrix
## if the matrix hasn't changed, it get the inverse from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

#Dilir Akhtar Khan
#Works fine with mat=matrix(1:4,2,2)
#v<-makeCacheMatrix()
#mat=matrix(1:4,2,2)
#v$set(mat)
#v$get(mat)
#cacheSolve(v)
#cacheSolve(v)
