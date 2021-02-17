# makeCacheMatrix makes a special matrix that contains four properties to:
# set the value of the matrix
# get the value of the matrix
# set the inverse of the matrix
# get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinvmatrix <- function(solve) m <<- solve
  getinvmatrix <- function() m
  
  list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}


# cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix. 
# However, it first checks to see if the inverse of the matrix has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
# the matrix of the data and sets the value of the matrix inverse in the cache via the setinvmatrix
# function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  x$setinvmatrix(m)
  m
}

# --------------------------------------------
# some tests
#--------------------------------------------

s = c(1,0,5,2,1,6,3,4,0)
m1 = matrix(data = s, nrow = 3, byrow = T)
print(m1)

(m1_inv = solve(m1) )
m1 %*% m1_inv

s2= c(3,2,-3,-1,0,1,1,4,2)
m2 = matrix(data = s2, ncol = 3, byrow = T)
print(m2)

(m2_inv = solve(m2) )
m2 %*% m2_inv

m = makeCacheMatrix(m1)
m$get()
m$set(m2)

cacheSolve(m)
