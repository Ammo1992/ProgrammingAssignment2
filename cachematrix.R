## Put comments here that give an overall description of what your
## functions do
#The first function, makeVector creates a special "vector", 
#The second function, cacheSolve calculates the inverse of the matrix


## Write a short comment describing this function
# this function is a list containing a function to
#1.set the value of the vector
#2.get the value of the vector
#3.set the value of the mean
#4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## Write a short comment describing this function
#function calculates the inverse of the special matrix
#created with the above function. However, it first checks
#to see if the inverse has already been calculated. If so, 
#it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the 
#value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) { #checking if inverse already calculated
    message("getting cached data")
    return(m)
  }
  data <- x$get()       #loading the matrix in environment
  m <- solve(data, ...) #calculating the inverse
  x$setinv(m)
  m                     ## Return a matrix that is the inverse of 'x'
}
