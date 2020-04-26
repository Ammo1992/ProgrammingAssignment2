#The first function, makeVector creates a special "vector", 
#The second function, cacheSolve calculates the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                  #set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                    #get the value of the matrix
  setinv <- function(solve) m <<- solve  #set the value of the inverse matrix
  getinv <- function() m                 #get the value of the mean
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

#it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the 
#value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {                  #check to see if inverse exists in cache
    message("getting cached data")
    return(m)                        #if yes exit function after returning the inverse from the cache, else run the code given below
  }
  data <- x$get()       #loading the matrix in environment
  m <- solve(data, ...) #calculating the inverse
  x$setinv(m)           #set the value of the inverse matrix
  m                     ## Return a matrix that is the inverse of 'x'
}
