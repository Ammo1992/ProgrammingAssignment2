#The first function, makeCacheMatrix creates a special "matrix", 
#The second function, cacheSolve calculates the inverse of the matrix

#First function
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {                  #function to set the value of the matrix
    x <<- y
    m <<- NULL
  }
  
  get <- function() x                    #function to get the value of the matrix
  
  setinv <- function(solve) m <<- solve  #function to set the value of the inverse matrix
  
  getinv <- function() m                 #function to get the value of the mean
  
  list(set = set, get = get,             #return a list for easy access of data.
       setinv = setinv,
       getinv = getinv)

}

#Second function
cacheSolve <- function(x, ...) {

  m <- x$getinv()
  
  if(!is.null(m)) {                  #check to see if inverse exists in cache
    message("getting cached data")
    return(m)                        #if yes return the inverse and exit function, else run the code given below
  }
  
  data <- x$get()       #load the matrix in environment
  
  m <- solve(data, ...) #calculate the inverse and store in m variable
  
  x$setinv(m)           #set the value of the inverse matrix 
  
  m                     ## Return a matrix that is the inverse of 'x'

}
