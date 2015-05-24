# Steps to use following functions
# Create Matrix 
# sample <- makeCacheMatrix(matrix(c(4,3,3,2), nrow = 2, ncol =2))
# cacheSolve(sample) This step would calculate cache
# Now whenever we use cacheSolve(sample)it would use cache value

# makeCacheMatrix will provide a list of functions. 
# A list where each element is a function as described below
# get: Gets the stored matrix
# set: Sets the stored matrix
# getInverse: Gets the cached inverse of matrix 
# setInverse: Sets the cached for inverse of matrix
makeCacheMatrix <- function(x = numeric()) {
  # Initially we do not have inverse value and so it is set to null. 
  inv <- NULL
  # Stores matrix data 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Returns matrix data
  get <- function() x
  # Cache inverse of matrix passed as argument
  setInverse <- function(solve) inv <<- solve
  # Return cached inverse value of matrix
  getInverse <- function() inv
  # Returns a list of functions. Basically each element of list is function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# cacheSolve function calculates the inverse of a matrix. 
# Matrix would be created by function makeCacheMatrix 
# Function first checks if inverse is already calculated and cached
# If inverse is not calculated, it will and cache it for future reference

cacheSolve <- function(x, ...) {
  # Retrieves cache inverse value
  inv <- x$getInverse()
  # Validates if cache for value is empty or not.
  # If cache is not found we will calculate it
  if(!is.null(inv)) {
    # If cache is found, we will return cache data
    message("getting cached data")
    return(inv)
  }
  # Get stored matrix
  data <- x$get()
  # Caluclate inverse of matrix
  inv <- solve(data)
  # As we have calculated value of matrix, lets cache it for future reference
  x$setInverse(inv)
  # Return inverse 
  inv
}