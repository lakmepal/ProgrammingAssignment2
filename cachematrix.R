## This function is created as part of Assignment 2 
## Functions built to cache the inverse of a matrix rather than computing it everytime

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # 2. get the value of the matrix
  get <- function() x
  
  # 3. set the value of inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # 4. get the value of inverse of the matrix
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The below function returns the inverse of any given matrix. 
# It checks if the inverse has already been computed. 
# If so, it reuses the result and skips computation
# If not, it computes the inverse and caches it

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # Checks if the inverse has already been computed
  if(!is.null(inv)) {
    message("reading cached data.")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

# Testing the functions
#> x <- rbind(c(1:2),c(2:1))
#> x
#[,1] [,2]
#[1,]    1    2
#[2,]    2    1
#> y <- makeCacheMatrix(x)


#> cacheSolve(y)
#[,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333

# Re-running should use the cache created in the above step

#> cacheSolve(y)
#reading cached data.
#[,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333
