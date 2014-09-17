## Rajesh Menon rprog-007 Programming assignment 2

# makeCacheMatrix is a datastructure/function
# to store the cached result with just getters/setters.
# This function doesn't do the actual calculations per se
makeCacheMatrix <- function(x = matrix())  {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(z) m <<- z
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  
# cacheSolve is the function that does the calculation, if the result is not 
# already in the cache. If the calcualted value is already in the cache, it 
# saves CPU cycles, by returning the value from the cache
cacheSolve <- function(cache, ...) {
    m <- cache$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- cache$get()
    m <- solve(data)
    cache$setinverse(m)
    m
  }


# Test Cases
# >    source("cachematrix.R")
# 
# >    amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# >    amatrix$get()         # Returns original matrix
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# 
# >   cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
# >  amatrix$getinverse()  # Returns matrix inverse
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
# >  cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
# >    amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
# >    cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
# [,1] [,2]
# [1,] -0.13333333  0.2
# [2,]  0.01010101  0.0
# 
# >    amatrix$get()         # Returns matrix
# [,1] [,2]
# [1,]    0   99
# [2,]    5   66
# 
# >    amatrix$getinverse()  # Returns matrix inverse
# [,1] [,2]
# [1,] -0.13333333  0.2
# [2,]  0.01010101  0.0