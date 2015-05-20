## These functions are to solve the problems in week 3 of the R programming course on coursera
## They are calculating and caching an inverse matrix

## Test instructions are included at the bottom of the code

## The first function is to set up an empty matrix as a repository
## it will provide a cache for an inverse matrix and hold it there to be called if necessary

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve, getsolve = getsolve)
}

## The second function is going to provide the solution (the inverse of a matrix) by:
## 1) Checking to see if a solution exists in the cache and fetching it or by
## 2) Calculating the solution if there is not one in the cache

cacheSolve <- function(x = matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    invmatrix <- x$get()
    m <- solve(invmatrix, ...)
    x$setsolve(m)
    m
  }

## Test instructions:
## 1) Make a matrix that has an inverse - I used "x <- matrix(c(2,2,3,2), 2, 2)"
## 2) Cache it - I used "x2 <- makeCacheMatrix(x)"
## 3) Call it - "cacheSolve(x2)"
