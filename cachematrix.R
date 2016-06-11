## Caching the values is always better than calculate it everytime
## These methods are used to cache a matrix and its inverse

## Cache the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(newMatrix) {
        x <<- newMatrix
        invMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(newInverseMatrix) invMatrix <<- newInverseMatrix
    getInverse <- function() invMatrix
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}

## The method solve the matrix and cache it

cacheSolve <- function(x, ...) {
    invMatrix <- x$getInverse()
    if(!is.null(invMatrix)) {
        message("getting cached data.")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data)
    x$setInverse(invMatrix)
    return(invMatrix)
}



## How to use it:

# > source("cachematrix.R")
# > x = rbind(rnorm(2), rnorm(2))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1]       [,2]
# [1,] -0.4047107 -1.5279713
# [2,]  0.1947928 -0.2092455
# > cacheSolve(m)
# [,1]      [,2]
# [1,] -0.5473023  3.996559
# [2,] -0.5094997 -1.058561
# > cacheSolve(m)
# getting cached data.
# [,1]      [,2]
# [1,] -0.5473023  3.996559
# [2,] -0.5094997 -1.058561