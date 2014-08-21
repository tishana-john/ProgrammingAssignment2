## This program computes and caches the inverse of a matrix. It contains
## two main functions: makeCacheMatrix and cacheSolve

##oldmat: variable used to cache the original matrix
oldmat <- NULL

##eqMat : function to check the equality of two matrices

eqMat <- function(x,y)
{ is.matrix(x) && is.matrix(y) && (dim(x)==dim(y))&& all(x==y)}

## makeCacheMatrix takes a matrix as its input
## and caches it into the variable:cache

makeCacheMatrix <- function(x = matrix()) {
    cache <<- x
}


## cacheSolve takes a matrix as the input and returns the inverse of the same.
## If the inverse of a particular matrix had been computed, the result is taken
## directly from the cache. Else, it is computed and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    if(eqMat(oldmat,x))
    {message("Getting data from cache")
     return(cache)
    }
    else
    { oldmat <<- x
      makeCacheMatrix(solve(x))
      return (solve(x))
     
    }
}
