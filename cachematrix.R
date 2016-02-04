## Little framework which creates matrices which can be cached and consumed them through
## special function calls which will use cached data if possilbe.

## Creates a cachable matrix

makeCacheMatrix <- function(m = matrix())
{
    ## returns a cachable matrix which is a copy of 'm'
    
    inv <- NULL
    
    set <- function(y)
    {
        m <<- y
        inv <<- NULL
    }
    
    get <- function() m
    
    setInv <- function(inverse) inv <<- inverse
    
    getInv <- function() inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Computes the inverse of m and adds caching capabilities to the default solve function

cacheSolve <- function(m, ...)
{
    ## Return inverse matrix of 'm'
    
    inv <- m$getInv()
    
    if(!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    
    data <- m$get()
    
    inv <- solve(data, ...)
    m$setInv(inv)
    
    inv
}
