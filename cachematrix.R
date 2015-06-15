## makeCacheMatrix(x) is a function that caches a matrix named x 
## and its inverse.

## There are 4 subfunctions that can be called for x:
## 1. get - to display the cached matrix.
##      syntax: a$get() , where a is the assigned name of the cached matrix
## 2. set - to change the cached matrix.
##      syntax: a$set(y) , where y is the new matrix to be cached.
## 3. getInv - to display the inverse matrix of x, NULL if not computed yet.
##      syntax: a$getInv()
## 4. setInv - to change/set the cached inverse matrix
##      syntax: a$setInv(z) , where z is the new matrix to be set as the inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        Inv <- NULL
        set <- function(y)
        {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setInv <- function(newInv) Inv <<- newInv
        getInv <- function() Inv
        list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
}


## cacheSolve is a function that calculates the 
## inverse of the matrix cached using makeCacheMatrix function.

## First, it checks if there's a stored inverse. 
## If there is none, it calculates, displays and caches the inverse of the cached matrix;
## Else it fetches the cached inverse and displays it.

cacheSolve <- function(x, ...) 
{
        Inv <- x$getInv()
        if(!is.null(Inv)) 
                {
                  message("getting cached data...")
                  message(" ")
                  return(Inv)
                }
        Mat <- x$get()
        Inv <- solve(Mat)
        x$setInv(Inv)
        Inv
}
