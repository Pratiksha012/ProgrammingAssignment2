##This function will create a matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) { ##defining argument with default mode of matrix"
        inv<-NULL ##initializing inverse as NULL
        set<-function(y){ ##defining set function
                x<<-y
                inv<<-NULL ##resetting inverse to NULL
                }
        get<-function()x ##defining get function to get matrix x
        setinverse<-function(inverse) inv<<-inverse ##assigns the value of inverse
        getinverse<-function()inv ##gets the value of inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
##This function calculates the inverse that is returned by makeCacheMatrix 
##If the inverse is already calculated then cacheSolve will get the inverse from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)) {
                message("retrieving the cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data, ...)
        x$setinverse(inv)
        inv
}
