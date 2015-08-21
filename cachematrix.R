#together the pair of functions calculates, stores, and returns the inverse value of a matrix, so that the inverse of identical matrices do not need to be recalculated each time we want to get the inverse

#creates special matrix; stores list of functions to be called in cacheSolve
makeCacheMatrix <- function(x=matrix()) {
    i <- NULL
    set <- function(y) {
        #using the special assignment sign (<<-) means x is replaced with y not just in the 
        #subordinate "set" function, but also in the main function.
        x <<- y
        i <<- NULL
    }
    
    get <- function()x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    #functions stored as a list:
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    #if the inverse of matrix x is already stored, then the function simply finds it and returns it:
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    #if the inverse of matrix x is not already stored, the function calculates the inverse of the matrix, and stores 	it using setinverse:
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
