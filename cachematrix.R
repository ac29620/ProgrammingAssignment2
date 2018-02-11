makeCacheMatrix <- function(x = matrix()) {
        
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        get <- function() x
        
        setInv <- function(inv) invMatrix <<- inv
        
        getInv <- function() invMatrix
        
        list(set=set, get=get, setInv=setInv, getInv=getInv)
}

cacheSolve <- function(x) {
        
        inv <- x$getInv()
        
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        
        data <- x$get()
        
        #If missing, b is taken to be an identity matrix 
        #and solve will return the inverse of a        
        inv <- solve(data)
        
        x$setInv(inv)
        
        return(inv)
}