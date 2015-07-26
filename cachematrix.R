## This function calculates the inverse of a matrix and stores and caches the output


makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                        x <<- y
                        inv <<- NULL
                        
        }
        
        get <- function() x
        set_inv <- function(solve) inv <<- solve
        get_inv <- function() inv
        list(set=set,get=get,set_inv=set_inv, get_inv=get_inv)
        
}


## This function calculates the inverse of an input matrix x if the value is not already stored in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get ()
        inv <- solve(data)
        x$set_inv(inv)
        inv
}
