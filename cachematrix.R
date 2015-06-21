## This two functions calculating inverse matrix. Functions optimize computation
## by saving inverse matrix in cache. 
## This function create a specail list of functions, witch we use to save data 
## to cache. Its make no computation on data.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL   
        set <- function(y) { ## This function set m to NULL in global 
                             ## enviroment, and it can change x if we want
            x <<- y 
            m <<- NULL 
        }
        get <- function() x ## this function just return x
        setsolve <- function(solve) m <<- solve ## This function set m to solve 
                                                ## in global enviroment
        getsolve <- function() m ## this function just return m
        list(set=set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
    ## Result is four functions
}


## Function make computations of invert matrix of take data from cache. 
## Its use vector from function makeCacheMatrix as input.


cacheSolve <- function(x, ...) {
        m <- x$getsolve() ## we use getsolve() function witch retrun us m. 
                          ## For the first time it will me NULL, for other times 
                          ## it will be solved matrix
        if(!is.null(m)) {
            message("getting cached data")
            return(m) ## if we have solved matrix in cache we exit from function
        }
        
        data <- x$get() ## if we dont have solves matrix we compute it
        m <- solve(data, ...)
        x$setsolve(m) ## and put data in cache by this function
        m
}
        ## Return a matrix that is the inverse of 'x'

