# Description of file:
# These functions are used to compute and cache the inverse  
# of a matrix or use previously created cache 
# if matrix has not changed

# This function creates a special "matrix" object that can cache 
# its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function(){
        x
    }
    setsolve <- function(solve){ 
        s <<- solve
    }
    getsolve <- function(){ 
        s
    }
    list(
        set = set, 
        get = get,
        setsolve = setsolve,
        getsolve = getsolve
    )
}

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }else{
        message("no cache found")
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

# # tests
# 
# cacheMatrix <- makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,4,0), 3,3))
# cacheMatrix$get()
# 
# cacheSolve(cacheMatrix) # should compute
# cacheSolve(cacheMatrix) # should return cache
# 
# cacheMatrix$set(matrix(1:4,2,2))
# cacheMatrix$get()
# 
# cacheSolve(cacheMatrix) # should compute
# cacheSolve(cacheMatrix) # should return cache




