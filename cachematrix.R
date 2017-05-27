## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}


makeCacheMatrix <- function(x = numeric()) { 		#creates the cache which is a list of functions set/get/setinv/getinv. Argument = input matrix
        #m <- NULL
        set <- function(y) {				#updates the value of input matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x				#gives as output the input matrix
        setinv <- function(inv) {m <<- inv}		#the inverse matrix of the input matrix is saved here "in cache", to be extracted when needed with getinv()
        getinv <- function() m				#reads and outputs the inverse matrix from the "cache"
        list(set = set, get = get,			#this list defines the output of the function makeCacheMatrix
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {			#outputs the inverse of the input matrix by either reading it from cache (makeCacheMatrix) or computing it if not available in cache.
        m <- makeCacheMatrix(x)$getinv()
        if(!is.null(m)) {
                message("getting cached data")       #generates diagnostic msg; different from print()!
                return(m)
        }
        data <- makeCacheMatrix(x)$get()
        unitary <- diag(x = 1, nrow=nrow(data), ncol=ncol(data))
        m <- solve(data, unitary, ...)
        makeCacheMatrix(x)$setinv(m)    #saves inv into cache
        print(makeCacheMatrix(x)$getinv())
        m
}