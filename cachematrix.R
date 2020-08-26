##A pair of functions that cache the inverse of a matrix

##Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ##initializing inverse property
        i <- NULL 
        ##set the matrix
        set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
        }
        ##get the matrix
        get <- function() {
    	## Return the matrix
    	        m
        }
        ##set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        ##get the inverse of the matrix
        getInverse <- function() {
        ##return the inverse property
                i
        }
        ##return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


##Compute the inverse of the special matrix returned by "makeCacheMatrix".
##If the inverse has already been calculated and the matrix has not changed then the function will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
         ##return the inverse if its already set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        ##get the matrix from our object
        data <- x$get()
        ##calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        ##set the inverse to the object
        x$setInverse(m)
        ##return the matrix
        m
}
