# These functions take a square, invertable matrix and calculate the inverse and cache that value for later use.
# This allows the functions and environment of each object to be called without memory demand. 

# This function creates a matrix and initiates the matrix and solve functions.
makeCacheMatrix <- function(x = matrix()){
        m = NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_matrix <- function(solve) m <<- solve
        get_matrix <- function() m
        
        list(set = set, get = get, set_matrix = set_matrix, get_matrix = get_matrix)
}

# This function solves for the inverse and captures it in a cache. 
cacheSolve <- function(x, ...) {
        m <- x$get_matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$set_matrix(m)
        m
}

#Example calculation...

a <- makeCacheMatrix(matrix(c(-1,-2,1,1),2,2))
cacheSolve(a)

