## makeVector creates a special "vector," a list containing a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

# makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# Follow the same format provided by example function, except dealing with cache and inverse.  

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL                                         # Initialize at NULL for first run.  
        set <- function(y) {                                    # Create set function to store the cache matrix.
                cache_x <<- y                                  
                cache_m <<- NULL                                        
        }
        get <- function() cache_x                               # Get the matrix but not inverse.
        set_cache_m <- function(inverse) cache_m <<- inverse    # Get the inverse. Create function to set the value of cache_m in cache to the value of inverse.        
        get_cache_m <- function() cache_m                       # Check for NULL. Create list.
        list(set = set, get = get,
             set_cache_m = set_cache_m,
             get_cache_m = get_cache_m)
}

# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# cacheSolve returns the inverted form of the submitted matrix.

cacheSolve <- function(x) {                     # Receive makeCacheMatrix.
        inverse <- x$get_cache_m()              # Inverse the m cache value.
        if(!is.null(inverse)) {                 # See if m is NULL.  
                message("getting cached data")  # Return the value of m with a message if NULL.
                return(inverse)
        }                                      
        first_matrix <- x$get()                # Call the nested function x$get and assign it to first_matrix.                         
        last_matrix <- solve(first_matrix)     # Invert first_matrix with solve function and assign the result to last_matrix.
        x$set_cache_m(last_matrix)             # Call nested function x$set_cache_m() in makeCacheMatrix.
        last_matrix                            # Return last_matrix as result.
}
