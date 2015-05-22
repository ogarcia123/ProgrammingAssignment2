makeCacheMatrix <- function(x = matrix()) {
        ## a function that creates a matrix that  can cache the inverse of it's value, 
        ## which will be computed in CacheSolve. It is set as a list (line 27) of functions
        ## to set the value of the matrix, get the value of the matrix, set the value of the inverse of the mean,
        ## and get the value of the inverse of the mean
        
        ## Args: x: an empty matrix that will house the values on which 
        ## the computations are to be done
        m <- NULL
        set <- function(y){  ## sets the value of the matrix
                x <<- y
                m <<- NULL
        }
        
        get <- function(){  ## a function that gets the value of the matrix, it's output
                x           ## is x, which was set in set         
        } 
        set_inv_mean <- function(mean) { ## a function that sets the inverse of the mean
                m <<-solve(mean)
        }
        get_inv_mean <- function(){ ## function that gets the inverse of the mean. It returns m, which
                m               ## was set by the set_inv_mean function  
        }
        list(set = set, get = get, set_inv_mean = set_inv_mean, get_inv_mean = get_inv_mean)
}


cacheSolve <- function(x, ...) {
        ## A function that calculates the mean of the above function, and then calculates the inverse of it.
        ## However, it checks to see if the inverse value of the mean has already been calculated. If it has been, it
        ## will retrieve the cached value. Otherwise it will calculate the mean of the data, compute
        ## the inverse of this square matrix, and then set it as the value in the cache, so it is
        ## available to the next time the function is run.
        
        m <- x$get_inv_mean()                    
        ## sets m as the get_inv_mean subsection of the above list. It is a subsection of the list that contains the function that gets the inverse of the mean
        if(!is.null(m)){                        
                message("getting cached data")   
                return(m)
        ## if is double negative. Basically if inverse of mean  HAS BEEN computed, and there IS a value for m, leave a message and get inv mean value from the cache         
        }
        
        data <- x$get()
        m <- mean(data, ...)
        
        x$set_inv_mean(m){
                solve(m)
        }
        

        ## If not,  Returns a matrix that is the inverse of 'x'. Gets the value of the get function and sets it as data.
        ## Means the data, and sets the function 'set_inv_mean' to equal the output of 'solve(m)' which inverts the mean
        ## the inverse of the mean is thus cached in set_inv_mean , and will be retrieved by cacheSolve in subsequent instances these 2 functions are run
}
