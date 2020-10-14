# MakeCacheMatrix stores the matrix and its inverse into a cache while the cacheSolve retrieves the va;ue of the inverse out of the cache 


#function that sets and gets the "matrix inputted"special" matrix and also set and gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        set_matrix <- function(y) { #setting the value of the matrix that was inputted
                x <<- y
                i <<- NULL
        }
        get_matrix <- function() x #getting the values of the matrix that was inputted
        set_inverse <- function(inverse) i <<- inverse #a function setting the value of the inverse
        get_inverse <- function() i #a function for getting the value of the inverse 
        
        #storing the values into a cache
        list(set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse) 
}

#solving the value of the inverse of the "special" matrix
cacheSolve <- function(x, ...) {
        i <- x$get_inverse() #getting the inverse of the value out of cache
        
        if (!is.null(i)) { #if there is already a cached data, it will generate a message and return the value of the inverse
                message("getting cached data")
                return(i)
        }
        
        matrix <- x$get_matrix() #getting the matrix out of cache
        i <- solve(matrix, ...) #solving the inverse of the matrix
        x$set_inverse(i)
        i #returning the value of the inverse of the matrix
}

x <- matrix(1:4, 2, 2) #creating a matrix
x #printing matrix
a <- makeCacheMatrix(x) #inputting the matrix into the cache 
cacheSolve(a) #solving and printing the inverse of the "matrix x"
