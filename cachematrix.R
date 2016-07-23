##These functions are meant to compute 

##and cache the inverse of a matrix




## makeCacheMatrix is a list of functions that will either update or

## return the value of x as well as update or return its inverse




makeCacheMatrix <- function(x = matrix()) {

  

  if(class(x) != "matrix"){

      message("please introduce a matrix")

  }else if (det(x) == 0) {

      message("matrix is not invertible")

    

  }else{

      c <- NULL

    

      set <- function(y) {

      

          if(class(y) != "matrix"){

              message("please introduce a matrix")

          }else if (det(y) == 0) {

              message("the matrix is not invertible")

        

          }else{

              x <<- y

              c <<- NULL

          }

      }

  

      get <- function() x

  

      setCache <- function(solve) c <<- solve

  

      getCache <- function() c

  

      list(set = set, get = get, 

          getCache = getCache,

          setCache = setCache)

  }

}




## cacheSolve computes the inverse of the matrix created by 

## makeCacheMatrix. If the inverse has already been calculated 

## then it will be cached




cacheSolve <- function(x, ...) {

  

  ## Return a matrix that is the inverse of 'x'

  c <- x$getCache()

  if(!is.null(c)) {

      message("getting cached data...")

      return(c)

  }

  

  data <- x$get()

  c <- solve(data)

  x$setCache(c)

  c




}