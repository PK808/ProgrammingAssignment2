## This file contains two functions, which when used together can allow more efficient
## computation of matrix operations which require solved matrices.

## The example supplied by the assignent text needed changing of a single word to work
## with matrix solving instead of vector averaging, however, in order to be more readable
## and robust, a couple of other changes were made and the variable names all changed
## from single non-descript letters to meaningful words.

## This function takes a matrix as its argument and returns an object with 2 properties and
## 4 functions. The functions are just accessor functions for the properties of the object.

## In order to use the original matrix that was input into the function in this manner:
## monkeyMatrix <- makeCacheMatrix(aPreviouslyCraftedMatrix)
## the user would have to type monkeyMatrix$get(), which specifies the get() function
## of the matrix.

## There is a similar method for setting the matrix once it has been created:
## monkeyMatrix$set(newMatrix). This setting function has the added (necessary)
## benefit flushing the cached matrix, so there is no danger of having returning a 
## no-longer-valid cahce of the inverted matrix.

makeCacheMatrix <- function(originalMatrix = matrix()) { ## The function takes a matrix as input.
  
  solvedMatrix <- NULL ## Upon constructing this object, instantiate an empty cache property

  set <- function(y) { ## This is the setter proerty
    originalMatrix <<- y ## assign the input to the primary matrix property
    solvedMatrix <<- NULL ## Flush the cache because it is no longer valid
  }
  get <- function() { ## This is the getter function
    originalMatrix ## It returns the primary matrix property
  }
  setCache <- function(newSolvedMatrix) { ##This is the caching function
    solvedMatrix <<- newSolvedMatrix ## Set the cached matrix property to the input
  }
  getCache <- function() { ## This is the cache getter function
    solvedMatrix ## Return the cached matrix property
  }
  objectToReturn <- list(set = set, get = get, ## This list creates the actual object to return
       setCache = setCache,
       getCache = getCache)
  
  return(objectToReturn) ## Return the list object that contains the data and associated functions.
}


## This function is the effective way to get the inverse of a matrix stored in an object made by the 
## makeCacheMatrix function. The first step this function takes is to figure out whether the matrix 
## object contains a cached inverse. This is done by checking to see if the getCache() command returns
## a non-null response. If there is a repsonse, the function returns the previously inverted matrix.
## If there isn't a response, the function then calculates the inverse of the matrix, stores it in the
## given matrix object, and returns the solved matrix.

cacheSolve <- function(mToSolve, ...) { ## The function takes a "makeCachedMatrix" object as input
  
  mSolved <- mToSolve$getCache() ## mSolved now contains whatever was in the input matrix's cache
  if(!is.null(mSolved)) { ## If there was a cached inverse...
    message("data is from previously cached inverse") ## report that the data came from the cache
    return(mSolved) ## Return the cached inverse
  }
  fetchedMatrix <- mToSolve$get() ## If there was no cached matrix, get the original matrix data
  mSolved <- solve(fetchedMatrix) ## Solve the original matrix for inverse
  mToSolve$setCache(mSolved) ## Cache the newly inverted matrix in the original matrix's cache property
  return(mSolved) ## Return the inverted matrix.
}

## Use of these functions looks like:

## myMatrixObject <- makeCacheMatrix(invertableMatrix)
## myInvertedMatrix <- cacheSolve(myMatrixObject)
## myOriginalMatrix <- myMatrixObject$get()


