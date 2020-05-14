rm(list=ls())
dimension<-2
#entering the values and creating a matrix
MakeCachematrix<-function(dimension){
  y=as.numeric(array())
  l=dimension*dimension
  cat("enter the",l,"numbers by clicking enter after every inputs")
  for (i in 1:dimension) {
  for (j in 1:dimension) {
    y[i,j]=as.numeric(readline(" "))
  }  
   }
 # z<-as.numeric(y)
 # matrix(z,dimension,dimension)
}
cachematrix<-MakeCachematrix(2)


#Function for inverse
Inverse<-function(cachematrix,dimension){
  #finding the determinant of given matrix
  det<-det(cachematrix)
  #Finding the minor of each eliments
  minor<-function(cachematrix,i,j){
    cachematrix[-i,-j]
  }
  #Creating a cofactor matrix
  Cof<-matrix(NA,dimension,dimension)
  for (i in 1:dimension) {
    for (j in 1:dimension) {
      Cof[i,j]<-(-1)^(i+j)*minor(cachematrix,i,j)
    }
  }
  Cof
  #Finding the inverse
  (1/det)*t(Cof)
}
Inverse(cachematrix,dimension = 2)

