
library(ggplot2)


setLabels <- function(data)
{
    n <- nrow(data)
    p <- ncol(data)
    labels <- c()
    
    for (i in 1:n)
    {
        if (floor(data[i, 1]) %% 2 == 0 && floor(data[i, 2]) %% 2 == 0)
        {
            labels <- c(labels, -1)          
        }
        else if (floor(data[i, 1]) %% 2 == 1 && floor(data[i, 2]) %% 2 == 1)
        {
            labels <- c(labels, -1)
        }
        else if (floor(data[i, 1]) %% 2 == 1 && floor(data[i, 2]) %% 2 == 0)
        {
            labels <- c(labels, 1)      
        }
        else if (floor(data[i, 1]) %% 2 == 0 && floor(data[i, 2]) %% 2== 1)
        {
            labels <- c(labels, 1)
        }
        else
        {
            print(data[i,])
        }
    }
  
  
    return(labels)
  
}






#Returns two vectors:
#-The first one contains indexes of points in the first class.
#-The second one, the second class.
cluster <- function(features, labels)
{
      
    n <- nrow(features)
    
    classe1 <- c()
    classe2 <- c()
    
  
    for(i in 1:n)
    {
        if (labels[i] == -1)
        {
            classe1 <- c(classe1, i)         
        }
        else
        {
            classe2 <- c(classe2, i)     
        }
      
    }
  
  
    return(list(classe1, classe2))
  
}








AllDistances <- function(mat, d)
{
    n1 <- nrow(mat)
    n2 <- ncol(mat)
    dist <- matrix(rep(0, n1*n1), nrow = n1, ncol = n1)
    
    for(i in 1:n1)
    {
        for(j in i:n1)
        {
            dist[i, j] <- Kernel(mat[i, ], mat[i, ], d) + Kernel(mat[j, ], mat[j, ], d) - 2*Kernel(mat[i, ], mat[j, ], d)
                  
        }
    }
  
    
    return(dist + t(dist))
  
}  






#Computes the distances between points of different classes and returns
#a matrix containing the indexes of the first class points in the first row,
#the indexes of the second class points in the second row and the distance between these two
#points in the third row.
distances <- function(training_set, classe1, classe2, d)
{
    n1 <- length(classe1)
    n2 <- length(classe2)
    
    dist_mat <- matrix(rep(0, (3*n1*n2)), nrow = 3, ncol = n1*n2)
    
    for (i in 0:(n1-1))
    {
          i_index <- rep(classe1[(i+1)], n2)
          dist_mat[1, ((i*n2)+1):((i+1)*n2)]<- i_index
          x <- training_set[classe1[(i+1)], ]
          for(j in 1:n2)
          {
              dist_mat[2, ((i*n2)+j)] <- classe2[j] 
              y <- training_set[classe2[j], ]
              dist_mat[3, ((i*n2)+j)] <- Kernel(x, x, d) + Kernel(y, y, d) - 2*Kernel(x, y, d)
          }
    }
  
    
    dist_mat <- dist_mat[,order(dist_mat[3, ])]
    return(dist_mat)
  
}





Kernel <- function(x, y, d)
{

    result <- (x%*%y + 1)
    result <- result^d
    
  
    return(result)  
  
}




#Computes the rempirical risk over the training set where S is the reference set.
EmpiricalRisk <- function(Alldist, historical, Remp, S, training_labels)
{
  
  n <- ncol(historical)
  y1 <- S[(length(S)-1)]
  y2 <- S[length(S)]
  
  for(i in 1:n)
  {
    
    d1 <- Alldist[i, y1]
    d2 <- Alldist[i, y2]
  
    
    if(d1 < historical[2, i])
    {
      lab <- training_labels[y1]
      historical[2, i] <- d1
      Remp <- Remp - historical[3, i]
      historical[3, i] <- 0
      if (!(lab == training_labels[i]))
      {
        Remp <- Remp + 1
        historical[3, i] <- 1 
      }  
    }
    if(d2 < historical[2, i])
    {
      lab <- training_labels[y2]
      historical[2, i] <- d2
      Remp <- Remp - historical[3, i]
      historical[3, i] <- 0
      if(!(lab == training_labels[i]))
      {
        Remp <- Remp + 1
        historical[3, i] <- 1
      }      
    }   
    
  }
  
  return(list(Remp, historical))
  
  
}























Training <- function(training_set, training_labels, d)
{  
    n <- nrow(training_set)
    result <- cluster(training_set, training_labels)
    
    classe1 <- result[[1]]
    classe2 <- result[[2]]
    
    Alldist <- AllDistances(training_set, d)
    dist <- distances(training_set, classe1, classe2, d)
    S <- c(dist[1, 1], dist[2, 1])
    
    Remp <- 0
    historical <- matrix(c(seq(1:n), rep(Inf, n), rep(0, n)), nrow = 3, ncol = n, byrow = TRUE)
    r <- EmpiricalRisk(Alldist, historical, Remp, S, training_labels)
    Remp <- r[[1]]
    historical <- r[[2]]
    cpt <- 2
    while(Remp > 0)
    {
        a <- dist[1, cpt]
        b <- dist[2, cpt]
        if (!(a%in%S))
        {
            S <- c(S, a) 
        }
        if (!(b%in%S))
        {
            S <- c(S, b)
        }
      
        cpt <- cpt + 1
        r <- EmpiricalRisk(Alldist, historical, Remp, S, training_labels)
        Remp <- r[[1]]
        historical <- r[[2]]
        print(Remp)
      
    }

    final_set<- c()
    final_labels<- c()
    for(i in 1:length(S))
    {
        final_set <- rbind(final_set, training_set[S[i], ])
        final_labels <- c(final_labels, training_labels[S[i]])
    }
    
    return(list(final_set, final_labels))
}






test<- function(final_set, final_labels,  test_set, test_labels, d)
{
     error <- 0 
     n <- nrow(test_set)
     
     for(i in 1:n)
     {
        x <- test_set[i, ]
        argmin <- 0
        dist_min <- Inf
        for(j in 1:nrow(final_set))
        {
            y <- final_set[j, ]
            distance <- Kernel(x, x, d) + Kernel(y, y, d) -2*Kernel(x, y, d)
            
            if(distance < dist_min)
            {
                argmin <- j 
                dist_min <- distance
            }          
        }
       
    
        classe_x <- final_labels[argmin]
        if(!(classe_x == test_labels[i]))
        {
            error <- error + 1
        }
       
       
     }
  
  
     return(error/n)
  
}

x<-runif(1000, 0, 4)
y<-runif(1000, 0, 4)

set<-cbind(x, y)

training_set<- set[1:600, ]
test_set <- set[601:1000, ]

training_labels <- setLabels(training_set)
test_labels <- setLabels(test_set)
ptm <- proc.time()
s <- Training(training_set, training_labels, 1)
proc.time() - ptm
test(s[[1]], s[[2]], test_set, test_labels,1)





#Plot new set

frm <- data.frame(X = s[[1]][, 1], Y = s[[1]][, 2], Label = s[[2]])

p <- ggplot(data = frm, aes(x = X, y = Y, colour = factor(Label), shape = factor(Label)))
p <- p + geom_point(size=3)
print(p)



#Plot initial set

frm <- data.frame(X = training_set[, 1], Y = training_set[, 2], Label = training_labels)

p <- ggplot(data = frm, aes(x = X, y = Y, colour = factor(Label), shape = factor(Label)))
p <- p + geom_point(size=3)
print(p)
