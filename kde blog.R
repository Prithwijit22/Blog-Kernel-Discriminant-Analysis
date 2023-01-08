library(ggplot2)
library(mvtnorm)
library(ks)
library(caret)
###################### Train Data ############################
set.seed(1144)
x1 = round(rmvnorm(1000,c(1,1),diag(2,2)),2)
x2 = round(rmvnorm(1000,c(4,5),diag(0.5,2)),2)

D_Train = rbind(x1,x2)
lebel = c(replicate(1000,"blue"),replicate(1000,"red"))
D_Train = cbind(D_Train,lebel)
colnames(D_Train) = c("x1","x2","Lebel")
D_Train = data.frame(D_Train)

p1 = ggplot(data = D_Train,mapping = aes(x = as.numeric(x1),y = as.numeric(x2)))+geom_point(color = lebel)
p1 + annotate("text",x = 1,y = 1,label = "class : 0",lwd = 20) + 
  annotate("text",x = 4,y = 5,label = "class : 1",lwd = 20) + 
  labs(x = "x1",y = "x2")
  ggtitle("Train Data Points")


####################### Analysis ###############################
hx1 = Hns.diag(x1)
hx2 = Hns.diag(x2)
f_0k = function(x,n)
{
  t = NULL
  for(i in 1:n)
    t[i] = dmvnorm(x,mean = x1[i,],sigma = hx1)
  return(mean(t)/prod(diag(hx1)))                                                
}
f_1k = function(x,n)
{
  t = NULL
  for(i in 1:n)
    t[i] = dmvnorm(x,mean = x2[i,],sigma = hx2)
  return(mean(t)/prod(diag(hx2)))                                                
}


######################### Test Data #############################
set.seed(2324)
w1 = round(rmvnorm(100,c(1,1),diag(2,2)),2)
w2 = round(rmvnorm(100,c(4,5),diag(0.5,2)),2)

D_Test = rbind(w1,w2)
lebel_test = c(replicate(100,"blue"),replicate(100,"red"))
D_Test = cbind(D_Test,lebel_test)
colnames(D_Test) = c("x1","x2","Lebel")
D_Test = data.frame(D_Test)

p2 = ggplot(data = D_Test,mapping = aes(x = as.numeric(x1),y = as.numeric(x2)))+geom_point(color = lebel_test)
p2 + annotate("text",x = 1,y = 1,label = "class : 0",lwd = 20) + 
    annotate("text",x = 4,y = 5,label = "class : 1",lwd = 20)+
  labs(x = "x1",y = "x2")+ggtitle("Test Data Points")




######################## Prediction ############################
t = NULL
n = 1000
for (i in 1:200)
{
  t[i] = f_0k(as.numeric(unlist(D_Test[i,c(1,2)])),n)/(f_0k(as.numeric(unlist(D_Test[i,c(1,2)])),n)+f_1k(as.numeric(unlist(D_Test[i,c(1,2)])),n))
}
pred = ifelse(t>0.5,0,1)
y_test = ifelse(lebel_test=="blue",0,1)
confusionMatrix(table(pred,y_test))


addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}


X1 = seq(min(as.numeric(D_Test[,1])),max(as.numeric(D_Test[,1])),length.out = 40)
X2 = seq(min(as.numeric(D_Test[,2])),max(as.numeric(D_Test[,2])),length.out = 40)

pre = function(x)
{
  return(f_0k(x,1000)/(f_0k(x,1000)+f_1k(x,1000)))
}
E =  expand.grid(X1,X2)
func = apply(X = E,FUN = pre,MARGIN = 1)
pred2 = ifelse(lebel_test=="blue","darkgreen","orange")
pred1 = ifelse(func>0.5,"green","yellow")
plot(E,col =addTrans(pred1,60),pch = 15,cex = 4,main = paste("Final plot with Decision Boundary"),xlab = "x1",ylab = "x2")
points(D_Test[,1],D_Test[,2],col = pred2,pch = 20, cex = 2)


















I've got a series of modelled class labels from the knn function. I've got a data frame with basic numeric training data, and another data frame for test data. How would I go about drawing a decision boundary for the returned values from the knn function? I'll have to replicate my findings on a locked-down machine, so please limit the use of 3rd party libraries if possible.

I only have two class labels, "orange" and "blue". They're plotted on a simple 2D plot with the training data. Again, I just want to draw a boundary around the results from the knn function.

Code:
  
  library(class)

n <- 100

set.seed(1)
x <- round(runif(n, 1, n))
set.seed(2)
y <- round(runif(n, 1, n))
train.df <- data.frame(x, y)

set.seed(1)
x.test <- round(runif(n, 1, n))
set.seed(2)
y.test <- round(runif(n, 1, n))
test.df <- data.frame(x.test, y.test)

k <- knn(train.df, test.df, classes, k=25)

plot(test.df, col=k)


# class labels: simple distance from origin
classes <- ifelse(x^2 + y^2 > 60^2, "blue", "orange")
classes.test <- ifelse(x.test^2 + y.test^2 > 60^2, "blue", "orange")

grid <- expand.grid(x=1:100, y=1:100)
classes.grid <- knn(train.df, grid, classes, k=25, prob=TRUE)  # note last argument
prob.grid <- attr(classes.grid, "prob")
prob.grid <- ifelse(classes.grid == "blue", prob.grid, 1 - prob.grid)

# plot the boundary
contour(x=1:100, y=1:100, z=matrix(prob.grid, nrow=100), levels=0.5,
        col="grey", drawlabels=FALSE, lwd=2)
# add points from test dataset
points(test.df, col=classes.test)










