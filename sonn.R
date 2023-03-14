#Defining & Viewing Objects

#Vector

a=c(4,9,1,21,6,5,10,12,8,3,2,19,17,20)

a=c('a','b','c')

b=c(1:500000)

#List
a=list('a',4,TRUE)


#Matrix
c=matrix(c(2,2,2,2,2,2,2,2,2),nrow = 3,ncol = 3,byrow = TRUE)
d=matrix(c(1:9),nrow = 3,ncol = 3,byrow = FALSE)
c
d

#Plain Multiplication
c*d

#Matrix Multiplication
c%*%d


#Array
m1=array(c(1:9),dim = c(3,3,1))
m1
m2=array(c(1:15),dim = c(3,3,2))
m2

#Data Frame
a=data.frame(Name=c('Ruby','Jessy','Wilson'),Weight=c(48,65,78))
a


#Function to find the sum of first 100 natural numbers
sumn = function(n){
  result = 0
  
  for(i in 1:n){
    result = result + i
  }
  
  return(result)
}

print(sumn(100))



#Function for sorting a vector
a=c(4,9,1,21,6,5,10,12,8,3,2,19,17,20)

sortVec = function(x){
  
  n = length(x)
  
  for(i in 1:n){
    for(j in 1:n){
      if(x[j]>x[i]){
        t=x[j]
        x[j]=x[i]
        x[i]=t
      }
    }
  }
  return(x)
}

print(sortVec(a))



#function to find median from a frequency distribution table
l = c(0, 10, 20, 30, 40)
u = c(10, 20, 30, 40, 50)
f = c(5, 7, 17, 4, 3)

medianFq = function(x, y, z){
  n = sum(z)
  h = y[1] - x[1]
  cf = c()
  cf_sum = 0
  
  for(i in f){
    cf_sum = cf_sum + i
    cf = c(cf,cf_sum)
  }  
  
  print(cf)
  
  for(i in cf){
    if(i>(n/2)){
      mc_index = which(cf==i)
      break
    }
  }
  
  med = l[mc_index] + ((((n/2)-cf[mc_index-1])*h)/f[mc_index])
  return(med)
}


print(medianFq(l, u, f))



#function to find mode from freq. dist. table
l = c(0, 10, 20, 30, 40)
u = c(10, 20, 30, 40, 50)
f = c(5, 7, 17, 4, 3)

modeFq = function(l, u, f){
  h = u[1]-l[1]
  mc_index = 0
  max_f=0
  
  for (i in f){
    if(i>max_f){
      max_f=i
      mc_index=which(f==i)
    }
  }
  
  md = l[mc_index] + (h*((f[mc_index]-f[mc_index-1])/((2*f[mc_index])-f[mc_index-1]-f[mc_index+1])))
  
  return(md)
}

print(modeFq(l,u,f))


#creating pie chart from distribution

Heads=c('Education','Rent','Savings','Food')
Amount=c(20000, 35000, 24000, 15000)
data = data.frame(Heads, Amount)

pie(data$Amount, labels = paste(data$Heads, sep = "\n", Amount),col = rainbow(4), main = "Expenses (in Rs.)")


#function to calculate Standard Deviation from a frequency distribution table
l = c(0, 10, 20, 30, 40)
u = c(10, 20, 30, 40, 50)
f = c(5, 7, 17, 4, 3)

sd_fq = function(l, u, f){
  
  sum_xf = 0
  sum_xsqf = 0
  n = 0
  sd = 0
  
  x=c()
  
  for(i in f){
    n = n + i
  }
  
  for(i in 1:length(l)){
    x[i] = ((l[i]+u[i])/2)
  }
  
  for(i in 1:length(f)){
    sum_xf = sum_xf + (x[i]*f[i])
    sum_xsqf = sum_xsqf + ((x[i]^2)*f[i])
  }
  
  mean_sq = (sum_xf/n)^2
  
  sd = sqrt((sum_xsqf/n)-mean_sq)
  
  return(sd)
}

sd_fq(l, u, f)




#function to calculate skewness
l = c(0, 10, 20, 30, 40)
u = c(10, 20, 30, 40, 50)
f = c(5, 7, 17, 4, 3)

skew = function(l, u, f){
  
  sum_xf = 0
  sum_xsqf = 0
  n = 0
  sd = 0
  
  x=c()
  
  for(i in f){
    n = n + i
  }
  
  for(i in 1:length(l)){
    x[i] = ((l[i]+u[i])/2)
  }
  
  for(i in 1:length(f)){
    sum_xf = sum_xf + (x[i]*f[i])
    sum_xsqf = sum_xsqf + ((x[i]^2)*f[i])
  }
  
  mean = (sum_xf/n)
  mean_sq = mean^2
  
  sd = sqrt((sum_xsqf/n)-mean_sq)
  
  
  h = u[1] - l[1]
  cf = c()
  cf_sum = 0
  
  for(i in f){
    cf_sum = cf_sum + i
    cf = c(cf, cf_sum)
  }  
  
  max_f = 0
  
  for(i in f){
    if(i>max_f){
      max_f = i
      f1_index = which(f==i)
    }
  }
  
  md = l[f1_index] + (h*((f[f1_index]-f[f1_index-1])/((2*f[f1_index])-f[f1_index-1]-f[f1_index+1])))
  
  sk = (mean - md)/sd
  
  return(sk)
}

skew(l, u, f)



#plotting a graph and clustering values
#x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#y = c(1, 2, 3, 7, 8, 9, 4, 5, 6, 8)

two_d=read.csv('2D.csv')

model = kmeans(two_d, 3)

#model$cluster

plot(two_d$x, two_d$y, col=model$cluster, xlab = "x", ylab = "y")




# coefficient of correlation 
x=c(50,50,55,60,65,65,65,60,60,50)
y=c(11,13,14,16,16,15,15,14,13,13)

corr = function(x,y) {
  sum_x = 0
  sum_y = 0
  sum_xy = 0
  sum_xx = 0
  sum_yy = 0
  n = length(x)
  result = 0
  
  for (i in 1:n) {
    sum_x = sum_x + x[i]
    sum_y = sum_y + y[i]
    sum_xy = sum_xy + (x[i]*y[i])
    sum_xx = sum_xx + (x[i]*x[i])
    sum_yy = sum_yy + (y[i]*y[i])
    
  }
  
  
  res_num = sum_xy - ((sum_x*sum_y)/n)
  res_den = sqrt((sum_xx-((sum_x)^2)/n)*(sum_yy-((sum_y)^2)/n))
  result = res_num/res_den
  
  print(result)
  
}

corr(x,y)




# LPP - cia sum - lpSolve package
obj = c(10,6,4)
con = matrix(c(1,1,1,10,4,5,2,2,6), nrow=3, byrow=TRUE)
dir = c("<=","<=","<=")
rhs = c(100,600,300)
model = lp ("max", obj, con, dir, rhs)
model$solution
model$objval

# Transportation
costs = matrix(c(16,20,12,14,8,18,26,24,16), byrow=TRUE, nrow = 3)

row.signs <- rep ("<=", 3)
row.rhs <- c(200,160,90)
col.signs <- rep (">=", 3)
col.rhs <- c(180,120,150)

lp.transport (costs, "min", row.signs, row.rhs, col.signs, col.rhs)$solution


# Assignment
lp.assign(matrix(c(65,40,90,80,90,60,35,100,85,85,60,38,105,90,95,70,45,120,90,100,65,40,105,87,90), nrow=5, byrow=TRUE))
