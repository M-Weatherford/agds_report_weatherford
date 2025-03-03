
r<-16
rad<-pi*r^2
circu<-2*pi*r
print(rad)
print(circu)

seq(0,pi,length.out=5)

vec <- seq(1,100,by=1)
sum(vec)

x<-12
x<-x+1
x<-x*2
x<-x+4
x<-x/2
x<-x-12
x

print(datasets::rivers)
rivers<-datasets::rivers
#The rivers dataset is a list with a length of 141

mean(rivers)
median(rivers)
max(rivers)
min(rivers)
quantile(rivers,c(.5,.33))

print(datasets::quakes)
quakes<-datasets::quakes
dim(quakes)
magnitude<-quakes[,4]
max(magnitude)

