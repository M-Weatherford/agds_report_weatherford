temp <- -0.5
if (temp < 0.0){
  is_frozen <- TRUE
} else {
  is_frozen <- FALSE
}
is_frozen

vec_temp <- seq(10)  # equivalent to 1:10
temp_sum <- 0        # initialize sum
for (idx in seq(length(vec_temp))){
  temp_sum <- temp_sum + vec_temp[idx]
}
temp_sum


###EXCERSICE-----------------------------------------------
vec<-seq(100)
sum <- 0

for (idx in seq(length(vec))){
  sum <- sum + vec[idx]
}
sum

idx = 1 
while_sum <- 0     
while (idx <= 100){
  while_sum <- while_sum + vec[idx]
  idx = idx + 1
}
while_sum

div_sum<-0
for (idx in seq(length(vec))){
  if(idx%%3==0&idx%%7==0){
    div_sum <- div_sum + vec[idx]
  }
}
cat("The sum of multiples of 3 and 7 within 1-100 is:",div_sum)


mymat <- matrix(c(6, 7, 3, NA, 15, 6, 7, 
                  NA, 9, 12, 6, 11, NA, 3, 
                  9, 4, 7, 3, 21, NA, 6, 
                  rep(NA, 7)),
                nrow = 4, byrow = TRUE)
myvec <- c(8, 4, 12, 9, 15, 6)

idx <- 1
idy <- 1
for (idx in seq(nrow(mymat))){
  for (idy in seq(ncol(mymat))) {
    if(is.na(mymat[idx,idy])){
      mymat[idx,idy]<-max(myvec)
    }
  }
  myvec<-myvec[myvec != max(myvec)]
}

new_vec <- 0
id <- 1
for (id in seq(1:100)) {
  
  if(id>=26&id<=65){
    new_vec[id]<-NA
  }
  else{
    new_vec[id]<- -20
  }
  if(id>=1&id<=25){
    new_vec[id]<-6
  }
}
new_vec

interp<-approx(seq_along(new_vec),new_vec,method='linear')
plot(interp)
