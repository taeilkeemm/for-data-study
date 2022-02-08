##1
#n = 8
n = 8
a = c()
k = 1
{for (i in 1:n){
  if (i < 3){
    a[i] = k
  }
  else{
    a[i] = a[i-1] + a[i-2]
  }
}
x = a[n]}
paste("n = ", n,",",  "x = ", x)
#n = 10
n = 10
a = c()
k = 1
{for (i in 1:n){
  if (i < 3){
    a[i] = k
  }
  else{
    a[i] = a[i-1] + a[i-2]
  }
}
  x = a[n]}
paste("n = ", n,",",  "x = ", x)
#n = 12
n = 12
a = c()
k = 1
{for (i in 1:n){
  if (i < 3){
    a[i] = k
  }
  else{
    a[i] = a[i-1] + a[i-2]
  }
}
  x = a[n]}
paste("n = ", n,",",  "x = ", x)

##2
#n = 10
n = 10
a = c()
k = 1
i = 1
for (b in 1:1000){
  if (b < 3){
    a[b] = k
  }
  else{
    a[b] = a[b-1] + a[b-2]
  }
}
while (a[i] < n ){
  i = i + 1
}
x = a[i]
paste("n = ", n)
paste("x = ", x)
paste("i = ", i)
#n = 50
n = 50
a = c()
k = 1
i = 1
for (b in 1:1000){
  if (b < 3){
    a[b] = k
  }
  else{
    a[b] = a[b-1] + a[b-2]
  }
}
while (a[i] < n ){
  i = i + 1
}
x = a[i]
paste("n = ", n)
paste("x = ", x)
qq = paste("i = ", i)
print(qq)
#n = 100
n = 100
a = c()
k = 1
i = 1
for (b in 1:1000){
  if (b < 3){
    a[b] = k
  }
  else{
    a[b] = a[b-1] + a[b-2]
  }
}
while (a[i] < n ){
  i = i + 1
}
x = a[i]
paste("n = ", n)
paste("x = ", x)
paste("i = ", i)

