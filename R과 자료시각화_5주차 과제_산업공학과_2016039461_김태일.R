##R과 자료시각화_5주차 과제_산업공학과_2016039461_김태일
#1

func1 = function(m = 5, n = 5){
  if (m - round(m) != 0 | n - round(n) != 0){
    stop("not integer.")
  }
  mat = matrix(ncol = n, nrow = m)
  
  for (i in 1:m){
    for (j in 1:n){
      mat[i, j] = i%%j
    }
  }
  
  vec = c()
  for (k in 1:n){
    if (k < 3){
      vec[k] = 1
    }
    else{
      vec[k] = vec[k-1] + vec[k-2]
    }
  }
  return(list(mat, vec))
}
func1(m = 12, n = 9)


#2

result = func1(m = 12, n = 9)

func2 = function(result, folder, namee){
  df = as.data.frame(result[[1]])
  output = paste(folder,"/",namee, ".csv", sep="")
  write.csv(df, output)
  return(output)
}

func2(result, "C:/Users/theil/Desktop/mooc", "result")

#3

result2 = func2(result, "C:/Users/theil/Desktop/mooc", "result")

install.packages("corrplot")
library(corrplot)

func3 = function(result2){
  df2 = read.csv(result2)
  cor.mat = cor(df2)
  corrplot(cor.mat)
}
func3(result2)





