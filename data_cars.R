summary(mtcars)
ratarata<-mean(mtcars$mpg)
n<-length(mtcars)
status <- numeric(n)
for (i in 1:n){
  if (mtcars$mpg[i]>ratarata){
    status[i]<-'tinggi'
  }
  else
    status<-append(status,'rendah')
}

hist(mtcars$mpg)

# Menambahkan hasil kategori ke data asli
mtcars_final <- cbind(mtcars, Kategori = status)

# Analisis Regresi
# Melihat pengaruh Berat Mobil (wt) terhadap MPG
model_regresi <- lm(mpg ~ wt, data = mtcars)
summary(model_regresi)

plot(mtcars$wt, mtcars$mpg, 
     main = "Analisis Regresi: Berat Mobil vs Efisiensi",
     xlab = "Berat Mobil (1000 lbs)", 
     ylab = "Miles Per Gallon (MPG)",
     pch = 19, 
     col = "hotpink", 
     frame.plot = FALSE)

# Menambahkan garis regresi
abline(model_regresi, col = "red", lwd = 2)
