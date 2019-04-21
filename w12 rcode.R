install.packages("forecast")



"Q13"
cbe <- read.table("C:/Users/reneq/Documents/2019SP Time-Series Analysis/w5-cbe.dat", header = TRUE)
dim(cbe)
choc <- cbe[["choc"]]
choc.ts <- ts(choc,st = c(1958,1), fr = 12)
choc
fc <-  forecast::naive(choc)

forecast::checkresiduals(fc)
forecast::autoplot(residuals(fc))
