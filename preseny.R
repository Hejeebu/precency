bank <- read.csv('C:/Users/Srirama/Downloads/bank-additional-full.csv', sep=",", stringsAsFactors=FALSE)
head(bank)

bank[] <- data.matrix(bank)
index = sample(1:nrow(bank),round(0.005*nrow(bank)))
train_ = bank[index,]

library(neuralnet)
names(bank)
attach(bank)
str(bank)
scaleddata<-scale(bank)
normalize<-function(x){
  return((x-min(x))/max(x)-min(x))
}
maxmindata = as.data.frame(lapply(data,normalize
))

n = names(train_)
n
f= as.formula(paste("yes.or.no.statement~",paste(n[!n%in%"yes.or.no.statement"],collapse = "+")))

nn = neuralnet(f,data = train_,hidden=c(2,1),linear.output = F)
plot(nn)
nn$result.matrix
back_prop = neuralnet(f,data = train_,hidden=3, algorithm=
                        "backprop", rep=3, learningrate = 0.0001)
plot(back_prop)
back_prop$result.matrix
write.csv(bank,"C:\\Users\\Srirama\\Downloads\\bank.csv",row.names = FALSE)
bank
