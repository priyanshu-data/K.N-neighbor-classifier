attach(wbcd_new)
wbcd<-wbcd_new

#table of diagnosis
table(wbcd$diagnosis)
#record diagnosis as a factor
wbcd$diagnosis<-factor(wbcd$diagnosis,levels = c("B","M"),
                       labels = c("Benign","Malignant"))

#table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis))*100,digits = 1)

#summary three numeric features
#summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

#create normalization function
normalize <-function(x){
  return((x - min(x))/(max(x)-min(x)))
}
#test normalization function - result should be identical 
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))
#normalize the wbcd data
wbcd_n<-as.data.frame(lapply(wbcd[2:31], normalize))
#write.csv(wbcd_n,file="wbcd_n.csv)
wbcd_n1<-cbind(wbcd_n,wbcd$diagnosis)
#confirm that normalization worked
summary(wbcd_n$area_mean)

#create training and test data
wbcd_train<-wbcd_n[1:469,]
wbcd_test<-wbcd_n[470:569,]

#create labels for training and test data
wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]

#--------training model on the data------

#load the 'class' library
library(class)
wbcd_test_pred<-knn(train = wbcd_train,test = wbcd_test,cl = wbcd_train_labels,k=20)

##-------Evaluating model performane-----

#load the "gmodels" library
library(gmodels)

#create the cross tabulation of predicted vs actual
CrossTable(x= wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)

##Improving modelperformance
