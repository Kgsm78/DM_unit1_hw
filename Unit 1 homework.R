########Ex 2.4************************************

mytable<-read.csv("C:/Users/Holly/Desktop/R/ex2_4_data.csv") #your filepath here

mytable$subject<-as.character(mytable$subject)
str(mytable)
summary(mytable) #this prints summary table
attach(mytable)  # use attach so not have to repeat dataframe name
rows<-nrow(mytable)  #this assumes that there are no NAs or nulls
#R defaults to sample std deviation instead of population, so need to adjust
sd(age)*(sqrt((rows-1)/rows))  #std deviation for age
sd(X.fat)*(sqrt((rows-1)/rows))#std deviation for % fat
boxplot(age)
boxplot(X.fat)

par(bg='light blue')
#scatterplot
plot(age,X.fat, main="Age and Body Fat",ylab="Percent body fat")
#call stats package for qqnorm plot
load(stats)

attach(mytable)
qqplot(age,X.fat, main = "QQPlot age and bodyfat",xlab ="Age",ylab ="Body Fat")
#####qqplot for zscore normalized data
table2<-read.csv("C:/Users/Holly/Desktop/R/ex2_4_zscore.csv") #your filepath here
attach(table2)
str(table2)
names(table2)
qqplot(agez,Xfatz, main = "QQPlot age and bodyfat",xlab ="Age",ylab ="Body Fat")

              

#***********************************************
# Ex 2.6
row1<-c(22,1,42,10)
row2<-c(20,0,36,8)
table1<-rbind(row1,row2)

dist(table1, method = "euclidean")
dist(table1, method = "manhattan")
dist(table1, method = "minkowski")
dist(table1, method = "maximum")
###############################################################################
####################################################
#Ex 2.8
install.packages("vegan")
library(vegan)


mytable<-read.csv("C:/Users/Holly/Desktop/R/ex2.8.csv")  #your filepath here
mytable
mytable<-mytable[,2:3] #selecting only data---I had put subject numbers in first col
mytable
dist(mytable, method = "euclidean")
#select and sort row 6 which is x'
array1<-as.matrix(dist(mytable, method = "euclidean"))
rowct<-nrow(array1) #this rowcount is reused, as all results tables will have the same
# number of rows
vector1<-array1[rowct,1:(rowct-1)]
print("Euclidean proximity in order")
sort(vector1,decreasing =FALSE)

dist(mytable, method = "manhattan")
array2<-as.matrix(dist(mytable, method = "manhattan"))

vector2<-array2[rowct,1:(rowct-1)]
print("Manhattan proximity in order")
sort(vector2,decreasing =FALSE)

#dist(mytable, method = "minkowski")
dist(mytable, method = "maximum")
array3<-as.matrix(dist(mytable, method = "maximum"))

vector3<-array3[rowct,1:(rowct-1)]
print("Maximum proximity in order")
sort(vector3,decreasing =FALSE)



install.packages('proxy') 
library('proxy') # Library of similarity/dissimilarity measures for 'dist()'
answer1<-dist(mytable, method="cosine")
answer2<-1-answer1
answer2
array4<-as.matrix(answer2)
vector4<-array4[rowct,1:(rowct-1)]

print("Cosine similarity in order")
sort(vector4,decreasing =TRUE)


###################print all sorted results###################
print("Euclidean proximity in order")
sort(vector1,decreasing =FALSE)
print("Manhattan proximity in order")
sort(vector2,decreasing =FALSE)
print("Maximum proximity in order")
sort(vector3,decreasing =FALSE)
print("Cosine similarity in order")
sort(vector4,decreasing =TRUE)

###Normalizing Attributes 1 and 2 via min-max normalization###################
table2<-data.frame()

for (j in 1:2){ 
        
        minaj<-min(mytable[,j])
        maxaj<-max(mytable[,j])
        
        for (i in 1:6) {
                x<-mytable[i,j]
                table2[i,j]<- (x-minaj)/(maxaj-minaj)
        }
}
#######################

table2

summary(table2)

dist(table2, method = "euclidean")
array5<-as.matrix(dist(table2, method = "euclidean"))
rowct<-nrow(array1) 
vector5<-array5[rowct,1:(rowct-1)]

array6<-as.matrix(dist(table2, method = "manhattan"))
vector6<-array6[rowct,1:(rowct-1)]
array7<-as.matrix(dist(table2, method = "maximum"))
vector7<-array7[rowct,1:(rowct-1)]
answer3<-dist(table2, method="cosine")
answer4<-1-answer3

array8<-as.matrix(answer4)
vector8<-array8[rowct,1:(rowct-1)]
######print sorted based on normalized
print("Euclidean proximity in order")
sort(vector5,decreasing =FALSE)
print("Manhattan proximity in order")
sort(vector6,decreasing =FALSE)
print("Maximum proximity in order")
sort(vector7,decreasing =FALSE)
print("Cosine similarity in order")
sort(vector8,decreasing =TRUE)
##########################################################################
#############################################################################
#ex 3.6 normalization

vect<-c(300,200,400,600,1000)

mytable<-data.frame(vect)
rows<-nrow(table)
cols<-ncol(table)

###Normalizing Attributes min-max normalization###################
table2<-data.frame()

for (j in 1:cols){ 
        
        minaj<-min(mytable[,j])
        maxaj<-max(mytable[,j])
        
        for (i in 1:rows) {
                x<-mytable[i,j]
                table2[i,j]<- (x-minaj)/(maxaj-minaj)
        }
}

table2
mytable
#############z score normalization
table3<-data.frame()
for (j in 1:cols){ 
        
        meanj<-mean(mytable[,j])
        sdj<-sd(mytable[,j])*sqrt((rows-1)/rows) #note adjustment to
        #use pop std deviation calc instead of sample std dev calc
        for (i in 1:rows) {
                x<-mytable[i,j]
                table3[i,j]<- (x-meanj)/sdj
        }
}
table3


############z score normalization with absolute deviation 
table4<-data.frame()

for (j in 1:cols){ 
        
        meanj<-mean(mytable[,j])
        sum<-0
        for (i in 1:rows) {
                val<-abs(mytable[i,j]-meanj)
                sum<-sum+val
        }
        absdevj<-(1/nrow(mytable))*sum
        for (i in 1:rows) {
                x<-mytable[i,j]
                table4[i,j]<- (x-meanj)/absdevj
        }
}
table4
########################################
