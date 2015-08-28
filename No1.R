###Recommender System algorithm implementation on Movie Lens 100k data###

#load libraries
library(recommenderlab)
library(reshape2)

##set path
setwd("D:/Recommendation_R_Exercise/")

##read the rating data for all users
readData<-function(){
  ratingDF <- read.delim("u.data", header=F)
  colnames(ratingDF) <- c("userID","movieID","rating","timestamp")
  #read movie data
  moviesDF<-read.delim("u.item", sep="|", header=F, stringsAsFactors=FALSE)
  colnames(moviesDF)[colnames(moviesDF)=="V1"] <- "movieID"
  colnames(moviesDF)[colnames(moviesDF)=="V2"] <- "name"
  return(list(ratingDF=ratingDF, movieDF=moviesDF))
}

##data cleansing and processing
preProcess= function(ratingDF,moviesDF){
  ratingDF[,2] <- dataList$movieDF$name[as.numeric(ratingDF[,2])]
  #remove duplicate 
  ratingDF <- ratingDF[!duplicated(ratingDF[,1:2]),]
}


##create movie ratingMatrix from rating Data and movie data
createRatingMatrix <- function(ratingDF){
  #converting the ratingData data frame into rating matrix
  ratingDF_tmp <- dcast(ratingDF, userID ~ movieID, value.var = "rating", index ="userID")
  ratingDF <- ratingDF_tmp[,2:ncol(ratingDF_tmp)]
  ratingMat<-as(ratingDF,"matrix")
  movieRatingMat<-as(ratingMat,"realRatingMatrix")
  #setting up the dimnames
  dimnames(movieRatingMat)[[1]]<-row.names(ratingDF)
  return(movieRatingMat)
}


##create recommender model
  evaluateModels<-function(movieRatingMat){
  #find out and anlyze available recommendation algorithm option for realRatingMatrix data
  recommenderRegistry$get_entries(dataType="realRatingMatrix")
  scheme <- evaluationScheme(movieRatingMat, method="split", train=.9, k=1, given=10, goodRating=4)
  algorithms<-list(
    RANDOM = list(name="RANDOM", param=NULL),
    POPULAR = list(name="POPULAR", param=NULL),
    UBCF = list(name="UBCF", param=NULL), 
    IBCF = list(name="IBCF", param=NULL)
  )
  #run algorithms, predict next n movie
  results<-evaluate(scheme, algorithms, n=c(1,3,5,10,15,20))  
  #select the first results
  return(results)
}

##visualize results
visualize <- function(results){
  #draw ROC curve
  plot(results, annotate=1:4, legend="topleft")
  #see precision /recall
  plot(results,"prec/rec", annotate=1:4, legend = "topright", xlim=c(0,.22))
}

##create prediction model
createModel<-function(movieRatingMat, method){
  model<-Recommender(movieRatingMat, method=method)
  names(getModel(model))
  getModel(model)$method
  getModel(model)$nn
  return(model)
}

##predict user rating using UBCF recommendation algorithm
recommendations <- function(movieRatingMat, model, userID, n){
  # predict top n recommendations for given user
  topN_recommendList<-predict(model,movieRatingMat[userID],n=n)
  as(topN_recommendList,"list")
}

##load movie lens data
dataList<-readData()

##data cleansing and preprocessing
ratingDF<-preProcess(dataList$ratingDF, dataList$movieDF)

##create movie rating matrix
movieRatingMat<-createRatingMatrix(ratingDF)

##evaluate models
evalList<-evaluateModels(movieRatingMat)

##plot evaluation result
visualize(evalList)

##get confusion matrix for "UBCF"
getConfusionMatrix(evalList[["UBCF"]])[[1]][,1:4]


##run "UBCF" recommender
rec_model<-createModel(movieRatingMat,"UBCF")
userID<-5
topN<-5
recommendations(movieRatingMat,rec_model,userID,topN)
