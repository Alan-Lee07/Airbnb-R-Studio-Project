# install.packages("dplyr")
# install.packages("purrr")


library(dplyr)
library(purrr)
#Visualization
library(ggmap)
library(lubridate)
library(tm)
library(tidytext)
library(qdapTools)
library(wordcloud)
#Prediction
library(caret)
library(forecast)
library(GGally)
#Knn
library(lattice)
library(FNN)
# naive Bayes
library(lubridate)
library(GGally)
library(e1071)
library(caret)
library(gains)
# tree
library(rpart)
library(rpart.plot)
library(e1071)

########## Data Download ##########
Melbourne <- read.csv("melbourne.csv")
Southbank <- filter(Melbourne, neighborhood == "Southbank") %>% 
  select(id,
         name,
         summary,
         neighborhood_overview,
         host_id,
         host_name,
         host_since,
         host_location,
         host_response_time,
         host_response_rate,
         host_is_superhost,
         host_verifications,
         host_identity_verified,
         street,
         neighborhood,
         city,
         zipcode,
         latitude,
         longitude,
         is_location_exact,
         property_type,
         room_type,
         accommodates,
         bathrooms,
         bedrooms,
         beds,
         bed_type,
         amenities,
         price,
         weekly_price,
         monthly_price,
         security_deposit,
         cleaning_fee,
         guests_included,
         extra_people,
         minimum_nights,
         maximum_nights,
         calendar_updated,
         has_availability,
         availability_30,
         availability_60,
         availability_90,
         availability_365,
         number_of_reviews,
         review_scores_rating,
         review_scores_accuracy,
         review_scores_cleanliness,
         review_scores_checkin,
         review_scores_communication,
         review_scores_location,
         review_scores_value,
         requires_license,
         instant_bookable,
         cancellation_policy,
         require_guest_profile_picture,
         require_guest_phone_verification,
         calculated_host_listings_count,
         reviews_per_month) %>% 
  droplevels()

########## Step I: Data Preparation & Exploration ##########
##### I. Missing Values #####
anyNA(Southbank)
Southbank[Southbank == ""] <- "NA"
map(Southbank, ~sum(is.na(.)))

# Drop the rows
Southbank <- filter(Southbank, host_response_time != "NA", host_response_rate != "NA",
                    host_is_superhost != "NA", host_identity_verified != "NA")

# Drop the variables
Southbank <- select(Southbank, -c(weekly_price, monthly_price))

# Fill with values
Southbank$beds[Southbank$id == 20831698 | Southbank$id == 13460274] <- 1
Southbank$security_deposit[is.na(Southbank$security_deposit == "NA")] <- 0
Southbank$cleaning_fee[is.na(Southbank$cleaning_fee == "NA")] <- 0
Southbank$review_scores_rating[is.na(Southbank$review_scores_rating)] <- median(Southbank$review_scores_rating, na.rm = TRUE)
Southbank$review_scores_accuracy[is.na(Southbank$review_scores_accuracy)] <- median(Southbank$review_scores_accuracy, na.rm = TRUE)
Southbank$review_scores_cleanliness[is.na(Southbank$review_scores_cleanliness)] <- median(Southbank$review_scores_cleanliness, na.rm = TRUE)
Southbank$review_scores_checkin[is.na(Southbank$review_scores_checkin)] <- median(Southbank$review_scores_checkin, na.rm = TRUE)
Southbank$review_scores_communication[is.na(Southbank$review_scores_communication)] <- median(Southbank$review_scores_communication, na.rm = TRUE)
Southbank$review_scores_location[is.na(Southbank$review_scores_location)] <- median(Southbank$review_scores_location, na.rm = TRUE)
Southbank$review_scores_value[is.na(Southbank$review_scores_value)] <- median(Southbank$review_scores_value, na.rm = TRUE)
Southbank$reviews_per_month[is.na(Southbank$reviews_per_month)] <- median(Southbank$reviews_per_month, na.rm = TRUE)

##### II. Summary Statistics #####
# group_by & summarise
Melbourne_summary <- select(Melbourne, neighborhood, price, review_scores_rating)
Melbourne_avg <- c(round(mean(Melbourne_summary$price), 5),
                   round(mean(Melbourne_summary$review_scores_rating, na.rm = TRUE), 5))
Southbank_avg <- c(round(mean(Southbank$price), 5),
                   round(mean(Southbank$review_scores_rating, na.rm = TRUE), 5))
Melbourne_Southbank <- cbind(Melbourne_avg, Southbank_avg) %>% 
  t()
dimnames(Melbourne_Southbank) = list(c("Melbourne", "Southbank"), c("Avg_Price", "Avg_Score"))
sapply(Melbourne_Southbank, as.numeric)
Melbourne_Southbank

# group_by & summarise
Southbank_group_by <- select(Southbank, property_type, price) %>% 
  group_by(property_type) %>% 
  summarise(Num = n(), MaxPrice = max(price),
            MinPrice = min(price), Avg = mean(price),
            Median = median(price), Sd = sd(price)) %>% 
  arrange(desc(Num))
Southbank_group_by

# correlation
Southbank_cor <- select(Southbank, review_scores_rating, review_scores_accuracy,
                        review_scores_cleanliness, review_scores_checkin,
                        review_scores_communication, review_scores_location,
                        review_scores_value) %>%
  cor()
names <- c("rating", "accuracy", "cleanliness", "checkin", "communication", "location", "value")
colnames(Southbank_cor) <- names
rownames(Southbank_cor) <- names
round(Southbank_cor, 5)

# table
Southbank_table <- select(Southbank, host_response_time, host_is_superhost)
table(Southbank_table)

# fivenum
Southbank_fivenum <- select(Southbank, cleaning_fee)
fivenum(Southbank_fivenum$cleaning_fee)


##### II. Visualization #####
# plot 1 # map
p1 <- qmplot(longitude, latitude, data=Southbank, extent="panel",
             colour=room_type, shape=room_type,
             maptype = "terrain", zoom=15) + 
  ggtitle("Map of Southbank Airbnb room type")+
  theme(plot.title = element_text(hjust = 0.5))
p1

p1_1 <- qmplot(longitude, latitude, data=Southbank, extent="panel", 
               colour=property_type, 
               maptype = "terrain", zoom=15) + 
  ggtitle("Map of Southbank Airbnb Property Type ") +
  theme(plot.title = element_text(hjust = 0.5))
p1_1

# plot 2 # boxplot with price and property type
p2 <- ggplot(Southbank, aes(x= reorder(property_type, price, "median"), y=price))+
  geom_boxplot(fill=rainbow(n=8)) +
  ggtitle("Southbank: Property Types Compared by Price") +
  xlab("Property Type") + theme(plot.title = element_text(hjust = 0.5))
p2

# plot 3 # price distribution 
p3 <- ggplot(data=Southbank, aes(x=price)) + 
  geom_histogram(binwidth = 30, fill="gray", color="black")+
  ggtitle("Price Distribution")+ theme(plot.title = element_text(hjust = 0.5))
p3

# plot 4 # line for host since
southbank_line <- select(Southbank, host_since) %>% 
  droplevels()
southbank_line$host_since <- as.Date(southbank_line$host_since, format= "%m/%d/%Y")
anyNA(southbank_line)
southbank_line <- mutate(southbank_line, host_year = year(host_since)) %>% 
  group_by(host_year)
southbank_line_plot <- summarise(southbank_line, num_of_hosts=n()) %>%
  as.data.frame()
str(southbank_line_plot)
p4 <- ggplot(southbank_line_plot, aes(x=host_year, y=num_of_hosts)) +geom_line()+
  geom_text(aes(label=num_of_hosts), vjust=-0.5) + xlab("starting Years") + 
  ylab("Number of Hosts") + ggtitle("Southbank: the Starting Year of the Hosts") +
  theme(plot.title = element_text(hjust = 0.5))
p4

# plot 5 # text minning 

#### select variables (neighborhood_overview )
southbanktm <- filter(Southbank) %>% select(neighborhood_overview) %>%
  droplevels()

#### cleaning 
corpus <- Corpus(VectorSource(southbanktm$neighborhood_overview))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)

### build a term-document matrix
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

### generate the word cloud
set.seed(1)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.45,
          colors=brewer.pal(8, "Dark2"))

### The frequency table of words
head(d, 10)
### Plot word frequencies
barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


########### Step II: Prediction ##########
## select variables 
Southbank_MLR <- select(Southbank, host_is_superhost, property_type, room_type, accommodates,
                        bathrooms, bedrooms, beds, security_deposit, cleaning_fee, 
                        guests_included, extra_people, number_of_reviews, review_scores_rating,
                        review_scores_accuracy, review_scores_cleanliness, review_scores_checkin,
                        review_scores_communication, review_scores_location, review_scores_value,
                        cancellation_policy, reviews_per_month, price) %>%
  droplevels()

## Data Set Patition
set.seed(1)
Southbank_MLR_sample <- sample_n(Southbank_MLR, 1247)
Southbank_MLR_train <- slice(Southbank_MLR_sample, 1:748)
Southbank_MLR_valid <- slice(Southbank_MLR_sample, 748:1247)

## Backward Elimination
Southbank_MLR_lm <- lm(price ~., data= Southbank_MLR_train)
summary(Southbank_MLR_lm)

Southbank_MLR_bw <- step(Southbank_MLR_lm, direction = "backward")
summary(Southbank_MLR_bw)

## check the correlation for remaining variables
Southbank_MLR_check <- select(Southbank_MLR_train, host_is_superhost,extra_people,  
                              bathrooms, bedrooms, security_deposit, guests_included,
                              reviews_per_month )
ggpairs(Southbank_MLR_check)

Southbank_MLR_check2 <- lm(price ~ host_is_superhost+bathrooms+bedrooms+security_deposit+extra_people
                           +guests_included+reviews_per_month, data=Southbank_MLR_train)
summary(Southbank_MLR_check2)

## MLR with selected variables
Southbank_MLR_final <- lm(price ~ host_is_superhost+bathrooms+bedrooms+security_deposit+guests_included
                          +reviews_per_month+extra_people, data=Southbank_MLR_train)
summary(Southbank_MLR_final)

## Prediction accuracy
pred_train <- predict(Southbank_MLR_final, Southbank_MLR_train)
accuracy(pred_train, Southbank_MLR_train$price)  #accuracy against trainset
pred_valid <- predict(Southbank_MLR_final, Southbank_MLR_valid)  #accuracy against validset
accuracy(pred_valid, Southbank_MLR_valid$price)


########## Step III: Classification ##########

##### Part I. k-nearest neighbors #####

# select variables
KNN <- select(Southbank, c(23:26,29:35,38:41,52))
str(KNN)
levels(KNN$cancellation_policy)

# partition
set.seed(1)
train.index <- sample(row.names(KNN), dim(KNN)[1]*0.6)
valid.index <- setdiff(row.names(KNN), train.index)
train.knn <- KNN[train.index, ]
valid.knn <- KNN[valid.index, ]

# create new data.frame
knn.new.data <- data.frame(accommodates=4, bathrooms=2, bedrooms=2, beds=3, price=200,
                           security_deposit=200, cleaning_fee=100, guests_included=2,
                           extra_people=0, minimum_nights=1,maximum_nights=365,
                           availability_30=7, availability_60=56, availability_90=77,
                           availability_365=215)

# normalization
norm.values <- preProcess(train.knn[, 1:15], method=c('center','scale'))
train.knn[, 1:15] <- predict(norm.values, train.knn[, 1:15])
valid.knn[, 1:15] <- predict(norm.values, valid.knn[, 1:15])
new.norm <- predict(norm.values, knn.new.data)

# optimal k-value
accuracy.df <- data.frame(k=seq(1,20,1), accuracy=rep(0,20))
for (i in 1:20){
  knn.pred <- knn(train.knn[, 1:15], valid.knn[, 1:15], cl=train.knn[, 16], k=i)
  accuracy.df[i,2] <- confusionMatrix(knn.pred, valid.knn[, 16])$overall[1]
}
accuracy.df
accuracy.df %>% filter(accuracy==max(accuracy))

# graph and chart
accuracy.df$k <- as.factor(accuracy.df$k)
ggplot(accuracy.df, aes(x=k, y=accuracy)) + geom_point(size=2,shape=16) + theme_bw() +
  ggtitle('The Distribution of K-Values and Accuracy Metrics') + xlab('K-Value') +
  ylab('Accuracy') + theme(plot.title=element_text(hjust=0.5))

# return the knn
nn <- knn(train=train.knn[, 1:15], test=new.norm, cl=train.knn[,16], k=13)
row.names(train.knn)[attr(nn, 'nn.index')]
nn


##### Part II. Naive Bayes #####
# Data Preparation
naive_Bayes <- select(Southbank, host_since, host_response_time, host_is_superhost, property_type,
                      room_type, accommodates, price, security_deposit, cleaning_fee, review_scores_rating,
                      review_scores_communication, cancellation_policy, require_guest_profile_picture,
                      require_guest_phone_verification, instant_bookable) %>% 
  droplevels()

naive_Bayes$host_since <- as.Date(naive_Bayes$host_since, format = "%m/%d/%Y")
naive_Bayes <- mutate(naive_Bayes, host_since = as.factor(year(host_since)))

str(naive_Bayes)
naive_Bayes$accommodates <- cut(naive_Bayes$accommodates, breaks = c(1, 2, 6, 12),
                                labels = c("Single or Couple Size", "Friends or Family Size", "Party Size"))
naive_Bayes$price <- cut(naive_Bayes$price, breaks = fivenum(naive_Bayes$price),
                         labels = c("Economic", "Affordable", "Moderate", "Luxurious"))
naive_Bayes$security_deposit <- cut(naive_Bayes$security_deposit, breaks = c(-1, 0, 300, 5000),
                                    labels = c("No Fee", "Market Price", "High Fee"))
naive_Bayes$cleaning_fee <- cut(naive_Bayes$cleaning_fee, breaks = c(-1, 0, 60, 450),
                                labels = c("No Fee", "Market Price", "High Fee"))
naive_Bayes$review_scores_rating <- cut(naive_Bayes$review_scores_rating, breaks = c(0, 60, 80, 90, 100),
                                        labels = c("Below Avg", "General Taste", "Above Avg", "Extraordinary"))
naive_Bayes$review_scores_communication <- cut(naive_Bayes$review_scores_communication, breaks = c(0, 6, 8, 9, 10),
                                               labels = c("Below Avg", "General", "Above Avg", "Excellent"))
str(naive_Bayes)

# Data Partition
set.seed(1)
nrow(naive_Bayes); nrow(naive_Bayes) * 0.6
naive_Bayes_Partition <- sample_n(naive_Bayes, nrow(naive_Bayes))
naive_Bayes_Train <- slice(naive_Bayes_Partition, 1 : round(nrow(naive_Bayes_Partition) * 0.6))
naive_Bayes_Valid <- slice(naive_Bayes_Partition, (round(nrow(naive_Bayes_Partition) * 0.6) + 1) : nrow(naive_Bayes_Partition))

# naive Bayes model built against training data
naive_Bayes_Train_nb <- naiveBayes(instant_bookable ~ ., data = naive_Bayes_Train)
naive_Bayes_Train_nb

# Model Performance
naive_Bayes_Train_pred <- predict(naive_Bayes_Train_nb, naive_Bayes_Train)
confusionMatrix(data = naive_Bayes_Train_pred, reference = naive_Bayes_Train$instant_bookable)

naive_Bayes_Valid_nb <- naiveBayes(instant_bookable ~ ., data = naive_Bayes_Valid)
naive_Bayes_Valid_pred <- predict(naive_Bayes_Valid_nb, naive_Bayes_Valid)
confusionMatrix(data = naive_Bayes_Valid_pred, reference = naive_Bayes_Valid$instant_bookable)

naive_Bayes_nb <- naiveBayes(instant_bookable ~ ., data = naive_Bayes_Partition)
naive_Bayes_pred_prob <- predict(naive_Bayes_nb, naive_Bayes, type = "raw")

gain <- gains(ifelse(naive_Bayes$instant_bookable == "f", 1, 0), naive_Bayes_pred_prob[ , 1], groups = 100)
plot(c(0, gain$cume.pct.of.total * sum(naive_Bayes$instant_bookable == "f")) ~ c(0, gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative", main = "", type = "l")
lines(c(0, sum(naive_Bayes$instant_bookable == "f")) ~ c(0, dim(naive_Bayes)[1]), lty = 2)

# Fictional Apartment 
Cozy_apt <- data.frame(host_since = "2018", host_response_time = "within an hour",
                       host_is_superhost = "f", property_type = "Apartment",
                       room_type = "Entire home/apt", accommodates = "Friends or Family Size",
                       price = "Affordable", security_deposit = "Market Price",
                       cleaning_fee = "High Fee", review_scores_rating = "Extraordinary",
                       review_scores_communication = "Excellent", cancellation_policy = "moderate",
                       require_guest_profile_picture = "f", require_guest_phone_verification = "f")

# predict probability
Cozy_apt_pred_prob <- predict(naive_Bayes_nb, Cozy_apt, type = "raw")
Cozy_apt_pred_prob
# predict class membership
Cozy_apt_pred_class <- predict(naive_Bayes_nb, Cozy_apt)
Cozy_apt_pred_class
cat("The Cozy Apt is predicted as", ifelse(Cozy_apt_pred_class == "t", "instantly bookable", "not instantly bookable"))


##### Part III. Classification Tree #####

str(Southbank)
View(Southbank)
#Binning cleaning fee
summary(Southbank$cleaning_fee)
Southbank$cleaning_fee2 <- cut(Southbank$cleaning_fee, breaks =
                                 c(-0.1, 0.1, 35, 60,450), labels = c("No Fee", "Low","Moderate", "High"))
View(Southbank)
anyNA(Southbank$cleaning_fee2)
#Data Partitioning
set.seed(1)
newSouthbank <- sample_n(Southbank, 1247)
train <- slice(newSouthbank, 1:748)
valid <- slice(newSouthbank, 749:1247)
#Classification tree
colnames(Southbank)
library(rpart)
library(rpart.plot)
treemodel <-rpart(cleaning_fee2~property_type+room_type+accommodates+bathrooms+bedrooms+beds+price+guests_included
                  +extra_people+review_scores_cleanliness, data=train,method="class")
rpart.plot(treemodel,extra = 101, nn = TRUE, tweak = 1)
#Determine the ideal size of your tree using cross-validation
treemodel2<-rpart(cleaning_fee2~property_type+room_type+accommodates+bathrooms+bedrooms+beds+price+guests_included
                  +extra_people+review_scores_cleanliness,data=train,method="class",cp=0.00001,xval=5)
printcp(treemodel2)
#when the xerror=0.68090, the CP is 0.01005025 
treemodel3<-rpart(cleaning_fee2~property_type+room_type+accommodates+bathrooms+bedrooms+beds+price+guests_included
                  +extra_people+review_scores_cleanliness,data=train,method="class",cp=0.01005025)
rpart.plot(treemodel3,extra = 101, nn = TRUE, tweak = 1)
# Assess the tree
pred.train<-predict(treemodel3,newdata = train,type = "class")
confusionMatrix(pred.train,train$cleaning_fee2)
pred.valid<-predict(treemodel3,newdata = valid,type = "class")
confusionMatrix(pred.valid,valid$cleaning_fee2)


########## Step IV: Clustering ##########

# Choosing Numerical Values for Clustering
Southbank2 <- filter(Southbank) %>% 
  select(accommodates, bathrooms, bedrooms, beds, price, security_deposit, cleaning_fee, guests_included,
         extra_people, minimum_nights, maximum_nights, availability_365, number_of_reviews, 
         review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, 
         review_scores_communication, review_scores_location, review_scores_value)
str(Southbank2)

# Converting to numerical values for Normalization
Southbank2$accommodates <- as.numeric(Southbank2$accommodates)
Southbank2$bedrooms <- as.numeric(Southbank2$bedrooms)
Southbank2$price <- as.numeric(Southbank2$price)
Southbank2$guests_included <- as.numeric(Southbank2$guests_included)
Southbank2$extra_people <- as.numeric(Southbank2$extra_people)
Southbank2$minimum_nights <- as.numeric(Southbank2$minimum_nights)
Southbank2$maximum_nights <- as.numeric(Southbank2$maximum_nights)
Southbank2$availability_365 <- as.numeric(Southbank2$availability_365)
Southbank2$number_of_reviews <- as.numeric(Southbank2$number_of_reviews)
Southbank2$review_scores_checkin <- as.numeric(Southbank2$review_scores_checkin)
Southbank2$review_scores_communication <- as.numeric(Southbank2$review_scores_communication)
str(Southbank2)

# normalizing data
Southbank.norm <- sapply(Southbank2, scale)
View(Southbank.norm)

# Used the kmeans algorithm to separate it into clusters. To determine the optimal 
# of clusters to use, we used an elbow chart.
set.seed(90)
k.max <- 15
wss <- sapply(1:k.max,
              function(k){kmeans(Southbank.norm, k, nstart = 50, iter.max = 15)$tot.withinss})
wss
plot(1:k.max, wss,
     type = "b", pch=20, frame=FALSE,
     xlab="Number of Clusters K",
     ylab="Total Within-clusters Sum of Squares")

km <- kmeans(Southbank.norm, 10, nstart = 25)
km$centers








