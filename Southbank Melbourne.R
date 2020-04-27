# install.packages("dplyr")

library(dplyr)

##### Data Download #####
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
         cancellation_policy,
         require_guest_profile_picture,
         require_guest_phone_verification,
         calculated_host_listings_count,
         reviews_per_month) %>% 
  droplevels()

##### Step I: Data Preparation & Exploration #####
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
Southbank$security_deposit[Southbank$security_deposit == "NA"] <- 0
Southbank$cleaning_fee[Southbank$cleaning_fee == "NA"] <- 0
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
Melbourne_avg <- c("Melbourne", round(mean(Melbourne_summary$price), 5), round(mean(Melbourne_summary$review_scores_rating, na.rm = TRUE), 5))
Southbank_avg <- c("Southbank", round(mean(Southbank$price), 5), round(mean(Southbank$review_scores_rating, na.rm = TRUE), 5))
Melbourne_Southbank <- cbind(Melbourne_avg, Southbank_avg) %>% 
  t()
row.names(Melbourne_Southbank) <- Melbourne_Southbank[ , 1]
Melbourne_Southbank <- Melbourne_Southbank[ , -1]
colnames(Melbourne_Southbank) <- c("Avg_Price", "Avg_Score")

# group_by & summarise
Southbank_group_by <- select(Southbank, property_type, price) %>% 
  group_by(property_type) %>% 
  summarise(Num = n(), MaxPrice = max(price),
            MinPrice = min(price), Avg = mean(price),
            Median = median(price), Sd = sd(price)) %>% 
  arrange(desc(Num))

# correlation
Southbank_cor <- select(Southbank, review_scores_rating, review_scores_accuracy,
                        review_scores_cleanliness, review_scores_checkin,
                        review_scores_communication, review_scores_location,
                        review_scores_value) %>%
  cor()
Southbank_cor

# table
Southbank_table <- select(Southbank, host_response_time, host_is_superhost)
table(Southbank_table)

# fivenum
Southbank_fivenum <- select(Southbank, cleaning_fee)
fivenum(Southbank_fivenum$cleaning_fee)










