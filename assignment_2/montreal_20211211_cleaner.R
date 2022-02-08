# Clear environment
rm(list=ls())

# Load packages
library(tidyverse)

# Load raw data
df <- read_csv("listings_montreal_20211211.csv")

# Data Cleaning, Feature Engineering
# Drop variables which I think are not gonna be useful for prediction
drops <- c("host_thumbnail_url","host_picture_url","listing_url", "picture_url",
           "host_url","last_scraped","description", "neighborhood_overview", 
           "host_about", "host_response_time", "name", "host_name",
           "host_location", "bathrooms","neighbourhood_group_cleansed", "calendar_updated", 
           "translation missing: en.hosting_amenity_49", "translation missing: en.hosting_amenity_50", 
           "scrape_id", "host_id", "calendar_last_scraped")

df<-df[ , !(names(df) %in% drops)]

# Drop broken lines - in case if id is not valid
df$junk<-grepl("[[:alpha:]]", df$id)
df<-subset(df,df$junk==FALSE)
df<-df[1:ncol(df)-1]

df <- df %>% select(-id)

# Check the class and type of each columns
sapply(df, class)
sapply(df, typeof)

# Filter only airbnb which host 2-6 guests and room type is entire home/ap
df <- df %>% filter(accommodates <=6 & accommodates >= 2,
                    room_type=='Entire home/apt')
df <- df %>% select(-room_type)

# Check property type
table(df$property_type)

# Filter out certain type of properties, keep only entire house/apt
property_types_to_exclude <- c("Boat", "Camper/RV", "Room in aparthotel","Room in boutique hotel")
df <- df %>% filter(!property_type %in% property_types_to_exclude)


# Format columns
# Remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
        df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

# Convert to numeric
df$p_host_response_rate <- as.numeric(df$host_response_rate)
df$p_host_acceptance_rate <- as.numeric(df$host_acceptance_rate)
df <- df %>% select(-c(host_response_rate, host_acceptance_rate))

# Remove dollar signs from price variable
for (pricevars in c("price")){
        df[[pricevars]]<-gsub("\\$","",as.character(df[[pricevars]]))
        df[[pricevars]]<-as.numeric(as.character(df[[pricevars]]))
}

# Format binary variables
for (binary in c("host_is_superhost",
                 "host_has_profile_pic",
                 "host_identity_verified",
                 "instant_bookable",
                 "has_availability")){
        df[[binary]][df[[binary]]=="f"] <- 0
        df[[binary]][df[[binary]]=="t"] <- 1
}


# Clean the bedroom texts
df <- df %>% mutate(bathrooms =ifelse(grepl("half", bathrooms_text, ignore.case=T), 0.5,
                                      as.numeric(gsub( " .*$", "", df$bathrooms_text )))) %>% 
        select(-bathrooms_text)

# Transform host_since, first_review and last_review to day difference to today
df$host_since_to_today = as.numeric(Sys.Date() - df$host_since)
df$first_review_to_today = as.numeric(Sys.Date() - df$first_review)
df$last_review_to_today = as.numeric(Sys.Date() - df$last_review)
df <- df %>% select(-c(host_since,first_review,last_review))

# Check amenities
df$amenities<-gsub("\\{","",df$amenities)
df$amenities<-gsub("\\}","",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-as.list(strsplit(df$amenities, ","))
levels(factor(unlist(df$amenities)))

# Pick and define amenities
amenities_dummies <- list(
        
        # general info
        d_tv = "tv",
        d_ac = "air conditioning",
        d_backyard = "backyard",
        d_baking_sheet = "baking sheet",
        d_bathtub = "bathtub",
        d_bbq = "bbq|barbecue",
        d_board_games = "board games",
        d_brekkie = "breakfast",
        d_coffee = "coffee",
        d_ceiling_fan = "ceiling fan",
        d_clothing_storage = "clothing storage",
        d_dedicated_workspace = "dedicated workspace",
        d_dishwasher = "dishwasher", 
        d_dryer = "dryer",
        d_drying_rack = "drying rack",
        d_eco = "eco logical|ecological", 
        d_wifi = "wifi",
        d_fire_guard = "fire extinguisher|fire pit|fireplace guards",
        d_first_aid = "first aid",
        d_fitness = "fitness|gym",
        d_heating = "heating",
        d_high_chair = "high chair", 
        d_organic = "organic",
        d_hot_tub = "hot tub", 
        d_spa = "spa", 
        d_fireplace = "fireplace", 
        d_iron = "iron",
        d_bluetooth = "bluetooth", 
        d_nespresso = "nespresso", 
        d_kitchen_aid = "kitchen aid|kitchenaid",
        d_rice_maker = "rice maker",
        d_mosquito_net = "mosquito net",
        d_laundromat = "landromat",
        d_security_camera = "security cameras",
        d_free_parking = "free parking",
        d_long_term = "long term",
        d_pool = "pool",
        d_pets_allowed = "pets allowed", 
        d_beachfront = "beachfront",
        d_lake_access = "lake access",
        d_kayak = "kayak",
        d_piano = "piano", 
        d_pingpong = "ping pong",
        
        # outdoor facilities
        d_outdoor_dining_area = "outdoor dining area",
        d_outdoor_furniture = "outdoor furniture", 
        d_outdoor_pool = "outdoor pool", 
        d_outdoor_shower = "outdoor shower", 
        
        # digital entertainment
        d_game_console = "game console",
        d_nintendo = "nintendo", 
        d_ps = "PS2|PS3|PS4|PS5",
        d_alexa = "alexa",
        d_amazon_prime = "amazon prime",
        d_apple = "apple",
        d_bang_and_olufsen = "bang and olufsen",
        d_beats = "beats",
        d_bosch = "bosch",
        d_boombox = "boombox",
        d_bose = "bose",
        d_chromecast = "chromecast", 
        d_google = "google",
        d_jbl = "jbl", 
        d_jvc = "jvc",
        d_netlix = "netflix",
        d_pioneer = "pioneer", 
        
        # beauty care brands
        d_aroma_zone = "aroma zone",
        d_attitude = "attitude",
        d_aveda = "aveda",
        d_bella_pella = "bella pella",
        d_costco = "costco", 
        d_dove = "dove",
        d_dumarchand = "dumarchand",
        d_garnier = "garnier",
        d_head_and_shoulders = "head & shoulder|head and shoulder|head n shoulder|head&shoulder",
        d_ivory = "ivory", 
        d_kiehls = "kiehl's|kiehls",
        d_kirkland = "kirkland",
        d_loreal = "l'or|l'oreal", 
        d_lemieux = "lemieux", 
        d_melaleuca = "melaleuca",
        d_nivea = "nivea",
        d_olay = "olay", 
        d_ordinaire = "ordinaire",
        d_organix = "organix",
        d_pantene = "pantene",
        d_plusieurs = "plusieurs",
        
        # baby/children friendly
        d_baby_bath = "baby bath", 
        d_baby_monitor = "baby monitor",
        d_baby_safety_gates = "baby safety gates", 
        d_babysitter_rec = "babysitter recommendations",
        d_crib = "crib", 
        d_children = "children",
        
        # home appliance brands
        d_danby = "danby",
        d_divers = "divers",
        d_electrolux = "electrolux",
        d_fisher_and_paykel = "fisher and paykel|fisher paykel",
        d_frigidaire = "frigidaire",
        d_galanz = "galanz",
        d_induction = "induction", 
        d_inglis = "inglis",
        d_kenmore = "kenmore",
        d_lg = "lg",
        d_miele = "miele",
        d_samsung = "samsung"
        
)

# Transform to dummies
for (dummy in names(amenities_dummies)) {
        df[, dummy] <- ifelse(grepl(amenities_dummies[[dummy]], df$amenities, ignore.case = T), 1, 0) 
}

df <- df %>% select(-amenities)

# Check host_verifications
df$host_verifications<-gsub("\\{","",df$host_verifications)
df$host_verifications<-gsub("\\}","",df$host_verifications)
df$host_verifications<-gsub('\\"',"",df$host_verifications)
df$host_verifications<-as.list(strsplit(df$host_verifications, ","))
levels(factor(unlist(df$host_verifications)))

# Pick and define host verifications
hv <- list(
        d_hv_facebook = "facebook",
        d_hv_google ="google",
        d_hv_government_id ="government_id",
        d_hv_jumio ="jumio",
        d_hv_manual ="manual",
        d_hv_kba="kba",
        d_hv_phone="phone",
        d_hv_reviews="reviews",
        d_hv_selfie="selfie",
        d_hv_sent_id="sent_id",
        d_hv_sesame="sesame",
        d_hv_email="email")

# Transform to dummies
for (dummy in names(hv)) {
        df[, dummy] <- ifelse(grepl(hv[[dummy]], df$host_verifications, ignore.case = T), 1, 0) 
}

df <- df %>% select(-host_verifications)

# check column types
sapply(df, typeof)

# add d_ to dummy variables
setnames(df, old = c('host_is_superhost','host_has_profile_pic', 'host_identity_verified', 'has_availability', 'instant_bookable'), 
         new = c('d_host_is_superhost','d_host_has_profile_pic','d_host_identity_verified','d_has_availability', 'd_instant_bookable'))

# add n_ to numeric variables
for(i in 1: length(colnames(df))){
        if(is.numeric(df[[i]])){
                if(!grepl("^d_.*", colnames(df)[[i]]) & colnames(df)[[i]] != "price"){
                        names(df)[i] <- paste0('n_',names(df[i]))
                }
        }
}

# Transform the character variables to factor and add f_
df<- df%>% mutate_if(is.character, factor)
for(i in 1:length(colnames(df))){
        if(is.factor(df[[i]])){
                names(df)[i] <- paste0('f_',names(df[i]))
        }
        
}

# add n_dt_ to date difference variables
setnames(df, old = c('n_host_since_to_today','n_first_review_to_today', 'n_last_review_to_today'), 
         new = c('n_dt_host_since_to_today','n_dt_first_review_to_today','n_dt_last_review_to_today'))

# Check missing values
to_filter <- sapply(df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# Handle missing values
# Drop if no target value - price
df <- df %>% drop_na(price)

# Drop the observations if only a few missing values (2 or 3 observations)
df <- df %>% filter(!is.na(d_host_is_superhost),
                    !is.na(n_host_listings_count),
                    !is.na(n_host_total_listings_count),
                    !is.na(d_host_has_profile_pic),
                    !is.na(d_host_identity_verified),
                    !is.na(n_bathrooms),
                    !is.na(n_dt_host_since_to_today))

# Drop variables if many missing and not that important to have them in the analysis
to_drop <- c("f_neighbourhood", "f_host_neighbourhood", "f_license") # neighbourhood_cleansed can cover the info, license too many missing(8392/8616)
df <- df %>% select(-one_of(to_drop))

# Impute 
# Assume beds=accommodates, if its integer values then take the median
df <- df %>% mutate(n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds),
                    n_bedrooms = ifelse(is.na(n_bedrooms), median(n_bedrooms, na.rm=T), n_bedrooms)) 

# Create flag variable to indicate if missing for the rest variables
df <- df %>% mutate(
        d_missing_n_review_scores_rating = ifelse(is.na(df$n_review_scores_rating),1,0),
        d_missing_n_review_scores_accuracy = ifelse(is.na(df$n_review_scores_accuracy),1,0),
        d_missing_n_review_scores_cleanliness = ifelse(is.na(df$n_review_scores_cleanliness),1,0),
        d_missing_n_review_scores_checkin = ifelse(is.na(df$n_review_scores_checkin),1,0),
        d_missing_n_review_scores_communication = ifelse(is.na(df$n_review_scores_communication),1,0),
        d_missing_n_review_scores_location = ifelse(is.na(df$n_review_scores_location),1,0),
        d_missing_n_review_scores_value = ifelse(is.na(df$n_review_scores_value),1,0),
        d_missing_n_reviews_per_month = ifelse(is.na(df$n_reviews_per_month),1,0),
        d_missing_n_p_host_response_rate = ifelse(is.na(df$n_p_host_response_rate),1,0),
        d_missing_n_p_host_acceptance_rate = ifelse(is.na(df$n_p_host_acceptance_rate),1,0),
        d_missing_n_dt_first_review_to_today = ifelse(is.na(df$n_dt_first_review_to_today),1,0),
        d_missing_n_dt_last_review_to_today = ifelse(is.na(df$n_dt_last_review_to_today),1,0)
)

# Replace the na value in the original variable with zero
df[is.na(df)] <- 0

# Check missing values again, making sure there's none
to_filter <- sapply(df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# Add features 
# Using different functional forms: logs, squared, cubic to catch some non-linear patterns
df <- df %>% mutate(n_squared_number_of_reviews = n_number_of_reviews^2,
                    n_cubic_number_of_reviews = n_number_of_reviews^3,
                    n_ln_host_since_to_today = log(n_dt_host_since_to_today+1) )

# Save as cleaned
write_csv(df, "listings_montreal_20211211_cleaned.csv")
