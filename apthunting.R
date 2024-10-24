library(readxl)
library(ggplot2)
library(rmdformats)
library(assertthat)
library(stringr)
library(paletteer)
library(nord)
library(scales)
library(plotly)
library(tidyr)
library(plyr)

xl = gsub('"', '', params$excel)
xl = gsub("\\\\", "/", xl)

df = read_excel(xl)   

print_by_keyword <- function(keyword) {
  for (i in 1:length(df)) {
    if (grepl(tolower(keyword), tolower(df[i, "Amenities"]))) {
      print(paste(df[i, "Name"]))
    }
  }
}

get_number_of_amenities <- function(df) {
  amenities_amts = list()
  for (i in 1:length(df[, "Amenities"][[1]])) {
    if (is.string(df[i, "Amenities"][[1]])) {
      split = (as.list(unlist((strsplit(df[i, "Amenities"][[1]], split=", ")))))
      amenities_amts[i] = length(split)
    }
    
  }
  
  return (amenities_amts)
}

get_count_amenity <- function(df, amenity) {
  str_count(df[, "Amenities"][[1]], amenity)
}

# algorithm for figuring out each apt's "score":
# ((1500 - rent) * 0.45) + (bedrooms * 100) + (bathrooms * 100) + (kitchens * 50) + (offices * 100)
# + ((30 - commute_time) * 10) + (pet_friendly * 100) + (furnished * 100) + (count_of_other_amenities * 50)

get_scores <- function(df) {
  scores <- list()
  for (i in 1:length(df[, ][[1]])) {
    rent_score = (1500 - df[i, "Rent per month (total)"][[1]]) * 0.45
    bedroom_score = df[i, "Bedrooms"][[1]] * 100
    bathroom_score = df[i, "Bathrooms"][[1]] * 100
    kitchen_score = df[i, "Kitchens"][[1]] * 50
    office_score = df[i, "Offices"][[1]] * 100
    commute_score = (30 - df[i, "Commute time (minutes)"][[1]]) * 10
    pet_score = 0
    furnished_score = 0
    total_am_score = 0
    
    if (grepl("pets", df[i, "Amenities"], ignore.case=TRUE)) {
      pet_score = 100
    }
    
    if (grepl("furnished", df[i, "Amenities"], ignore.case=TRUE)) {
      furnished_score = 100
    }
    
    if (is.string(df[i, "Amenities"][[1]])) {
      decrease_length = 0;
      split = as.list(unlist((strsplit(df[i, "Amenities"][[1]], split=", "))))
      for (j in split) {
        if (is.na(split[j])) {
          decrease_length = decrease_length + 1
        }
      }
      total_am_score = (length(split) - decrease_length) * 50
    }
    
    scores[[i]] = list(rent=rent_score, bedrooms=bedroom_score, bathrooms=bathroom_score, kitchens=kitchen_score, offices=office_score, commute=commute_score, pet_friendly_binary=pet_score, furnished_binary=furnished_score, total_amenities=total_am_score)
    
    
  }
  
  return(scores)
}

scores = get_scores(df)

amenities_amts = get_number_of_amenities(df)

amenities = list()

for (i in 1:length(df[, "Amenities"][[1]])) {
  if (is.string(df[i, "Amenities"][[1]])) {
    split = as.list(unlist((strsplit(df[i, "Amenities"][[1]], split=", "))))
    for (j in 1:length(split)) {
      split[j] = tolower(split[j])
    }
    amenities[[i]] = split
  }
  
}

dumb_workaround <- function(df) {
  options(repr.plot.width =2, repr.plot.height =2)
  
  ggplotly(width=1000, height=600, ggplot(df, aes(x=as.numeric(df[, "Commute time (minutes)"][[1]]), y=as.numeric(amenities_amts), size=as.numeric(df[, "Rent per month (total)"][[1]]), color=as.factor(df[, "Name"][[1]]))) +
             geom_point(alpha=0.75) + geom_point(shape=21, stroke=1, alpha=0.5, color="white") + 
             scale_size(range = c(5, 10), name="Rent per month in USD") + 
             labs(color = "Apartment Name") + theme_light() + labs(x="Commute time (minutes)", y="Amount of amenities") + scale_color_nord("lumina") + xlim(5,21) + ylim(0, 10) + guides(colour = guide_legend(override.aes = list(size=10))))
}

u_amenities = unique(unlist(amenities))

frequencies = list()

for (i in 1:length(u_amenities)) {
  frequencies[i] = sum(grepl(u_amenities[i], unlist(amenities)))
}


#print(ggplot(df, aes(x=as.numeric(df[, "Commute time (minutes)"][[1]]), y=as.numeric(amenities_amts), size=as.numeric(df[, "Rent per month (total)"][[1]]), color=as.factor(df[, "Name"][[1]]))) +
#        geom_point(alpha=0.85) + 
#        scale_size(range = c(10, 50), name="Rent per month in USD") +
#        labs(color = "Apartment Name") + theme_light() + labs(x="Commute time (minutes)", y="Amount of amenities") + scale_color_nord("lumina") + xlim(5,21) + ylim(0, 10))