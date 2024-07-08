options(repos = c(CRAN = "https://cran.rstudio.com/"))


# Install and load necessary packages
install.packages("readr")
install.packages("dplyr")
install.packages("car")
install.packages("ggplot2")
library(readr)
library(dplyr)
library(car)
library(ggplot2)
# Load the data
df <- read.csv('E:\\ASSIGNMENT\\Data\\pizza_data.csv',header=TRUE)

# Define the model formula
model <- 'ranking ~ brand + price + weight + crust + cheese + size + toppings + spicy'

# Fit the model
model_fit <- lm(model, data = df)
summary(model_fit)

# Conjoint attributes
conjoint_attributes <- c('brand','price','weight','crust','cheese','size','toppings','spicy')
level_name <- list()
part_worth <- list()
part_worth_range <- c()
important_levels <- list()
end <- 1  # Initialize index for coefficient in params

for (item in conjoint_attributes) {
  levels <- unique(df[[item]])
  level_name[[item]] <- levels
  
  nlevels <- length(levels)
  
  begin <- end
  end <- begin + nlevels - 1
  
  new_part_worth <- model_fit$coefficients[begin:end]
  new_part_worth <- c(new_part_worth, -sum(new_part_worth))
  
  important_levels[[item]] <- which.max(new_part_worth)
  part_worth[[item]] <- new_part_worth
  part_worth_range <- c(part_worth_range, max(new_part_worth) - min(new_part_worth))
}
# next iteration
cat("-------------------------------------------------------------\n")
cat("level name:\n")
print(level_name)
cat("npw with sum element:\n")
print(new_part_worth)
cat("imp level:\n")
print(important_levels)
cat("part worth:\n")
print(part_worth)
cat("part_worth_range:\n")
print(part_worth_range)
cat(length(part_worth))
cat("important levels:\n")
print(important_levels)

attribute_importance <- sapply(part_worth_range, function(i) round(100 * (i / sum(part_worth_range)), 2))
print(attribute_importance)

part_worth_dict <- list()
attrib_level <- list()
for (i in seq_along(conjoint_attributes)) {
  item <- conjoint_attributes[i]
  cat("Attribute :", item, "\n")
  cat("    Relative importance of attribute ", attribute_importance[i], "\n")
  cat("    Level wise part worths: \n")
  for (j in seq_along(level_name[[item]])) {
    cat(i, "\n")
    cat(j, "\n")
    cat("          ", level_name[[item]][j], ":", part_worth[[item]][j], "\n")
    part_worth_dict[[level_name[[item]][j]]] <- part_worth[[item]][j]
    attrib_level[[item]] <- level_name[[item]]
  }
}
print(part_worth_dict)

# Plot relative importance of attributes
ggplot(data.frame(attribute=conjoint_attributes, importance=attribute_importance), aes(x=attribute, y=importance)) +
  geom_bar(stat='identity') +
  labs(title='Relative importance of attributes', x='Attributes', y='Importance')

# Calculate utility scores
utility <- sapply(1:nrow(df), function(i) {
  sum(sapply(conjoint_attributes, function(attr) part_worth_dict[[df[[attr]][i]]]))
})
df$utility <- utility
print(utility)

for (i in seq_along(conjoint_attributes)) {
  cat("Preferred level in", conjoint_attributes[i], "is ::", level_name[[conjoint_attributes[i]]][important_levels[[conjoint_attributes[i]]]], "\n")
}