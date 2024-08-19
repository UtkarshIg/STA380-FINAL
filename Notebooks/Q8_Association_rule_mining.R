library(tidyverse)
library(igraph)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)

file_lines = readLines("C:/Users/MUJ/Desktop/iml_p2_assigment/STA380-master/STA380-master/data/groceries.txt")

i = 1
groceries = list()

# read the lines
for (line in file_lines) {
  items = strsplit(line, ",")[[1]]
  for (item in items) {
    groceries = c(groceries, list(data.frame(basket_id = i, item = item)))
  }
  i = i + 1
}

# Create a data frame
groceries = do.call(rbind, groceries)
print(groceries)

groceries$basket_id = factor(groceries$basket_id)
groceries = split(x=groceries$item, f=groceries$basket_id)
groceries = lapply(groceries, unique)
groceriestrans = as(groceries, "transactions")


groceries_rules = apriori(groceriestrans,
                          parameter=list(support=.001, confidence=.01, maxlen=9))
#inspect(groceries_rules)
plot(groceries_rules, jitter=0)

#plot top 10 lifts

# Sort rules by lift in descending order
rules_sorted <- sort(groceries_rules, by = "lift", decreasing = TRUE)
# Extract top 10 rules
top_10_rules <- head(rules_sorted, 10)
# Print a table of top 10 rules with selected metrics
top_10_rules_df <- as(top_10_rules, "data.frame")
# Plot the top 10 rules with a bar chart
barplot(top_10_rules_df$lift, names.arg = top_10_rules_df$rules,
        main = "Top 10 Rules by Lift", xlab = "Rule", ylab = "Lift",las = 2, cex.axis = 0.8,cex.lab = 0.8)


#plot bottom 10 lifts

# Sort rules by lift in ascending order
rules_sorted <- sort(groceries_rules, by = "lift", decreasing = FALSE)
# Extract bottom 10 rules
top_10_rules <- head(rules_sorted, 10)
# Print a table of bottom 10 rules with selected metrics
top_10_rules_df <- as(top_10_rules, "data.frame")
# Plot the bottom 10 rules with a bar chart
barplot(top_10_rules_df$lift, names.arg = top_10_rules_df$rules,
        main = "Bottom 10 Rules by Lift", xlab = "Rule", ylab = "Lift",las = 2, cex.axis = 0.8,cex.lab = 0.8)

#plot top 10 supports

# Sort rules by support in descending order
rules_sorted <- sort(groceries_rules, by = "support", decreasing = TRUE)
# Extract top 10 rules
top_10_rules <- head(rules_sorted, 10)
# Print a table of top 10 rules with selected metrics
top_10_rules_df <- as(top_10_rules, "data.frame")
# Plot the top 10 rules with a bar chart
barplot(top_10_rules_df$support, names.arg = top_10_rules_df$rules,
        main = "Top 10 Rules by support", xlab = "Rule", ylab = "support",las = 2, cex.axis = 0.8,cex.lab = 0.8)



#plot top 10 confidence

# Sort rules by confidence in descending order
rules_sorted <- sort(groceries_rules, by = "confidence", decreasing = TRUE)
# Extract top 10 rules
top_10_rules <- head(rules_sorted, 10)
# Print a table of top 10 rules with selected metrics
top_10_rules_df <- as(top_10_rules, "data.frame")
# Plot the top 10 rules with a bar chart
barplot(top_10_rules_df$confidence, names.arg = top_10_rules_df$rules,
        main = "Top 10 Rules by confidence", xlab = "Rule", ylab = "support",las = 2, cex.axis = 0.8,cex.lab = 0.8)


#plot bottom 10 confidence

# Sort rules by confidence in ascending order
rules_sorted <- sort(groceries_rules, by = "confidence", decreasing = FALSE)
# Extract top 10 rules
top_10_rules <- head(rules_sorted, 10)
# Print a table of top 10 rules with selected metrics
top_10_rules_df <- as(top_10_rules, "data.frame")
# Plot the top 10 rules with a bar chart
barplot(top_10_rules_df$confidence, names.arg = top_10_rules_df$rules,
        main = "bottom 10 Rules by confidence", xlab = "Rule", ylab = "support",las = 2, cex.axis = 0.8,cex.lab = 0.8)



# can now look at subsets driven by the plot
inspect(subset(musicrules, support > 0.035))
inspect(subset(groceries_rules, subset=support > 0.02 & confidence > 0.8))
inspect(subset(groceries_rules, lift > 10))

inspect(subset(groceries_rules, subset=lift > 10 & confidence > 0.6))


groceries_graph = associations2igraph(subset(groceries_rules, subset=lift > 10 & confidence > 0.6), associationsAsNodes = FALSE)
igraph::write_graph(groceries_graph, file='groceries.graphml', format = "graphml")