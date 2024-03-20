library(arules)
library(arulesViz)

# https://www.kirenz.com/blog/posts/2020-05-14-r-association-rule-mining/

# create a list of baskets
market_basket <-  
  list(  
    c("apple", "beer", "rice", "meat"),
    c("apple", "beer", "rice"),
    c("apple", "beer"), 
    c("apple", "pear"),
    c("milk", "beer", "rice", "meat"), 
    c("milk", "beer", "rice"), 
    c("milk", "beer"),
    c("milk", "pear")
  )

# set transaction names (T1 to T8)
names(market_basket) <- paste("T", c(1:8), sep = "")

# support is how popular a set of items is
# confidence tells us how likely item Y is to be purchased given item X is purchased, measured by proportion of transactions with item X in which Y also appears
# lift tells how likely Y is purchased when X is purchased controlling for how popular items X and Y are
# lift == 1 no association between items
# lift > 1 Y is more likely to be bought if X is bought
# lift < 1 Y is less likely to be bought if X is bought

# apriori algorithm reduces the number of items needed to examine
# eliminates itemsets by looking at smaller sets first and recognizing a large set cannot be frequent unless all subsets are
# if an item set is infrequent all its subsets must also be infrequent
# allows you to "prune" number of itemsets that need to be examined

# use function of arules package to load data into transaction class object
trans <- as(market_basket, "transactions")

# inspect data
# 8 transactions with 6 distinct items
dim(trans)
# list of distinct items in data
itemLabels(trans)
# summary
summary(trans)

# look at transactions and items in matrix
image(trans)
itemFrequencyPlot(trans, topN = 10, cex.names = 1)
# apple, milk, and rice all have support (item frequence) of 50%

# analyze using apriori algorithm
# apriori() function requires minimum support and minimum confidence constraint at same time 
# parameter option lets you set support threshold and confidence threshold and max length of items
# defaults are support threshold 0.1 and confidence threshold 0.8
rules <- apriori(trans,
                 parameter = list(supp = 0.3, conf = 0.5,
                                  maxlen = 10,
                                  target = "rules"))
summary(rules)
inspect(rules)
# rules 1-4 with empty LHS mean no matter what other items are involved the item in the RHS will appear with the probability given by the rule's confidence (which equals the support)
# avoid these by adding argument parameter = list(minlen = 2)
rules <- apriori(trans,
                 parameter = list(supp = 0.3, conf = 0.5,
                                  maxlen = 10, minlen = 2,
                                  target = "rules"))
inspect(rules)

# set specific rules with appearance to set LHS (if) or RHS (then)
# to see what items customers buy before buying beer
beer_rules_rhs <- apriori(trans,
                          parameter = list(supp = 0.3, conf = 0.5,
                                           maxlen = 10, minlen = 2),
                          appearance = list(default = "lhs", default = "beer"))
inspect(beer_rules_rhs)

# to see what items customers buy after buying beer
beer_rules_lhs <- apriori(trans,
                          parameter = list(supp = 0.3, conf = 0.5,
                                           maxlen = 10, minlen = 2),
                          appearance = list(lhs = "beer", default = "rhs"))
inspect(beer_rules_lhs)

# mining association rules often results in large number of found rules
# use areulesViz to visualize results: scatterplot, interactive scatterplot, individual rule representations

# draws a 2D scatter plot with different measures of intersecting ness (parameter "measure") on the axes and a third measure (parameter"shading") is represented by the color of the points
plot(rules)
# use only "confidence" as a specific measure of interest
plot(rules, measure = "confidence")

# interactive scatter plot
plot(rules, engine = "plotly")

# graph based techniques concentrate on relationship between individual items in rule set
# represent rules as graph with items as labeled vertices
# and rules as vertices connected to items using arrows
subrules <- head(beer_rules_lhs, n = 10, by = "confidence")
plot(subrules, method = "graph", engine = "htmlwidget")

# parallel coordinate plot
plot(subrules, method = "paracoord")
