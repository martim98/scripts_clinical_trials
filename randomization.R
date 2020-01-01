# randomization chapter
# graphical representation

#############################################################
# Categorical data
## insert data

table <- matrix()
colnames(tabela) <- c()
rownames(tabela) <- c()

## without total column in results
## prop.table() by row column and total
prop.table()

## graphics
pie()
barplot(, beside = T, 
        legend.text = rownames()) # beside to be side by side

#Continuos data
hist(, right = F, prob = T)
boxplot()

#discrete data
table()
summary()
TabFreq()


#############################################################