#1

kidney_df <- data.frame(patient = seq(1:1005), no_kidneys = c(1, 3, 3, 3, 4, rep(2, 1000)))

barplot(table(kidney_df), ylab = "Frequency", xlab = "Number of kidneys", main = "Distribution of number of kidneys at birth")
ylab()


# 1b

kidney_df <- data.frame(patient = seq(1:1005), no_kidneys = c(1, 3, 3, 3, 4, rep(2, 1000)))

require(ggplot2)

ggplot(kidney_df) + geom_histogram(aes(x = no_kidneys), bins = 4) + theme_bw() +
  labs(x = "Number of kidneys", y = "Frequency") + ggtitle("Distribution of number of kidneys at birth") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size=20))

if (!require("ggplot2")) install.packages("ggplot2")


# 2 height


hist(c(1.2, 2, rnorm(n = 10000, mean = 1.6, sd = 0.1)), nclass = 40, ylab = "", xlab = "Height in cm", main = "Distribution of adult height in cm", axes = F)
axis(side = 1,  at=seq(1.2 , 2, 0.1), labels = seq(1.2 , 2, 0.1))



# large SD

hist(c(0, 100, rnorm(n = 100000, mean = 50, sd = 12)), nclass = 101, ylab = "Frequency", xlab = "", main = "", axes = F)
axis(side = 1,  at=seq(0 , 100, 20), labels = seq(0 , 100, 20))



# small SD

hist(c(0, 101, rnorm(n = 100000, mean = 50, sd = 4)), nclass = 101, ylab = "Frequency", xlab = "", main = "", axes = F)
axis(side = 1,  at=seq(0 , 100, 20), labels = seq(0 , 100, 20))


# poisson SD


barplot(table(rpois(n = 100000, lambda = 4)), axes = F)

