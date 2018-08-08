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

hist(rnorm(n = 1000, mean = 1.6, sd = 0.1), ylab = "", xlab = "Height in cm", main = "Distribution of adult height in cm")
axis(side = 2, labels = F, tick = -0.01)
