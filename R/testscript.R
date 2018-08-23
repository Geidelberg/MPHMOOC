#1

if (!require("showtext")) install.packages("showtext")

library(showtext)
## add the Arial font
font_add("Arial", regular = "arial.ttf",
         bold = "arialbd.ttf", italic = "ariali.ttf", bolditalic = "arialbi.ttf")



kidney_df <- data.frame(patient = seq(1:1005), no_kidneys = c(1, 3, 3, 3, 4, rep(2, 1000)))



setEPS()
windows.options(reset=TRUE)
postscript("1a.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device


barplot(table(kidney_df), ylab = "Frequency", xlab = "Number of kidneys", main = "Distribution of number of kidneys at birth")
dev.off()



# 1b

kidney_df <- data.frame(patient = seq(1:1005), no_kidneys = c(1, 3, 3, 3, 4, rep(2, 1000)))

require(ggplot2)
require(extrafont)


setEPS()
windows.options(reset=TRUE)
postscript("1b.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device

g1 = ggplot(kidney_df) + geom_bar(aes(x = no_kidneys), bins = 4, fill = "#fbb4ae", colour = "black", size = 0.8, width = 0.8) + theme_bw() + labs(x = "Number of kidneys", y = "Frequency") + ggtitle("Distribution of number of kidneys at birth") +
  theme(plot.title = element_text(hjust = 0.5, size = 16,  family="Arial"),text = element_text(size = 16),  axis.text.x = element_text(colour = "black"), axis.ticks.y = element_blank(), axis.text.y = element_blank())
g1

dev.off()



# ggsave(filename = "test.eps")

if (!require("ggplot2")) install.packages("ggplot2")


# 2 height

setEPS()
windows.options(reset=TRUE)
postscript("2a.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device

hist(c(1.2, 2, rnorm(n = 10000, mean = 1.6, sd = 0.1)), nclass = 40, ylab = "", xlab = "Height in metres", main = "Distribution of adult height in metres", axes = F)
axis(side = 1,  at=seq(1.2 , 2, 0.1), labels = seq(1.2 , 2, 0.1))
dev.off()




height_hist <- data.frame(Height = c(1.2, 2, rnorm(n = 10000, mean = 1.6, sd = 0.1)))



setEPS()
windows.options(reset=TRUE)
postscript("2b.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device

g2 = ggplot(height_hist) + geom_histogram(aes(x = Height), fill = "#b3cde3", col = "black", bins = 80) + theme_bw()+ labs(x = "Height in metres") + ggtitle("Distribution of adult height in metres")+
  theme(plot.title = element_text(hjust = 0.5, size = 16),text = element_text(size = 16),
        axis.text.x = element_text(colour = "black"), axis.ticks.y=element_blank(), axis.text.y = element_blank())
g2
dev.off()



# large SD
setEPS()
windows.options(reset=TRUE)
postscript("3a.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device

hist(c(0, 100, rnorm(n = 100000, mean = 50, sd = 12)), nclass = 101, ylab = "Frequency", xlab = "", main = "", axes = F)
axis(side = 1,  at=seq(0 , 100, 20), labels = seq(0 , 100, 20))
dev.off()



large_SD_hist <- data.frame(Height = c(rnorm(n = 100000, mean = 50, sd = 12)))

setEPS()
windows.options(reset=TRUE)
postscript("3b.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device

g3 = ggplot(large_SD_hist) + geom_histogram(aes(x = Height), fill = "#b3cde3", col = "black", bins = 100) + theme_bw()+ labs(x = "", y = "Frequency") +
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, 100))+
  theme(plot.title = element_text(hjust = 0.5, size = 16),text = element_text(size = 16),
        axis.text.x = element_text(colour = "black"), axis.ticks.y=element_blank(), axis.text.y = element_blank())
g3
dev.off()






# small SD
setEPS()
windows.options(reset=TRUE)
postscript("4a.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device

hist(c(0, 101, rnorm(n = 100000, mean = 50, sd = 4)), nclass = 101, ylab = "Frequency", xlab = "", main = "", axes = F)
axis(side = 1,  at=seq(0 , 100, 20), labels = seq(0 , 100, 20))

dev.off()


small_SD_hist <- data.frame(Height = c(rnorm(n = 100000, mean = 50, sd = 4)))

setEPS()
windows.options(reset=TRUE)
postscript("4b.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device

g4 = ggplot(small_SD_hist) + geom_histogram(aes(x = Height), fill = "#b3cde3", col = "black", bins = 100) + theme_bw()+ labs(x = "", y = "Frequency") +
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, 100))+
  theme(plot.title = element_text(hjust = 0.5, size = 16),text = element_text(size = 16),
        axis.text.x = element_text(colour = "black"), axis.ticks.y=element_blank(), axis.text.y = element_blank())
g4
dev.off()



# poisson SD

setEPS()
windows.options(reset=TRUE)
postscript("5a.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device

barplot(table(rpois(n = 100000, lambda = 4)), axes = F)
dev.off()


poisson_hist <- data.frame(Height = c(rpois(n = 100000, lambda = 4)))


setEPS()
windows.options(reset=TRUE)
postscript("5b.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device


g5 = ggplot(poisson_hist) + geom_bar(aes(x = Height), fill = "#b3cde3", col = "black") + theme_bw()+ labs(x = "", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),text = element_text(size = 16),
        axis.text.x = element_text(colour = "black"), axis.ticks.y=element_blank(), axis.text.y = element_blank())
g5
dev.off()




# 6 US
US_nutrition <- data.frame(Country = "United States", Gender = c("Men", "Women"), Percent_5_a_day = c(20.2, 29.5))
Pakistan_nutrition <- data.frame(Country = "Pakistan", Gender = c("Men", "Women"), Percent_5_a_day = c(100 - 99.2, 100 - 99.3))

# US
setEPS()
windows.options(reset=TRUE)
postscript("6a.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device

g6a = ggplot(US_nutrition) + geom_bar(aes(y = Percent_5_a_day, x = Gender, fill = Gender), stat = "identity", colour = "black") +
  scale_fill_brewer(type = "qual") + theme_bw() + labs(y = "%") + ggtitle("Percent of citizens of US who consume at \n least 5 portions of fruit or vegetables a day")+
  theme(strip.background = element_blank(), legend.position = "", plot.title = element_text(hjust = 0.5, size = 16), text = element_text(size = 16), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))+
  scale_y_continuous(limits = c(0, 70))
g6a
dev.off()

# Pakistan
setEPS()
windows.options(reset=TRUE)
postscript("6b.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device

g6b = ggplot(Pakistan_nutrition) + geom_bar(aes(y = Percent_5_a_day, x = Gender, fill = Gender), stat = "identity", colour = "black") +
  scale_fill_brewer(type = "qual") + theme_bw() + labs(y = "%") + ggtitle("Percent of citizens of Pakistan who consume at \n least 5 portions of fruit or vegetables a day")+
  theme(strip.background = element_blank(), legend.position = "", plot.title = element_text(hjust = 0.5, size = 16), text = element_text(size = 16), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) +
  scale_y_continuous(limits = c(0, 70))
g6b
dev.off()

# another country with high consumption
Ghana_nutrition <- data.frame(Country = "Ghana", Gender = c("Men", "Women"), Percent_5_a_day = c(100 - 36.6, 100 - 38.0))

setEPS()
windows.options(reset=TRUE)
postscript("6c.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device

g6c = ggplot(Ghana_nutrition) + geom_bar(aes(y = Percent_5_a_day, x = Gender, fill = Gender), stat = "identity", colour = "black") +
  scale_fill_brewer(type = "qual") + theme_bw() + labs(y = "%") + ggtitle("Percent of citizens of Ghana who consume at \n least 5 portions of fruit or vegetables a day")+
  theme(strip.background = element_blank(), legend.position = "", plot.title = element_text(hjust = 0.5, size = 16), text = element_text(size = 16), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) +
  scale_y_continuous(limits = c(0, 70))
g6c

dev.off()



Nutition_df <- rbind(US_nutrition, Pakistan_nutrition, Ghana_nutrition)

setEPS()
windows.options(reset=TRUE)
postscript("6d.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device

g6d = ggplot(Nutition_df) + geom_bar(aes(y = Percent_5_a_day, x = Gender, fill = Gender), stat = "identity", colour = "black") +
  scale_fill_brewer(type = "qual") +
  facet_wrap(~Country) + theme_bw() + labs(y = "%") + ggtitle("Percent of people who consume at \n least 5 portions of fruit or vegetables a day")+
  theme(strip.background = element_rect(fill = "white"), legend.position = "", plot.title = element_text(hjust = 0.5, size = 16), text = element_text(size = 16), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g6d
dev.off()


# 6.5
# US consumption of at least 5 fruit/veg by age group and gender

US_nutrition_extra_men <- data.frame(Gender = rep("Men", 6), Age = c("18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 64", "65 and over"),
                                 Percent_5_a_day = c(20.5, 18, 18.1, 19, 20.4, 27.9))

US_nutrition_extra_women <- data.frame(Gender = rep("Women", 6), Age = c("18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 64", "65 and over"),
                                     Percent_5_a_day = c(25, 24.7, 27.7, 29, 31.8, 38.1))
US_nutrition_extra_df <- rbind(US_nutrition_extra_men, US_nutrition_extra_women)

US_nutrition_extra_df$Age <- factor(US_nutrition_extra_df$Age, levels = c("18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 64", "65 and over"))

setEPS()
windows.options(reset=TRUE)
postscript("6e.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device


g6e = ggplot(US_nutrition_extra_df) + geom_bar(aes(y = Percent_5_a_day, x = Age, fill = Age), stat = "identity", colour = "black") +
  scale_fill_brewer(type = "seq", palette = "Greens") +
  facet_wrap(~Gender) + theme_bw() + labs(y = "%") + ggtitle("Percent of people who consume at least 5 portions of fruit or vegetables a day \n")+
  theme(strip.background = element_rect(fill = "white"), legend.position = "", plot.title = element_text(hjust = 0.5, size = 16), text = element_text(size = 16), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g6e

dev.off()








# 7 health survey england



fruit_veg_df <- data.frame(portions = c("Less than 1", "1 to 3", "3 to 5", "5 to 7", "More than 7"),
                          percent = c(15.2, 30.9, 29.0, 15.7, 9.3))

fruit_veg_df$portions <- factor(fruit_veg_df$portions, levels = c("Less than 1", "1 to 3", "3 to 5", "5 to 7", "More than 7"))

setEPS()
windows.options(reset=TRUE)
postscript("7.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device


g7 = ggplot(fruit_veg_df) + geom_bar(aes(x = portions, y = percent), stat =  "identity", fill = "#a6cee3", colour = "black", size = 0.5, width = 0.8) + theme_bw()+
  labs(x = "\ Portions", y = "Percent of respondants (%)") + ggtitle("Portions of fruit and vegetables \n eaten on previous day in England") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), text = element_text(size = 16), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g7
dev.off()



# 8  normal distribution mean 26

setEPS()
windows.options(reset=TRUE)
postscript("8a.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device

hist(c(10, 42, rnorm(n = 100000, mean = 26, sd = 2)), nclass = 101, ylab = "Frequency", xlab = "BMI", main = "", axes = F)
axis(side = 1,  at=seq(10 , 42, 4), labels = seq(10 , 42, 4))
dev.off()

setEPS()
windows.options(reset=TRUE)
postscript("8b.eps", width = 12, height  = 10)
showtext.begin() ## call this function after opening a device


normal_mean_26 <- data.frame(Height = c(rnorm(n = 100000, mean = 26, sd = 1)))
g8 = ggplot(normal_mean_26) + geom_histogram(aes(x = Height), fill = "#b3cde3", col = "black", bins = 100) + theme_bw()+ labs(x = "BMI", y = "Frequency") +
  scale_x_continuous(breaks = seq(22, 30, 1), limits = c(22, 30))+
  theme(plot.title = element_text(hjust = 0.5, size = 16),text = element_text(size = 16),
        axis.text.x = element_text(colour = "black"), axis.ticks.y=element_blank(), axis.text.y = element_blank())
g8
dev.off()




pdf("plots.pdf", width = 12, height = 8)
g1
g2
g3
g4
g5
g6a
g6b
g6c
g6d
g6e
g7
g8
dev.off()
