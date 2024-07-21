
# Ładowanie potrzebnych bibliotek -----------------------------------------


library(tidyverse)
library(knitr)
library(forcats)
library(graphics)
library(corrplot)
library(ggpubr)
#library(scales)
#library(rstatix)
#library(rworldmap)
options(scipen=999)



# Wgranie pliku potrzebnego do analizy ------------------------------------


suicide_rates_overview <- read_csv("Suicide Rates Overview.csv")

suicide_rates_overview <- suicide_rates_overview %>% na.omit()

# Pierwszy rzut oka na dane -----------------------------------------------

head(suicide_rates_overview)

glimpse(suicide_rates_overview)

summary(suicide_rates_overview)

sum(is.na(suicide_rates_overview))


table1 <- suicide_rates_overview %>% 
  select(year, suicides_no) %>%
  na.omit() %>% 
  group_by(year) %>% 
  summarise('liczba samobojstw' = sum(suicides_no)) %>% 
  arrange(desc(year))

kable(table1, col.names = c("Rok", "Liczba samobojstw"))

plot1 <- ggplot(suicide_rates_overview, aes(x = year, y = suicides_no)) + 
  geom_bar(stat = "identity", fill = "#4f5d75") +
  labs(x = "Rok", y = "Liczba samobojstw", title = "Liczba samobojstw na przesteni 1985 - 2020")+
  scale_y_continuous(breaks = seq(0, 400000,by = 50000))+
  scale_x_continuous(breaks = seq(1985, 2020,by = 1)) +
#  theme_minimal() +
  theme_classic() +
#  theme(plot.background = element_rect(fill = "#ffffff")) +
#  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0, size = 12, color = "#2d3142"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10,),
        axis.title.y = element_text(margin = margin(r = 10), size = 10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
        axis.text = element_text(size = 8))
  
plot1

# Bazując na wynikach zawartych w zbiorze danych i nagłemu spadkowi w 2016 roku zdecydowałem aby ograniczyć zakres lat do 1985-2015


# Histogram ---------------------------------------------------------------

hist <- ggplot(suicide_rates_overview) + 
  geom_histogram(aes(x = suicides_100k, after_stat(density)), binwidth = 4, fill = "antiquewhite4") +
  labs(title = "Histogram: ",
       x = "Ilosc samobojstw na 100 tys. mieszkancow",
       y = "Gestosc") +
  geom_density(aes(x = suicides_100k),alpha = 0.1, fill = "brown", color = "brown", lwd = 1) +
  facet_wrap(~sex, ncol = 3) + 
  theme_classic()
           
hist

shapiro.test(suicide_rates_overview$suicides_100k)

library(stats)
ks.test(suicide_rates_overview$suicides_100k, "pnorm", mean(suicide_rates_overview$suicides_100k), sd(suicide_rates_overview$suicides_100k))

library(nortest)
lillie.test(suicide_rates_overview$suicides_100k)

lillie.test(suicide_rates_overview$suicides_no)


# Ograniczenie danych oraz podzial na .... -------------------------------


suicide_rates_overview <- suicide_rates_overview %>% 
  filter(year < 2016)


# Srednia samobojstw na rok w podziale na generacje

avg_by_gen <- suicide_rates_overview %>% 
  select(generation, suicides_100k) %>% 
  na.omit() %>% 
  group_by(generation) %>% 
  summarise(Srednia = mean(suicides_100k))

plot2 <- ggplot(avg_by_gen, aes(y = fct_reorder(generation, Srednia), x = Srednia)) + 
  geom_bar(stat = "identity", fill = "#4f5d75") +
  labs(y = "", x = "Srednia liczba samobojstw", title = "Srednia liczba samobojstw na rok dla badanej eneracji na przesteni 1985 - 2015")+
#  scale_y_continuous(breaks = seq(0, 400000,by = 50000))+
#  scale_x_continuous(breaks = seq(1985, 2020,by = 1)) +
  #  theme_minimal() +
  theme_classic() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0, size = 12, color = "#2d3142"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10,),
        axis.title.y = element_text(margin = margin(r = 10), size = 10),
        axis.text.x = element_text( vjust = 1, hjust = 1, size = 8),
        axis.text = element_text(size = 8))

plot2


# Srednia samobojstw na rok w podziale na wiek

avg_by_age <- suicide_rates_overview %>% 
  select(age, suicides_100k) %>%
  na.omit() %>% 
  group_by(age) %>% 
  summarise(Srednia = mean(suicides_100k))

plot3 <- ggplot(avg_by_age, aes(y = fct_reorder(age, Srednia), x = Srednia)) + 
  geom_bar(stat = "identity", fill = "#4f5d75") +
  labs(y = "", x = "Srednia liczba samobojstw", title = "Srednia liczba samobojstw na rok dla grup wiekowych na przestrzeni 1985 - 2015")+
  #  scale_y_continuous(breaks = seq(0, 400000,by = 50000))+
  #  scale_x_continuous(breaks = seq(1985, 2020,by = 1)) +
  #  theme_minimal() +
  theme_classic() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0, size = 12, color = "#2d3142"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10,),
        axis.title.y = element_text(margin = margin(r = 10), size = 10),
        axis.text.x = element_text( vjust = 1, hjust = 1, size = 8),
        axis.text = element_text(size = 8))

plot3


# Srednia samobojstw na rok w podziale na plec
# 

avg_by_sex <- suicide_rates_overview %>% 
#  filter(country == "Poland") %>% 
  select(sex, suicides_100k) %>%
  na.omit() %>% 
  group_by(sex) %>% 
  summarise(Srednia = mean(suicides_100k))

plot4 <- ggplot(avg_by_sex, aes(y = fct_reorder(sex, Srednia), x = Srednia)) + 
  geom_bar(stat = "identity", fill = "#4f5d75") +
  labs(y = "", x = "Srednia liczba samobojstw", title = "Srednia liczba samobojstw na 100tys mieszkancow na rok dla grup wiekowych na przestrzeni 1985 - 2015")+
  #  scale_y_continuous(breaks = seq(0, 400000,by = 50000))+
  #  scale_x_continuous(breaks = seq(1985, 2020,by = 1)) +
  #  theme_minimal() +
  theme_classic() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0, size = 12, color = "#2d3142"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10,),
        axis.title.y = element_text(margin = margin(r = 10), size = 10),
        axis.text.x = element_text( vjust = 1, hjust = 1, size = 8),
        axis.text = element_text(size = 8))

plot4


avg_by_sex <- suicide_rates_overview %>% 
  #  filter(country == "Poland") %>% 
  select(sex, year, suicides_100k) %>%
  na.omit() %>% 
  group_by(sex, year) %>% 
  summarise(Srednia = mean(suicides_100k))

plot5 <- ggplot(avg_by_sex, aes(x = year, y = Srednia, color = sex)) + 
  geom_line(size = 1) +
  labs(y = "", x = "Srednia liczba samobojstw", title = "Srednia liczba samobojstw na 100tys mieszkancow na rok dla grup wiekowych na przestrzeni 1985 - 2015")+
#  scale_y_continuous(breaks = seq(0, 400000,by = 50000))+
  scale_x_continuous(breaks = seq(1985, 2015,by = 1)) +
  scale_color_discrete(name = "")+
  #  theme_minimal() +
  theme_classic() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0, size = 12, color = "#2d3142"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10,),
        axis.title.y = element_text(margin = margin(r = 10), size = 10),
        axis.text.x = element_text( vjust = 1, hjust = 1, size = 8),
        axis.text = element_text(size = 8))

plot5

# podzial na generacje

avg_by_gen_line <- suicide_rates_overview %>% 
  select(generation, year, suicides_100k) %>% 
  na.omit() %>% 
  group_by(generation, year) %>% 
  summarise(Srednia = mean(suicides_100k))

plot6 <- ggplot(avg_by_gen_line, aes(y = Srednia, x = year, color = generation)) + 
  geom_line() +
  labs(y = "", x = "Srednia liczba samobojstw", title = "Srednia liczba samobojstw na rok dla badanej eneracji na przesteni 1985 - 2015")+
  scale_x_continuous(breaks = seq(1985, 2015,by = 1)) +
  scale_color_discrete(name = "")+
  #  theme_minimal() +
  theme_classic() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0, size = 12, color = "#2d3142"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10,),
        axis.title.y = element_text(margin = margin(r = 10), size = 10),
        axis.text.x = element_text( vjust = 1, hjust = 1, size = 8),
        axis.text = element_text(size = 8))

plot6






# podzial na kraje

# top 15
# 
avg_by_country <- suicide_rates_overview %>% 
  select(country, suicides_100k) %>%
  group_by(country) %>% 
  summarise(Srednia = mean(suicides_100k)) %>% 
  top_n(10, Srednia) %>%
  arrange(desc(Srednia))

avg_by_country %>% 
  distinct(country)
  

plot5 <- ggplot(avg_by_country, aes(y = fct_reorder(country, Srednia), x = Srednia)) + 
  geom_bar(stat = "identity", fill = "#4f5d75") +
  labs(y = "", x = "Srednia liczba samobojstw", title = "Srednia liczba samobojstw na 100tys mieszkancow na rok dla grup wiekowych na przestrzeni 1985 - 2015")+
  #  scale_y_continuous(breaks = seq(0, 400000,by = 50000))+
  #  scale_x_continuous(breaks = seq(1985, 2020,by = 1)) +
  #  theme_minimal() +
  theme_classic() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0, size = 12, color = "#2d3142"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10,),
        axis.title.y = element_text(margin = margin(r = 10), size = 10),
        axis.text.x = element_text( vjust = 1, hjust = 1, size = 8),
        axis.text = element_text(size = 8))

plot5


# bottom 20
# 
avg_by_country_bottom <- suicide_rates_overview %>% 
  select(country, suicides_100k) %>%
  group_by(country) %>% 
  summarise(Srednia = mean(suicides_100k)) %>%
  filter(Srednia != 0) %>% 
  arrange(desc(Srednia)) %>%
  slice_tail(n = 10)

avg_by_country_bottom %>% 
  distinct(country)


plot6 <- ggplot(avg_by_country_bottom, aes(y = fct_reorder(country, Srednia), x = Srednia)) + 
  geom_bar(stat = "identity", fill = "#4f5d75") +
  labs(y = "", x = "Srednia liczba samobojstw", title = "Srednia liczba samobojstw na 100tys mieszkancow na rok dla grup wiekowych na przestrzeni 1985 - 2015")+
  #  scale_y_continuous(breaks = seq(0, 400000,by = 50000))+
  #  scale_x_continuous(breaks = seq(1985, 2020,by = 1)) +
  #  theme_minimal() +
  theme_classic() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0, size = 12, color = "#2d3142"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10,),
        axis.title.y = element_text(margin = margin(r = 10), size = 10),
        axis.text.x = element_text( vjust = 1, hjust = 1, size = 8),
        axis.text = element_text(size = 8))

plot6




```{r}

t.test(suicide_rates_overview$suicides_100k ~ suicide_rates_overview$sex)
t_test(suicides_100k ~ sex, data = suicide_rates_overview)


formuły <- str_c("suicides_100k", " ~ age") %>% 
  map(as.formula)
map(formuły, t_test, data = suicide_rates_overview)
```




# Korelacje ---------------------------------------------------------------

avg_table <- suicide_rates_overview %>%
  na.omit() %>% 
  group_by(country) %>% 
  summarise(gdp_per_capita = mean(gdp_per_capita), suicides_100k = mean(suicides_100k), 
            suicides_no = mean(suicides_no), gdp_for_year = mean(gdp_for_year)) %>% 
  select(gdp_per_capita, suicides_100k, suicides_no, gdp_for_year)



# wykres liniowy

ggplot(avg_table, aes(x = gdp_per_capita, y = suicides_100k)) +
  geom_point(color = "#4f5d75", fill = "#4f5d75") +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Korelacja pomiędzy produktem krajowym brutto na mieszkańca, a wskaźniekiem samobójstw na 100 tyś. mieszkańców") +
  xlab("gdp_per_capita") +
  ylab("suicides_100k") +
  theme_classic()

cor.test(avg_table$gdp_per_capita, avg_table$suicides_100k)

m

ggscatter(data = avg_table, x = "gdp_per_capita", y = "suicides_no",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "GDP per capita", ylab = "Suicides per 100,000 people",
          main = "Suicides vs. GDP per capita",
          color = "#4f5d75", palette = "jco", size = 3) +
  scale_x_log10(labels = scales::dollar_format(suffix = "")) +
  scale_y_log10(labels = scales::comma) +
  theme_classic() +
  labs(subtitle = "An analysis of the relationship between GDP per capita and suicides per 100,000 people")

##

ggplot(suicide_rates_overview, aes(x = gdp_per_capita, y = suicides_100k, color = 'red')) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Correlation between wt and mpg") +
  xlab("gdp_per_capita") +
  ylab("suicides_100k") +
  theme_classic()

m <- cor(avg_table$gdp_for_year, avg_table$suicides_100k)
summary(m)
cor.test(suicide_rates_overview$gdp_per_capita, suicide_rates_overview$suicides_100k)

m

ggscatter(data = suicide_rates_overview, x = "gdp_per_capita", y = "suicides_100k",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "GDP per capita", ylab = "Suicides per 100,000 people",
          main = "Suicides vs. GDP per capita",
          color = "#4f5d75", palette = "jco", size = 1) +
  scale_x_log10(labels = scales::dollar_format(suffix = "")) +
  scale_y_log10(labels = scales::comma) +
  theme_classic() +
  labs(subtitle = "An analysis of the relationship between GDP per capita and suicides per 100,000 people")

df <- data.frame(country = c("USA", "China", "India", "Japan"),
                 gdp = c(21, 14, 11, 5),
                 population = c(328, 1386, 1350, 126))

cor(suicide_rates_overview[, c("suicides_no", "country")])



# Regresja linowa ---------------------------------------------------------

avg_table <- suicide_rates_overview %>%
  na.omit() %>% 
  group_by(country, age) %>% 
  summarise(gdp_per_capita = mean(gdp_per_capita), suicides_100k = mean(suicides_100k), 
            suicides_no = mean(suicides_no), gdp_for_year = mean(gdp_for_year)) %>% 
  select(country, age, gdp_per_capita, suicides_100k, suicides_no, gdp_for_year)


lm <- lm(suicide_rates_overview$suicides_100k ~ suicide_rates_overview$gdp_per_capita, data = suicide_rates_overview)

summary(lm)

lm <- lm(suicides_100k ~  age + gdp_per_capita, data = avg_table )
summary(lm)


plot(lm)

suicide_rates_overview %>% 
distinct(generation)

t.test(suicide_rates_overview$suicides_100k ~ suicide_rates_overview$sex)
t_test(suicides_100k ~ sex, data = suicide_rates_overview)

t.test(suicide_rates_overview$suicides_100k ~ suicide_rates_overview$age)



summary(lm)

plot(suicide_rates_overview$gdp_per_capita, suicide_rates_overview$suicides_100k)
abline(lm, col = "red")



predictions <- predict(lm, newdata = suicide_rates_overview)

predictions

glm_model <- glm(country ~ suicides_100k, data = suicide_rates_overview, family = binomial)



##
##
##
corrplot(cor(avg_table),
         method = "number",
         type = "upper" # show only upper side
)


M = cor(avg_table)
set.seed(0)
testRes = cor.mtest(avg_table, conf.level = 0.95)

## specialized the insignificant value according to the significant level
corrplot(M, p.mat = testRes$p, sig.level = 0.05, order = 'hclust', addrect = 2)

## leave blank on no significant coefficient
corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig ='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag = FALSE)


round(cor(avg),
      digits = 2) # rounded to 2 decimals)


suicide_rates_overview %>% 
  select(year, gdp_per_capita, suicides_100k ) %>% 
  group_by(year) %>% 
  summarise(gdp_per_capita = mean(gdp_per_capita), suicides_100k = mean(suicides_100k) ) %>% 
  ggplot(aes(x = gdp_per_capita, y = suicides_100k)) +
  geom_point(colour = "#0c4c8a") +
  geom_smooth()+
  theme_minimal()

avg %>% 
  select(year, gdp_per_capita, suicides_100k ) %>% 
  group_by(year) %>% 
  summarise(gdp_per_capita = mean(gdp_per_capita), suicides_100k = mean(suicides_100k) ) %>% 
  ggscatter( y = "gdp_per_capita", x = "suicides_100k", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE,
          cor.coeff.args = list(method = "pearson", label.x = 4, label.sep = "\n"))





avg <- suicide_rates_overview %>% 
  select(year, gdp_per_capita, suicides_100k ) %>% 
  group_by(year) %>% 
  summarise(gdp_per_capita = mean(gdp_per_capita), suicides_100k = mean(suicides_100k) )



# Load data
data("mtcars")
df <- mtcars
df$cyl <- as.factor(df$cyl)
head(df[, c("wt", "mpg", "cyl")], 3)

# Basic plot
# +++++++++++++++++++++++++++
ggscatter(df, x = "wt", y = "mpg",
          color = "black", shape = 21, size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", label.x = 4, label.sep = "\n")
)

# loess method: local regression fitting
ggscatter(df, x = "wt", y = "mpg",
          add = "loess", conf.int = TRUE,
          add.params = list(color = "blue", fill = "lightgray"))


# Control point size by continuous variable values ("qsec")
ggscatter(df, x = "wt", y = "mpg",
          color = "#00AFBB", size = "qsec")


# Change colors
# +++++++++++++++++++++++++++
# Use custom color palette
# Add marginal rug
ggscatter(df, x = "wt", y = "mpg", color = "cyl",
          palette = c("#00AFBB", "#E7B800", "#FC4E07") )




# Add group ellipses and mean points
# Add stars
# +++++++++++++++++++
ggscatter(df, x = "wt", y = "mpg",
          color = "cyl", shape = "cyl",
          palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ellipse = TRUE, mean.point = TRUE,
          star.plot = TRUE)


# Textual annotation
# +++++++++++++++++
df$name <- rownames(df)
ggscatter(df, x = "wt", y = "mpg",
          color = "cyl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          label = "name", repel = TRUE)


data(mtcars)
M = cor(mtcars)
set.seed(0)

##  different color series
## COL2: Get diverging colors
## c('RdBu', 'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdYlBu')
## COL1: Get sequential colors
## c('Oranges', 'Purples', 'Reds', 'Blues', 'Greens', 'Greys', 'OrRd', 'YlOrRd', 'YlOrBr', 'YlGn')

wb = c('white', 'black')

par(ask = TRUE)

## different color scale and methods to display corr-matrix
corrplot(M, method = 'number', col = 'black', cl.pos = 'n')



corrplot(M, method = 'number')
corrplot(M)
corrplot(M, order = 'AOE')
corrplot(M, order = 'AOE', addCoef.col = 'grey')


corrplot(M, order = 'AOE',  cl.length = 21, addCoef.col = 'grey')
corrplot(M, order = 'AOE', col = COL2(n=10), addCoef.col = 'grey')

corrplot(M, order = 'AOE', col = COL2('PiYG'))
corrplot(M, order = 'AOE', col = COL2('PRGn'), addCoef.col = 'grey')
corrplot(M, order = 'AOE', col = COL2('PuOr', 20), cl.length = 21, addCoef.col = 'grey')

corrplot(M, order = 'AOE', col = COL2('PuOr', 10), addCoef.col = 'grey')

corrplot(M, order = 'AOE', col = COL2('RdYlBu', 100))
corrplot(M, order = 'AOE', col = COL2('RdYlBu', 10))


corrplot(M, method = 'color', col = COL2(n=20), cl.length = 21, order = 'AOE',
         addCoef.col = 'grey')
corrplot(M, method = 'square', col = COL2(n=200), order = 'AOE')
corrplot(M, method = 'ellipse', col = COL2(n=200), order = 'AOE')
corrplot(M, method = 'shade', col = COL2(n=20), order = 'AOE')
corrplot(M, method = 'pie', order = 'AOE')

## col = wb
corrplot(M, col = wb, order = 'AOE', outline = TRUE, cl.pos = 'n')

## like Chinese wiqi, suit for either on screen or white-black print.
corrplot(M, col = wb, bg = 'gold2',  order = 'AOE', cl.pos = 'n')


## mixed methods: It's more efficient if using function 'corrplot.mixed'
## circle + ellipse
corrplot(M, order = 'AOE', type = 'upper', tl.pos = 'd')
corrplot(M, add = TRUE, type = 'lower', method = 'ellipse', order = 'AOE',
         diag = FALSE, tl.pos = 'n', cl.pos = 'n')

## circle + square
corrplot(M, order = 'AOE', type = 'upper', tl.pos = 'd')
corrplot(M, add = TRUE, type = 'lower', method = 'square', order = 'AOE',
         diag = FALSE, tl.pos = 'n', cl.pos = 'n')

## circle + colorful number
corrplot(M, order = 'AOE', type = 'upper', tl.pos = 'd')
corrplot(M, add = TRUE, type = 'lower', method = 'number', order = 'AOE',
         diag = FALSE, tl.pos = 'n', cl.pos = 'n')

## circle + black number
corrplot(M, order = 'AOE', type = 'upper', tl.pos = 'tp')
corrplot(M, add = TRUE, type = 'lower', method = 'number', order = 'AOE',
         col = 'black', diag = FALSE, tl.pos = 'n', cl.pos = 'n')


## order is hclust and draw rectangles
corrplot(M, order = 'hclust')
corrplot(M, order = 'hclust', addrect = 2)
corrplot(M, order = 'hclust', addrect = 3, rect.col = 'red')
corrplot(M, order = 'hclust', addrect = 4, rect.col = 'blue')
corrplot(M, order = 'hclust', hclust.method = 'ward.D2', addrect = 4)

## visualize a  matrix in [0, 1]
corrplot(abs(M), order = 'AOE', col.lim = c(0, 1))
corrplot(abs(M), order = 'AOE', is.corr = FALSE,  col.lim = c(0, 1))


# when is.corr=TRUE, col.lim only affect the color legend
# If you change it, the color is still assigned on [-1, 1]
corrplot(M/2)
corrplot(M/2, col.lim = c(-0.5, 0.5))

# when is.corr=FALSE, col.lim is also used to assign colors
# if the matrix have both positive and negative values
# the matrix transformation keep every values positive and negative
corrplot(M*2, is.corr = FALSE, col.lim = c(-2, 2))
corrplot(M*2, is.corr = FALSE, col.lim = c(-2, 2) * 2)
corrplot(M*2, is.corr = FALSE, col.lim = c(-2, 2) * 4)

## 0.5~0.6
corrplot(abs(M)/10+0.5, col = COL1('Greens', 10))
corrplot(abs(M)/10+0.5, is.corr = FALSE, col.lim = c(0.5, 0.6), col = COL1('YlGn', 10))


## visualize a  matrix in [-100, 100]
ran = round(matrix(runif(225, -100, 100), 15))
corrplot(ran, is.corr = FALSE)

corrplot(ran, is.corr = FALSE, col.lim = c(-100, 100))

## visualize a matrix in [100, 300]
ran2 = ran + 200

# bad color, not suitable for a matrix in [100, 300]
corrplot(ran2, is.corr = FALSE, col.lim = c(100, 300), col = COL2(, 100))

# good color
corrplot(ran2, is.corr = FALSE, col.lim = c(100, 300), col = COL1(, 100))


## text-labels and plot type
corrplot(M, order = 'AOE', tl.srt = 45)

corrplot(M, order = 'AOE', tl.srt = 60)
corrplot(M, order = 'AOE', tl.pos = 'd', cl.pos = 'n')
corrplot(M, order = 'AOE', diag = FALSE, tl.pos = 'd')
corrplot(M, order = 'AOE', type = 'upper')
corrplot(M, order = 'AOE', type = 'upper', diag = FALSE)
corrplot(M, order = 'AOE', type = 'lower', cl.pos = 'b')
corrplot(M, order = 'AOE', type = 'lower', cl.pos = 'b', diag = FALSE)



#### color-legend
corrplot(M, order = 'AOE', cl.ratio = 0.2, cl.align = 'l')
corrplot(M, order = 'AOE', cl.ratio = 0.2, cl.align = 'c')
corrplot(M, order = 'AOE', cl.ratio = 0.2, cl.align = 'r')
corrplot(M, order = 'AOE', cl.pos = 'b')
corrplot(M, order = 'AOE', cl.pos = 'b', tl.pos = 'd')
corrplot(M, order = 'AOE', cl.pos = 'n')


## deal with missing Values
M2 = M
diag(M2) = NA
corrplot(M2)
corrplot(M2, na.label = 'o')
corrplot(M2, na.label = 'NA')


##the input matrix is not square
corrplot(M[1:8, ])
corrplot(M[, 1:8])

testRes = cor.mtest(mtcars, conf.level = 0.95)

## specialized the insignificant value according to the significant level
corrplot(M, p.mat = testRes$p, sig.level = 0.05, order = 'hclust', addrect = 2)

## leave blank on no significant coefficient
corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig ='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag = FALSE)

## add p-values on no significant coefficients
corrplot(M, p.mat = testRes$p, insig = 'p-value')

## add all p-values
corrplot(M, p.mat = testRes$p, insig = 'p-value', sig.level = -1)

## add significant level stars
corrplot(M, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'grey20', order = 'AOE')

## add significant level stars and cluster rectangles
corrplot(M, p.mat = testRes$p, tl.pos = 'd', order = 'hclust', addrect = 2,
         insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05),
         pch.cex = 0.9, pch.col = 'grey20')

# Visualize confidence interval
corrplot(M, lowCI = testRes$lowCI, uppCI = testRes$uppCI, order = 'hclust',
         tl.pos = 'd', rect.col = 'navy', plotC = 'rect', cl.pos = 'n')

# Visualize confidence interval and cross the significant coefficients
corrplot(M, p.mat = testRes$p, lowCI = testRes$lowCI, uppCI = testRes$uppCI,
         addrect = 3, rect.col = 'navy', plotC = 'rect', cl.pos = 'n')



res1 = cor.mtest(mtcars, conf.level = 0.95)
res2 = cor.mtest(mtcars, conf.level = 0.99)


## plot confidence interval(0.95), 'circle' method
corrplot(M, low = res1$uppCI, upp = res1$uppCI,
         plotCI = 'circle', addg = 'grey20', cl.pos = 'n')
corrplot(M, p.mat = res1$p, low = res1$lowCI, upp = res1$uppCI,
         plotCI = 'circle', addg = 'grey20', cl.pos = 'n')
corrplot(M, low = res1$lowCI, upp = res1$uppCI,
         col = c('white', 'black'), bg = 'gold2', order = 'AOE',
         plotCI = 'circle', cl.pos = 'n', pch.col = 'red')
corrplot(M, p.mat = res1$p, low = res1$lowCI, upp = res1$uppCI,
         col = c('white', 'black'), bg = 'gold2', order = 'AOE',
         plotCI = 'circle', cl.pos = 'n', pch.col = 'red')

## plot confidence interval(0.95), 'square' method
corrplot(M, low = res1$lowCI, upp = res1$uppCI,
         col = c('white', 'black'), bg = 'gold2', order = 'AOE',
         plotCI = 'square', addg = NULL, cl.pos = 'n')
corrplot(M, p.mat = res1$p, low = res1$lowCI, upp = res1$uppCI,
         col = c('white', 'black'), bg = 'gold2', order = 'AOE', pch.col = 'red',
         plotCI = 'square', addg = NULL, cl.pos = 'n')

## plot confidence interval0.95, 0.95, 0.99, 'rect' method
corrplot(M, low = res1$lowCI, upp = res1$uppCI, order = 'hclust',
         rect.col = 'navy', plotCI = 'rect', cl.pos = 'n')
corrplot(M, p.mat = res1$p, low = res1$lowCI, upp = res1$uppCI,
         order = 'hclust', pch.col = 'red', sig.level = 0.05, addrect = 3,
         rect.col = 'navy', plotCI = 'rect', cl.pos = 'n')
corrplot(M, p.mat = res2$p, low = res2$lowCI, upp = res2$uppCI,
         order = 'hclust', pch.col = 'red', sig.level = 0.01, addrect = 3,
         rect.col = 'navy', plotCI = 'rect', cl.pos = 'n')


## an animation of changing confidence interval in different significance level
## begin.animaton
par(ask = FALSE)
for (i in seq(0.1, 0, -0.005)) {
  tmp = cor.mtest(mtcars, conf.level = 1 - i)
  corrplot(M, p.mat = tmp$p, low = tmp$lowCI, upp = tmp$uppCI, order = 'hclust',
           pch.col = 'red', sig.level = i, plotCI = 'rect', cl.pos = 'n',
           mar = c(0, 0, 1, 0),
           title = substitute(alpha == x,
                              list(x = format(i, digits = 3, nsmall = 3))))
  Sys.sleep(0.15)
}
## end.animaton


