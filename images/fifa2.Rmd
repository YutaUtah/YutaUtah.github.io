---
title: "fifa2"
author: "Yuta Hayashi"
date: "5/5/2019"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r,message=FALSE warning=FALSE}
library(tidyverse)
library(scales)
library(ggthemes)
library(kableExtra)
library(readr)
library(plotly)
options(scipen = 999)
```

```{r,warning=FALSE, message=FALSE}
fifa_data <- read_csv("data.csv") %>% select(-X1)
```


```{r}
#Data manipulation in this chunk includes the modification of salary of players
fifa_data <- fifa_data %>%
  mutate(ValueMultiplier = ifelse(str_detect(Value, "K"), 1000, ifelse(str_detect(Value, "M"), 1000000, 1))) %>%
  mutate(ValueNumeric_pounds = as.numeric(str_extract(Value, "[[:digit:]]+\\.*[[:digit:]]*")) * ValueMultiplier) %>%
  mutate(Position = ifelse(is.na(Position), "Unknown", Position))

fifa_data <- fifa_data %>%
  mutate(WageMultiplier = ifelse(str_detect(Wage, "K"), 1000, ifelse(str_detect(Wage, "M"), 1000000, 1))) %>%
  mutate(WageNumeric_pounds = as.numeric(str_extract(Wage, "[[:digit:]]+\\.*[[:digit:]]*")) * WageMultiplier)


positions <- unique(fifa_data$Position)

gk <- "GK"
defs <- positions[str_detect(positions, "B$")]
mids <- positions[str_detect(positions, "M$")]
f1 <- positions[str_detect(positions, "F$")]
f2 <- positions[str_detect(positions, "S$")]
f3 <- positions[str_detect(positions, "T$")]
f4 <- positions[str_detect(positions, "W$")]
fwds <- c(f1, f2, f3, f4)

fifa_data <- fifa_data %>% 
  mutate(PositionGroup = ifelse(Position %in% gk, "GK", ifelse(Position %in% defs, "DEF", ifelse(Position %in% mids, "MID", ifelse(Position %in% fwds, "FWD", "Unknown")))))

fifa_data <- fifa_data %>%
  mutate(AgeGroup = ifelse(Age <= 20, "20 and under", ifelse(Age > 20 & Age <=25, "21 to 25", ifelse(Age > 25 & Age <= 30, "25 to 30", ifelse(Age > 30 & Age <= 35, "31 to 35", "Over 35")))))
```

end of the data manipulation

```{r,message=FALSE}

fifa_data %>%
  ggplot(aes(x= Overall)) +
  geom_histogram(color = "white", fill = "grey") +
  ggtitle("Player Ratings Distribution") 
```

```{r}
fifa_data %>%
  filter(!PositionGroup %in% c("GK", "Unknown")) %>%
  group_by(Age) %>%
  summarize(Rating = mean(Overall)) %>%
  ggplot(aes(x= Age, y= Rating, group = 1)) +
  geom_line(color = "grey50", size = 1) +
  ggtitle("The Age Curve Flattens Off", subtitle = "Player ratings tend not to get better after the age of 30") +
  xlab("Age") 
```


```{r}
fifa_data %>%
  filter(!PositionGroup %in% c("GK", "Unknown")) %>%
  group_by(PositionGroup, Age) %>%
  summarize(Rating = mean(Overall)) %>%
  ggplot(aes(x= Age, y= Rating, group = PositionGroup)) +
  geom_line(size = 0.5, color = "black") +
  facet_wrap(~ PositionGroup, ncol = 1) +
  theme(strip.background = element_rect(fill = "darkgrey"), strip.text = element_text(colour = "white", face = "bold"))
```


```{r,message=FALSE,warning=FALSE}
#Valuation on a log scale, so differences \nbetween the age groups are significant
#Players Are In High Demand In Their Mid-20s
fifa_data %>%
  ggplot(aes(x= AgeGroup, y= ValueNumeric_pounds)) +
  geom_boxplot(fill = "darkgrey") +
  scale_y_log10(labels = dollar_format(prefix = "€")) +
  ggtitle("Log Valuation Versus Age Group") 
```

```{r,warning=FALSE,fig.height=5}
a <- fifa_data %>%
  filter(PositionGroup != "Unknown") %>%
  ggplot(aes(x= PositionGroup, y= ValueNumeric_pounds)) +
  geom_boxplot(fill = "grey") +
  scale_y_log10(labels = dollar_format(prefix = "€")) +
  ggtitle("Player Value Comparison")


b <- fifa_data %>%
  filter(PositionGroup != "Unknown") %>%
  ggplot(aes(x= Position, y= ValueNumeric_pounds)) +
  geom_boxplot(fill = "darkgrey") +
  scale_y_log10(labels = dollar_format(prefix = "€")) +
  coord_flip() +
  facet_wrap(~ PositionGroup, scales = "free") 
gridExtra::grid.arrange(a, b)
```

```{r}
fifa_data <- fifa_data  %>%
  mutate(AttackingRating = (Finishing + LongShots + Penalties + ShotPower + Positioning) /5)

data_cluster <- fifa_data %>%
  filter(PositionGroup != "Unknown") %>%
  filter(PositionGroup != "GK") %>%
  mutate(RoomToGrow = Potential - Overall) %>%
  select_if(is.numeric) %>%
  select(-ID, -`Jersey Number`, -AttackingRating, -starts_with("Value"), - starts_with("Wage"), -starts_with("GK"), -Overall)

scaled_data <- scale(data_cluster)
```



```{r}
set.seed(14)

wss <- 0


#fviz_nbclust(scaled_data, kmeans, method = "wss",nstart = 50)

for (j in 1:10) {
  km.out <- kmeans(scaled_data[1:4000,], centers = j,iter.max=50, nstart = 500)
  # Save total within sum of squares to wss variable
  wss[j] <- km.out$tot.withinss
}

# create a DF to use in a ggplot visualisation
wss_df <- data.frame(num_cluster = 1:10, wgss = wss)

ggplot(data = wss_df, aes(x=num_cluster, y= wgss)) + 
  geom_line(color = "lightgrey") + 
  geom_point(color = "blue", size = 2) +
  labs(title = "elbow method")

```

```{r}
wisc.km <- kmeans(scale(data_cluster), centers = 5, nstart = 20)
#wisc.km <- kmeans(scale(data_cluster), centers = 2, nstart = 20)

data_cluster$Cluster <- wisc.km$cluster
data_cluster$ValueNumeric_pounds <- cluster_analysis$ValueNumeric_pounds

cluster_data <- fifa_data %>%
  filter(PositionGroup != "Unknown") %>%
  filter(PositionGroup != "GK") %>%
  mutate(Cluster = wisc.km$cluster)

cluster_analysis <- cluster_data %>%
  select(ID, Name, Club, Age, PositionGroup, Overall, Cluster, ValueNumeric_pounds)

table(cluster_analysis$Cluster, cluster_analysis$PositionGroup)
```



```{r}
cluster_analysis$Cluster <- as.factor(cluster_analysis$Cluster)
data_cluster$Cluster <- as.factor(cluster_analysis$Cluster)
cluster_data$Cluster <- as.factor(cluster_analysis$Cluster)

q  <- cluster_data %>%
ggplot(aes(x= Age, y= log(ValueNumeric_pounds), color=Cluster, pch=19)) +
  geom_point(position = "jitter", shape = 21) +
  scale_y_continuous(labels = dollar_format(prefix = "€")) +
  ggtitle("Player's Age Plotted against Their Value") 

q

Cluster4 <- cluster_data %>% filter(Cluster == 4)

mean(Cluster3$`Preferred Foot`)

d <- Cluster4 %>%
  ggplot(aes(x= `Position`, y= ValueNumeric_pounds)) +
  geom_boxplot(fill = "grey") +
  scale_y_log10(labels = dollar_format(prefix = "€")) +
  ggtitle("Player Value Comparison")

d

```









