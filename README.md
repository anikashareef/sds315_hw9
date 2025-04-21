# sds315_hw9

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(knitr)
library(lubridate)
library(mosaic)
library(effectsize)

```

## 1

#### A \newline

```{r, message=FALSE, echo=FALSE,fig.width=6, fig.height=3}

#skips / response = # of solder skips on circuit board (manufacturing flaws)

#load in data set
solder <- read_csv("solder.csv")

#make ggplot for skips and opening 
ggplot(data=solder)+
  geom_jitter(aes(x=Opening, y=skips),color="cornflowerblue")+
  labs(
    title="# of Skips aross Opening Size",
    x= "Opening Size",
    y= "# of Skips")+
  theme_minimal()

```

```{r, message=FALSE, echo=FALSE,fig.width=5, fig.height=3}

#make ggplot for skips and solder 
ggplot(data=solder)+
  geom_jitter(aes(x=Solder, y=skips),color="cornflowerblue")+
  labs(
    title="# of Skips: Thick vs. Thin",
    x= "Solder Thickness Level",
    y= "# of Skips")+
  theme_minimal()
```
#### B

```{r echo=FALSE, message=FALSE, warning=FALSE}

#make model
solder_model = lm(skips ~ Opening + Solder + Solder:Opening, data=solder)

estimates = coef(solder_model)

#ci
conf_int = confint(solder_model, level = 0.95)

model_table <- cbind(Estimate = estimates, conf_int)
model_table





```
## 2 
									

#### A

```{r echo=FALSE, message=FALSE, warning=FALSE}

#load data set
groceries <- read_csv("groceries.csv")

#calculate mean price 
store_means <- groceries |>
  select(Store, Price, Product) |>
  group_by(Store) |>
  summarize(mean_price=mean(Price))

#plot
ggplot(data=store_means)+
  geom_col(aes(x=Store, y=mean_price), fill="cornflowerblue")+
  coord_flip()+
   labs(
    x = "Store",
    y = "Average Price ($)",
    title = "Average Price of Products Across Stores"
  ) +
  theme_minimal()


```

#### **B**			

```{r echo=FALSE, message=FALSE, warning=FALSE}

#wrangle
store_counts <- groceries |>
  group_by(Product, Store) |>
  summarize() |>
  group_by(Product) |>
  summarize(store_count = n())

#plot
ggplot(data=store_counts)+
  geom_col(aes(x=store_count,y=Product), fill="cornflowerblue") +
  labs(
    x = "Number of Stores Selling Product",
    y = "Product",
    title = "Store Availability per Product") +
  theme_minimal()+
  theme_minimal()


```


#### **C**

```{r echo=FALSE, message=FALSE, warning=FALSE}
#regression
groceries_model  = lm(Price ~ Product + Type, data=groceries)
#ci
grocieries_confint = confint(groceries_model, level = 0.95)

```

```{r echo=FALSE, message=FALSE, warning=FALSE}
#regression
groceries_model2  = lm(Price ~ Product + Store, data=groceries)
coef(groceries_model2)

```



```{r echo=FALSE, message=FALSE, warning=FALSE}
#ci 
confint(groceries_model2, level=0.95)
```


#### F

```{r echo=FALSE, message=FALSE, warning=FALSE}
#mutate
groceries <- groceries|>
  mutate(Income10k=Income/10000)
#regression model
groceries_model3 = lm(Price ~ Product + Income10k, data=groceries)
coef(groceries_model3)
#standardize coeffecients 
standardize_parameters(groceries_model3)
```
