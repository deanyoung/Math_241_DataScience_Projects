---
title: "Interest Rate Effects on S&P 500 Yield"
author: "Dean Young"
output: html_document
---

####Note: No part of this analysis constitutes investment advice in any form or fashion. Invest at your own risk. The author of this analysis holds a total US stock market index which includes the S&P 500.

> Data Sources:   
> US Treasury and Fed Funds rates were drawn from Quandl.com via the Quandl package  
> Inflation data drawn from St. Louis Federal Reserve (FRED)  
> S&P 500 Yields drawn from multpl.com (pulled from Robert Schiller's book: "Irrational Exuberance")  

###Introduction

This project attempts to examine the "spread" effect of interest rates (from US treasury bonds) on stock yields (from the S&P 500). It is well accepted among participants in financial markets that movements in interest rates have a huge effect on the stock market. This discussion is even more relevant in today's time, as Janet Yellen and the Fed are contemplating raising the current rock-bottom interest rates now that the US economy has recovered. 

A basic understanding of finance is expected of the reader. The basic concepts include understanding differences between a bond and stock, interest rates, yields, PE ratios, expected returns, inflation and other concepts. 

A cursory understanding of time series analysis is recommended for the reader, although not required. I will take the time to explain the time series concepts included in this analysis.

```{r,echo=FALSE}
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(Quandl))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tseries))
suppressPackageStartupMessages(library(lmtest))
Quandl.auth("uipvA4VQdjDviQ9TT3cE")
```

```{r,echo=FALSE,cache=TRUE} 
# Data cleaning and set up
inflation <- read.csv("inflation.csv") %>% tbl_df() %>% rename(Infl = Inflation)
inflation$Date <- dmy(inflation$Date)
inflation %<>% mutate(year=year(Date)) %>% select(Infl,year)

Fed.Fund <- Quandl("FRED/DFF") %>% tbl_df() %>% mutate(Date = ymd(Date), year=year(Date)) %>% rename(rate=Value)
One.Year <- Quandl("FRED/DTB1YR") %>% tbl_df() %>% mutate(Date = ymd(Date), year=year(Date)) %>% rename(rate=Value)
Three.Year <- Quandl("FRED/DGS3") %>% tbl_df() %>% mutate(Date = ymd(Date), year=year(Date)) %>% rename(rate=Value)
Five.Year <- Quandl("FRED/DGS5") %>% tbl_df() %>% mutate(Date = ymd(Date), year=year(Date)) %>% rename(rate=Value)
Ten.Year <- Quandl("FRED/DGS10") %>% tbl_df() %>% mutate(Date = ymd(Date), year=year(Date)) %>% rename(rate=Value)
Thirty.Year <- Quandl("FRED/DGS30") %>% tbl_df() %>% mutate(Date = ymd(Date), year=year(Date)) %>% rename(rate=Value)
Stock <- read.csv("SP 500 PE Ratios.csv") %>% tbl_df()
Stock$Date <- dmy(Stock$Date)

# need a function to fix lubridates' issue with years before 1969 (assumes <69 means 2069 not 1969)
foo <- function(x){
  m <- year(x) %% 100
  year(x) <- ifelse(m > 15, 1900+m, 2000+m)
  x
}

Stock$Date <- foo(Stock$Date)
Stock %<>% arrange(Date) %>% mutate(PE = (1/PE)*100) %>% rename(stock.rate=PE) %>% mutate(year=year(Date))

Stock.Y <- Stock %>% group_by(year) %>% summarise(rate=mean(stock.rate)) %>% filter(year>1961 & year<2015) %>%
  mutate(type="S&P 500")
Fed.Fund.Y <- Fed.Fund %>% group_by(year = year(Date)) %>% summarise(rate=mean(rate)) %>% mutate(dur="Fed Funds")
One.Year.Y <- One.Year %>% group_by(year = year(Date)) %>% summarise(rate=mean(rate)) %>% mutate(dur="One Year")
Three.Year.Y <- Three.Year %>% group_by(year = year(Date)) %>% summarise(rate=mean(rate)) %>% mutate(dur="Three Year")
Five.Year.Y <- Five.Year %>% group_by(year = year(Date)) %>% summarise(rate=mean(rate)) %>% mutate(dur="Five Year")
Ten.Year.Y <- Ten.Year %>% group_by(year = year(Date)) %>% summarise(rate=mean(rate)) %>% mutate(dur="Ten Year")
Thirty.Year.Y <- Thirty.Year %>% group_by(year = year(Date)) %>% summarise(rate=mean(rate)) %>% mutate(dur="Thirty Year")

spread <- bind_rows(Fed.Fund.Y,One.Year.Y) %>% bind_rows(Three.Year.Y) %>% bind_rows(Five.Year.Y) %>% 
  bind_rows(Ten.Year.Y) %>% bind_rows(Thirty.Year.Y) %>% mutate(year=as.character(year))

all.years <- spread %>% group_by(dur) %>% summarise(rate=mean(rate)) %>% mutate(year="all")

spread.select <- spread %>% filter(year==2015 | year==1981 | year==1984) %>% bind_rows(all.years)
spread.select$dur2 <- factor(spread.select$dur, levels=c("Fed Funds","One Year","Three Year","Five Year","Ten Year","Thirty Year"))

Reg.Data2 <- Three.Year.Y %>% select(year,bond.nom.rate = rate) %>% mutate(lag.year=year-1) %>%
  inner_join(rename(inflation,lag.Infl=Infl),by=c("lag.year"="year")) %>% 
  inner_join(select(Stock.Y,year,stock.nom.rate = rate),by="year")

Reg.Data2 %<>% mutate(bond.exp.rate=bond.nom.rate-lag.Infl,stock.exp.rate=stock.nom.rate-lag.Infl)

Graph.Data2 <- select(Reg.Data2,year,rate=stock.nom.rate) %>% mutate(type="Nom.Stock")
Graph.Data2 <- select(Reg.Data2,year,rate=bond.nom.rate) %>% mutate(type="Nom.Bond") %>% bind_rows(Graph.Data2)
Graph.Data2 <- select(Reg.Data2,year,rate=bond.exp.rate) %>% mutate(type="Exp.Bond") %>% bind_rows(Graph.Data2)
Graph.Data2 <- select(Reg.Data2,year,rate=stock.exp.rate) %>% mutate(type="Exp.Stock") %>% bind_rows(Graph.Data2)
Graph.Data2$type <- as.factor(Graph.Data2$type)

Reg.Data.Dif2 <- Reg.Data2 %>% 
  select(-lag.Infl,-lag.year) %>% 
  mutate(bond.n.dif=bond.nom.rate-lag(bond.nom.rate),bond.e.dif=bond.exp.rate-lag(bond.exp.rate),
         stock.n.dif=stock.nom.rate-lag(stock.nom.rate),stock.e.dif=stock.exp.rate-lag(stock.exp.rate)) %>% 
  select(year,bond.n.dif,bond.e.dif,stock.n.dif,stock.e.dif) %>% slice(2:length(year))
```

###Spreads

Interest rates on fixed-income securities are often evaluated on a spread basis. The idea behind the spread basis is that you aren't competing for absolute returns, but rather, relative returns. These relative returns are often dictated by risk and liquidity. If the US is selling treasury bonds with an interest rate at 5%, then a corporation who issues bonds should pay a certain spread above the rate of the treasury bond, maybe a 2% spread (so 7% on the bond). This is because the corporation is more likely to default than the US and has to pay a premium for the risk. If the treasury bond rate is just 2%, then the corporation can get away with only paying 4% on their bonds. 

Spreads can also manifest from liquidity concerns. If I am faced with a choice between a 10-year bond and a 30-year bond paying the same interest rate, I'm definitely going to pick the 10-year bond. I get the same interest rate for both bonds, and I get my principal back sooner, so I am not as contrained liquidity-wise. That means for a longer term bond to be attractive, it must pay a spread above shorter term bonds. 

A visualization of this concept is presented below, using various terms of US treasury bonds and the Federal Funds rate averaged by year. Specific years of returns were picked out to show different eras of very low rates and very high rates. The data set ranges from 1962 to present day. 

```{r,echo=FALSE}
ggplot(spread.select, aes(x=dur2,y=rate, group=year)) + geom_point(aes(col=year),size=5) + geom_line(aes(col=year)) +
  xlab("Loan/Bond Duration") + ylab("Nominal Interest Rate") + ggtitle("Interest Rate Spreads") +
  scale_color_discrete(name="Year")
```

Overall, the rates behave as one would expect from the spread basis hypothesis. It is worthy to note that in 1981, the yield curve was "inverted" meaning long term rates were actually less than short term rates. This happens often when investors are very bearish on the economy. If investors believe the economy is doing/will be doing very poorly, then they expect that short term rates will drop significantly in the future when the Fed lowers interest rates to combat recessions. This means investors want to lock in decent rates for a long period by investing in long term bonds. As investors flood into long term bonds, the rates for those bonds drop below short term bonds. 1981 was during a period of a "stagflation" where economic growth was stalling and hyper-inflation was a huge issue.

The inverse of a stock's PE ratio (price of the stock divided by its trailing 12 month net earnings) can be seen as the stock's yield. Stock's are considerably more risky that US treasury bonds and should be subject to the spread basis established before. The typical mutual fund investor holds stocks for in between 3-4 years so 3 year US treasury bond rates will be used in this analysis. One would expect that bond rates and stock yields to be positive correlated to maintain the spread basis.

###Stationary/Non-Stationary Time Series (Optional if you already understand time series)
Does the following graph depict a strong correlation?
![](http://i.imgur.com/k0PiAmR.png)
![](http://i.imgur.com/wqAA3wf.png)
Source:[http://academic.reed.edu/economics/parker/312/tschapters/S13_Ch_2.pdf (page 2)](http://academic.reed.edu/economics/parker/312/tschapters/S13_Ch_2.pdf)

The statistical significance of the slope seems to suggest so. The R-squared is also phenomenally good. The intuition is there too since you would expect that as American's get richer, they can spend more on leisure activites such as attending baseball games. It all makes sense...except that GDP/capita in this analysis is actually GDP/capita of Botswana and not the US. What the heck is going on?

This is an example of a spurious regression. If two independent and non-stationary time series are regressed against each other, the probability of a type 1 error occurs much more frequently than 1 in 20 as an $\alpha=.05$ test would suggest. In order to fix this problem, you have to use stationary time series. To convert a non-stationary time series to a stationary one, taking the difference usually suffices. As shown in the regression table above, once you take the difference of the variables, the correlation statistics fall apart since there's no reason for Botswana's GDP/capita to affect baseball game attendence in the US.

A critical identifying difference between stationary and non-stationary time series is that stationary time series revert to a given mean over time. Non-stationary time series can take on a random walk model where movements at time t is not determined by any of its past movements. They can also take on a explosive model where the variable grows contintually over time. More information about time series models can be found in the sources.

A stationary time series is modeled by $y_{t}=\rho*y_{t-1}+\epsilon_{t}$ (an AR(1) model) where $\left|\rho\right| < 1$  and $\epsilon_{t}$ has expected value 0 and variance $\sigma_{\epsilon}^2$. If $\left|\rho\right|$ is exactly 1, then the model is a random walk. If $\left|\rho\right|$ is greater than 1, then the model is explosive.

A stationary time series should revert to its mean, $\mu$, over time (Note that $\mu$ does not depend on t). This is demonstrated by differencing both sides of the AR(1) process to get $y_{t}-y_{t-1}=\Delta y_{t}=(\rho-1)y_{t-1}+\epsilon_{t}$. In this model, $\mu=0$ but any $\mu$ can be added into the equation with some algebraic manipulations without changing the dynamics. Notice that if $y_{t-1}>0$ (ie. the previous period's $y$ is greater than the mean) and since $\rho<1$, $(\rho-1)<0$ then $E_{t}[\Delta y_{t}]=(\rho-1)y_{t-1}+E_{t}[\epsilon_{t}]=(\rho-1)y_{t-1}+0<0$ (ie. $y$ will fall in the next period to bring the variable closer to the mean). Vice versa for $y_{t-1}<0$.

An example comparing stationary vs non-stationary time series is available [here](http://upload.wikimedia.org/wikipedia/en/e/e1/Stationarycomparison.png).

###Data Notes
* The bond yield rates are for a 3 year US treasury bond.
* The stock yield rates are for the S&P 500 index
* Expected real rates/yields are determined by subtracting previous year's inflation from nominal rates/yields
* Expected inflation is often difficult to model. Many macroeconomic models assume that people expect next year's inflation to be the same as last year's inflation, which is the standard used in this analysis.

###Testing Data for Stationarity

Sometimes, just looking a graph of time series can inform one of whether it is stationary or not.

```{r,echo=FALSE}
Graph.Data2 %>% 
  ggplot(aes(x=year,y=rate)) + geom_point(aes(col=type)) + geom_line(aes(col=type)) +
  scale_color_discrete(name="",breaks=c("Nom.Bond","Exp.Bond","Nom.Stock","Exp.Stock"),
                       labels=c("Nominal 3YR Rate","Exp. Real 3YR Rate","Nominal Real S&P 500 Yield","Exp. Real S&P 500 Yield")) +
  xlab("Time") + ylab("Rate of Return (%)") + ggtitle("3YR US Treasury Bond vs S&P 500 Yields")
```

It's difficult to tell whether the variables are stationary or not. A hypothesis test called the Augmented Dickey-Fuller test was created to test for stationarity. The null hypothesis is that the series is non-stationary. More details about the Augmented DF test can be found in sources.

A more complete and interactive graph can be found [here](https://youngde.shinyapps.io/Bond_Stock_Shiny_App/).

Expected Real S&P 500 Yield
```{r,echo=FALSE}
adf.test(Reg.Data2$stock.exp.rate)
```

Expected Real 3YR US Treasury Bond Yield
```{r,echo=FALSE}
adf.test(Reg.Data2$bond.exp.rate)
```

Nominal S&P 500 Yield
```{r,echo=FALSE}
adf.test(Reg.Data2$stock.nom.rate)
```

Nominal 3YR US Treasury Bond Yield
```{r,echo=FALSE}
adf.test(Reg.Data2$bond.nom.rate)
```

The only succesful null hypothesis rejection was for the expected real S&P 500 yield. Remember that this does not mean that these variables are non-stationary. There just was not enough evidence to reject that hypothesis. In fact, it is well accepted that stock yields and bond rates both nominal and real are stationary and that they return their means over time. 

Let's take a look at the movements between bond rates and stock yields, distinguishing between nominal and expected real stock returns.

```{r,echo=FALSE}
filter(Graph.Data2,type!="Exp.Stock") %>% 
  ggplot(aes(x=year,y=rate)) + geom_point(aes(col=type)) + geom_line(aes(col=type)) +
  scale_color_manual(values=c("blue", "red", "limegreen"),name="",breaks=c("Nom.Bond","Exp.Bond","Nom.Stock"),
                     labels=c("Nominal 3YR Rate","Exp. Real 3YR Rate","Nominal S&P 500 Yield")) +
  xlab("Time") + ylab("Rate of Return (%)") + ggtitle("3YR US Treasury Bond vs S&P 500 Yields")

filter(Graph.Data2,type!="Nom.Stock") %>% 
  ggplot(aes(x=year,y=rate)) + geom_point(aes(col=type)) + geom_line(aes(col=type)) +
  scale_color_manual(values=c("blue", "limegreen", "red"),name="",breaks=c("Nom.Bond","Exp.Bond","Exp.Stock"),
                     labels=c("Nominal 3YR Rate","Exp. Real 3YR Rate","Exp. Real S&P 500 Yield")) +
  xlab("Time") + ylab("Rate of Return (%)") + ggtitle("3YR US Treasury Bond vs S&P 500 Yields")
```

It appears that nominal stock yields correlate better with nominal bond rates and expected real stock yields correlate better with expected real bond rates. We will see later whether this result holds statistically.

An interesting anomaly to point out is that bonds seemed to give better returns than stocks in the 1980s and 1990s. The possible explanation I have for the 80s is that that period was a period of hyper-inflation where investors were deathly afraid of inflation. This fear drives up nominal rates, and also real rates if expectations of inflation are higher than realized inflation. My assumption that investors' expected inflation being last year's inflation is incorrect for this decade. In addition, there is a popular myth that stocks provide a good hedge against inflation. Investors may have bought into that myth during the hyper-inflation period and drove stock yields down by overbuying. The possible explanation I have for the 90s is the tech bubble. Investors were so irrationally excited about stocks that they were extremely overbought without any increases in yields. If stock prices are driven up really high without earnings increasing commesurrately, the yield plummates.

### Analysis

Given that we were unable to reject the null hypothesis that 3/4 of our variables were non-stationary, it's better to play it safe and difference all of our variables before doing a regression. You may wonder why econometricians and statisticians don't just always take differences to ensure stationarity. I've listed some pros and cons of doing so below.

* Pros
    + Taking a difference usually gives a stationary time series
    + Taking a difference usually gets rid of serial (auto) correlation issues
* Cons
    + Difficult to interpret ("A 1 unit change in the change of X is associated with a 1 unit change in the change of Y")
    + Makes predictions difficult (inputs are in changes not in actual values)
    + Loses one degree of freedom

The specification of our model is $y_{t} = \beta_{1} + \beta_{2}x_{2t} + \beta_{3}x_{3t} + \epsilon_{t}$
where y is stock yield difference, $x_{2}$ is expected real bond rate difference, and $x_{3}$ is nominal bond rate difference. Two regressions are run for both expected real and nominal stock yields as y. Note that only the three-year bond rates are used. Other bond types were not included to avoid multi-collinearity. Due to the spread basis of bond rates, it's likely that the different bond types are strongly correlated with each other so it would not be appropriate to include them all in the model.

A summary of results, confidence intervals, and Breusch-Godfrey tests for serial correlation (auto correlation for time series) are presented below. 

```{r,echo=FALSE}
model.e <- lm(data=Reg.Data.Dif2,stock.e.dif~bond.e.dif+bond.n.dif)
model.n <- lm(data=Reg.Data.Dif2,stock.n.dif~bond.e.dif+bond.n.dif)
summary(model.e)
bgtest(model.e)
confint(model.e)
summary(model.n)
bgtest(model.n)
confint(model.n)
```

Both models provided some expected results, that expected real (nominal) changes in bond rates are positively correlated with expected real (nominal) changes in stock yields. 

The 1st model (y is expected real stock yield) does have an odd result in that the slope coefficient for the effect that nominal bond rate changes brings is strongly negative. This result is reflected in the confidence interval as well. I think that this is a case of omitted variable bias. A positive increase in the change in nominal bond rates likely proxy for fear of increased inflation. Often, the Fed will hike nominal interest rates to combat rising inflation or expected rising inflation (hawkish policy). As stated before, a large increase in the fear of rising inflation may cause investors to flood to stocks which drive down stock yields. This would likely explain the negative coefficient for nominal bond rate changes.

For the 2nd model (y is nominal stock yield), both coefficients are positive. Intuitively, since the stock yields are in nominal terms, nominal bond rates should have a larger effect than expected real bond rates. The point estimate of the slopes reflect this, but both slopes have standard errors. A hypothesis test is in order.

The null hypothesis we are testing is $b_{3}-b_{2}=0$. The standard error for this test is $\sqrt{var(b_{3})+var(b_{2})-2cov(b_{3},b_{2})}$. The terms inside of the square root can be extracted from the variance-covariance matrix via the vcov() command. The null distribution of this test is a t-distribution with `r df.residual(model.n)` degrees of freedom.

p-value of the test statistic
```{r,echo=FALSE}
n.coef <- coef(model.n)
n.vcov <- vcov(model.n)
n.t.test <- (n.coef[3]-n.coef[2])/sqrt((n.vcov[2,2]+n.vcov[3,3]-2*n.vcov[2,3]))
n.p.value <- 1-pt(n.t.test,df=df.residual(model.n))
n.p.value
```

95% confidence interval of $b_{3}-b_{2}$.
```{r,echo=FALSE}
error <- qt(.975,df=df.residual(model.n))*sqrt((n.vcov[2,2]+n.vcov[3,3]-2*n.vcov[2,3]))
left <- n.coef[3]-n.coef[2]-error
right <- n.coef[3]-n.coef[2]+error
n.confint <- cbind(left,right)
colnames(n.confint) <- c("2.5%","97.5%")
rownames(n.confint) <- "b3-b2"
n.confint
```
We are unable to reject the null hypothesis that $b_{3}$ and $b_{2}$ are the same. However, the p-value and confidence interval suggests that we are approaching significance. 

Recall earlier that differencing variables often removes serial correlation issues. The Breusch-Godfrey tests on the models with differenced variables could not reject the null that there is no serial correlation. Let's do a B-G test on the undifferenced verision of our model to see if there were any serial correlation issues to begin with.

Breusch-Godfrey test on non-differenced data:
```{r,echo=FALSE}
test <- lm(data=Reg.Data2, stock.exp.rate~bond.exp.rate+bond.nom.rate)
bgtest(test)
```

The B-G test rejects the null that there is no serial correlation in the error terms. Looks like differencing our variables indeed gave us the positive side effect of removing serial correlation.

###Concluding Thoughts

Given the historical evidence for a rise in interest rates to result in the rise in yields for stocks, one would expect that this holds again if the Fed decides to raise the rock-bottom interest rates soon. Generally, since S&P 500 average earnings do not shift quickly, it is the average price of the S&P 500 that must fall to raise yields. Even Janet Yellen herself has commented recently that "equity market valuations are generally quite high". We may be in for some bear markets when the interest rate increases.

> Sources:  
> http://useconomy.about.com/od/glossary/g/Inverted_yield.htm (reasons for inverted yield curve)  
> http://academic.reed.edu/economics/parker/312/tschapters/S13_Ch_1.pdf (Stationary vs. Non-Stationary time series)  
> http://academic.reed.edu/economics/parker/312/tschapters/S13_Ch_2.pdf (Breusch-Godfrey test)  
> http://academic.reed.edu/economics/parker/312/tschapters/S13_Ch_4.pdf (Dickey-Fuller)  
> http://seekingalpha.com/instablog/604052-element-alpha/65463-proof-that-the-modern-portfolio-theory-should-be-declared-dead (Average stock holding period)  
> _The Intelligent Investor_ by Benjamin Graham (false belief that stocks are a good hedge for inflation)  
> _Advanced Macroeconomics - 4th Edition_ by David Romer (modeling expected inflation)  
> http://www.reuters.com/article/2015/05/06/us-usa-fed-yellen-idUSKBN0NR1JI20150506 (Janet Yellen on valuations) 