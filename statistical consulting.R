library(dplyr)
library(ggplot2)
library(ggord)
library(psych)
library(GPArotation)
library(ggdendro)
library(gridExtra)
library(grid)
library(reshape2)
library(nlme)
library(lmerTest)
library(scales)

SPBD <- read.csv("SPBD.csv") %>%
        dplyr::select(-starts_with("x")) %>%
        mutate_each(funs(as.character(.)), -(School.Name:Finish.Date))
questions <- names(SPBD)
questions <- questions[-(1:3)]
names(SPBD)[4:ncol(SPBD)] <- paste("col_", letters[1:(ncol(SPBD)-3)], sep="")

# Normal case
SPBD[SPBD == "I feel that I do not know my colleagues well enough to answer this question."] <- 2.5
SPBD[SPBD == "I don't know."] <- 2.5
SPBD[SPBD == "Currently, my school does not have a common set of student expectations."] <- 2.5
SPBD[SPBD == "Currently, my school does not have a common set of consequences."] <- 2.5
SPBD[SPBD == "999"] <- NA
SPBD <- SPBD %>%
        mutate_each(funs(as.numeric(.)), -c(School.Name, ID, Finish.Date))

X <- SPBD %>% dplyr::select(-(School.Name:Finish.Date))
X.mean <- apply(X, 2, function(d) {mean(d, na.rm = T)})
X.sd <- apply(X, 2, function(d) {sd(d, na.rm = T)})

# 1. Descriptive Analysis
## Boxplot of 23 core survey items
na.num <- NULL
for(i in 1:ncol(X)) {
  na.num <- c(na.num, sum(is.na(X[,i])))
}
na.rate <- na.num / nrow(X)
X.NAomit <- na.omit(X)

id <- 1:nrow(X.NAomit)
survey <- cbind(id, X.NAomit)
long.survey <- melt(survey, id = "id", variable.name = "item", 
                    value.name = "score")
item.means <- aggregate(score ~ item, long.survey, mean)
item.means$score <- round(item.means$score, 2)

box <- ggplot(long.survey, aes(x = item, y = score)) +
  geom_boxplot() + 
  stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
               shape = 18, size = 2.3, show.legend = F) + 
  geom_text(data = item.means, aes(label = score, 
                                   y = score + c(0.12,0.16,rep(0.12,8),0.11,0.12,0.12,0.16,
                                                 rep(0.12,9))), size = 3.5) +
  geom_hline(yintercept = 2.5, linetype = 3) +
  scale_x_discrete(labels = as.character(1:23))

## Heatmap of correlations between core survey items
ggCorHeatmap <- function(cormx) {
  x <- cormx
  dd.col <- as.dendrogram(hclust(dist(x)))
  col.ord <- order.dendrogram(dd.col)
  
  dd.row <- dd.col
  row.ord <- col.ord
  
  xx <- survey.cor[col.ord,row.ord]
  xx_names <- attr(xx, "dimnames")
  df <- as.data.frame(xx)
  colnames(df) <- xx_names[[2]]
  df$item <- xx_names[[1]]
  df$item <- with(df, factor(item, levels = item, ordered = T))
  
  mdf <- melt(df, id.vars = "item")
  
  ddata_x <- dendro_data(dd.row)
  ddata_y <- dendro_data(dd.col)
  
  ### Set up a blank theme
  theme_none <- theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_text(colour = NA),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_blank()
    # axis.ticks.length = element_blank()
  )
  
  ### Create plot components ###    
  # Heatmap
  p1 <- ggplot(mdf, aes(x = variable, y = item)) + 
    geom_tile(aes(fill = value)) + 
    scale_fill_gradient2(high = "#54278f", low = "#f2f0f7") +
    xlab("Items") + ylab("Items") 
  
  # Dendrogram 1
  p2 <- ggplot(segment(ddata_x)) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
    theme_none + theme(axis.title.x = element_blank())
  
  # Dendrogram 2
  p3 <- ggplot(segment(ddata_y)) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
    coord_flip() + theme_none
  
  ### Draw graphic ###
  grid.newpage()
  print(p1, vp = viewport(0.8, 0.8, x = 0.4, y = 0.4))
  print(p2, vp = viewport(0.72, 0.2, x = 0.38, y = 0.9))
  print(p3, vp = viewport(0.2, 0.8, x = 0.9, y = 0.4))
}

colnames(X.NAomit) <- 1:23
survey.cor <- cor(X.NAomit)
ggCorHeatmap(survey.cor)

## School-level barplot
p1 <- ggplot(data = state.data, aes(x = state, y = count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
      theme_minimal()
p1 <- p1 + theme(legend.position = "none")

p2 <- ggplot(data = school.data, aes(x = level, y = count)) +
      geom_bar(stat = "identity", fill = "#9999CC") +
      geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
      theme_minimal() + xlab("school level")
p2 <- p2 + theme(legend.position = "none")


p3 <- ggplot(data = imple.level, aes(x = level, y = count)) +
      geom_bar(stat = "identity", fill = "#CC6666") +
      geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
      theme_minimal() + xlab("implementation level of SWPBS")
p3 <- p3 + theme(legend.position = "none")


p4 <- ggplot(data = imple.year, aes(x = year, y = count)) +
      geom_bar(stat = "identity", fill = "#c994c7") +
      geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
      theme_minimal() + xlab("implementation year of SWPBS")
p4 <- p4 + theme(legend.position = "none")

grid.arrange(p1, p2, p3, p4, ncol = 2)

## Individual-level barplot
p5 <- ggplot(data = role, aes(x = role)) + 
      geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue", width = 0.5) + 
      scale_y_continuous(labels = scales::percent) + theme_minimal() +
      xlab("staff's role") + ylab("Percent") + theme(legend.position = "none") +
      geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= (..count..)/sum(..count..)), stat= "count", vjust = -.5, size = 3.5) 

p6 <- ggplot(data = year, aes(x = level)) + 
      geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#9999CC", width = 0.6) + 
      scale_y_continuous(labels = scales::percent) + theme_minimal() +
      xlab("years of experience") + ylab("Percent") + theme(legend.position = "none") + 
      geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= (..count..)/sum(..count..)), stat = "count", vjust = -.5, size = 3.5)

p7 <- ggplot(data = understand, aes(x = level)) + 
      geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#CC6666", width = 0.5) + 
      scale_y_continuous(labels=scales::percent) + theme_minimal() +
      xlab("level of understanding of SWPBS") + ylab("Percent") + theme(legend.position = "none") +
      geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= (..count..)/sum(..count..)), stat = "count", vjust = -.5, size = 3.5) 
grid.arrange(p5, p6, p7, ncol = 2)


# 2. Principal Component Analysis and Varimax Rotation
## Scree plot
d <- data.frame(x = 1:length(fit.varimax$values), y = fit.varimax$values)
ggplot(d, aes(x = x, y = y)) + xlab("Factors") + ylab("Eigenvalues") +
  geom_point() + geom_line() + 
  geom_hline(yintercept = 1, linetype = 3)

## PCA and varimax rotation
pca.mod <- prcomp(~., center = T, scale = T, na.action = na.omit, data=X)
n.comp <- 5
raw.loading <- pca.mod$rotation[,1:n.comp]  %*% diag(pca.mod$sdev, n.comp, n.comp)
varimax.result <- varimax(raw.loading)
varimax.result$loadings
a <- varimax.result$loadings

fit.varimax <- principal(X, nfactors = 5, rotate = "varimax")
fit.oblimin <- principal(X, nfactors = 5, rotate = "oblimin")
fit.promax <- principal(X, nfactors = 5, rotate = "promax")
fit.cluster <- principal(X, nfactors = 5, rotate = "cluster")

loading.2.matrix <- function(loading, nfactors) {
  return(matrix(loading, ncol = nfactors))
}

varimax.loading <- fit.varimax$loadings

## Cronbach's alpha coefficient and KMO index
cronbach <- psych::alpha(X)
c.alpha <- cronbach$total[1]

kmo <- KMO(X)$MSA

## Five factors
factor.1 <- c(X[,4], X[,12], X[,13], X[,14], X[,17], X[,18], X[,19])
factor.2 <- c(X[,1], X[,3], X[,5], X[,8])
factor.3 <- c(X[,9], X[,15], X[,16], X[,19], X[,20])
factor.4 <- c(X[,6], X[,7], X[,10], X[,11])
factor.5 <- c(X[,21], X[,22], X[,23])
all.values <- c(factor.1, factor.2, factor.3, factor.4, factor.5)

## Boxplot for each factor
dd <- data.frame(factor = c(rep(1, length(factor.1)), rep(2, length(factor.2)),
                          rep(3, length(factor.3)), rep(4, length(factor.4)),
                          rep(5, length(factor.5))),
                 value = c(factor.1, factor.2, factor.3, factor.4, factor.5))
dd <- na.omit(dd)
dd$factor <- as.factor(dd$factor)
overall.mean <- mean(dd$value, na.rm = T)
factor.means <- aggregate(value ~ factor, dd, mean)
factor.means$value <- round(factor.means$value, 2)
ggplot(dd, aes(x = factor, y = value, fill = factor)) +
       geom_boxplot() +
       stat_summary(fun.y = mean, colour = "darkred", geom = "point", 
                    shape = 18, size = 3, show.legend = F) + 
       geom_text(data = factor.means, aes(label = value, y = value + 0.15)) +
       geom_hline(yintercept = 2.5, linetype = 3) +
       geom_hline(yintercept = overall.mean, linetype = 2, colour = "red")


# 3. Hierarchical Linear Model (HLM)
X.hier <- SPBD %>% dplyr::select(-c(Finish.Date))

count <- 0
for(i in 1:dim(X)[1]) {
  if (sum(is.na(X[i,])) > 0){
    count <- count + 1
  }
}

dat_organ <- read.csv("dat_organ.csv")
tab <- merge(dat_organ, X.hier, by = "ID")
tab_temp <- NULL
for (i in 1:dim(tab)[1]) {
  if (sum(is.na(tab[i,])) == 0) {
    tab_temp <- rbind(tab_temp, tab[i,])
  }
}

colnames(tab_temp)[2:7] <- c('SL','IL','IY','LOU','SOC','DEV')
SPS <- apply(tab_temp[11:33], 1, sum)
attach(tab_temp)

## HLM
l <- lme(fixed = SPS ~ 1 + SL + IL + Commun + LOU + SOC, method = "REML",
         random = reStruct(~1 + Commun + LOU + SOC|School.Name,
                           pdClass = "pdDiag"))
coefs <- round(coef(summary(l)), 2)
ps <- round(coef(summary(l))[,5], 3)

## Diagnosis
### Residuals versus fitted value of the HLM
dat <- data.frame(fitted = l$fitted[,2], res = l$residuals[,2])

ggplot(dat, aes(x = fitted, y = res)) + 
       geom_point(size = 0.7) + geom_hline(yintercept = 0) +
       labs(x = "Fitted Values", y = "Residuals")

### Norm QQ-plot of residuals
y <- quantile(dat$res, probs = c(0.25,0.75))
x <- qnorm(c(0.25,0.75))
sl <- 1.2*diff(y) / diff(x)
int <- y[1] - sl*x[1]

ggplot(dat, aes(sample = res)) +
       stat_qq() + geom_abline(slope = sl,intercept = int)

### Norm QQ-plot of each second level random effect
a1 <- a3 <- 3.5
a2 <- a4 <- 0.05

ran <- data.frame(u01 = l$coefficients$random$School.Name[,1],
                  u02 = l$coefficients$random$School.Name[,2],
                  u03 = l$coefficients$random$School.Name[,3],
                  u04 = l$coefficients$random$School.Name[,4])

y <- quantile(ran$u01, probs = c(0.25,0.75))
x <- qnorm(c(0.25,0.75))
sl <- 1.2*diff(y) / diff(x)
int <- y[1] - sl*x[1]

p1 <- ggplot(ran, aes(sample = u01)) + 
      ggtitle(expression(paste(hat(U)['0j']))) + 
      stat_qq() + geom_abline(slope=sl,intercept = int) + 
      theme(text = element_text(size=6.5),
            plot.title = element_text(hjust = 0.5))

y <- quantile(ran$u02, probs = c(0.25,0.75))
x <- qnorm(c(0.25,0.75))
sl <- diff(y) / diff(x)
int <- y[1] - sl*x[1]

p2 <- ggplot(ran,aes(sample = u02)) +
      ggtitle(expression(paste(hat(U)['1j']))) + 
      stat_qq() + geom_abline(slope = sl,intercept = int) + 
      theme(text = element_text(size = 6.5),
            plot.title = element_text(hjust = 0.5))

y <- quantile(ran$u03, probs = c(0.25,0.75))
x <- qnorm(c(0.25, 0.75))
sl <- diff(y) / diff(x)
int <- y[1] - sl*x[1]
p3 <- ggplot(ran,aes(sample = u03)) + 
      ggtitle(expression(paste(hat(U)['2j']))) + 
      stat_qq() + geom_abline(slope = sl,intercept = int) + 
      theme(text = element_text(size = 6.5),
            plot.title = element_text(hjust = 0.5))

y <- quantile(ran$u04, probs = c(0.25,0.75))
x <- qnorm(c(0.25,0.75))
sl <- diff(y) / diff(x)
int <- y[1] - sl*x[1]
p4 <- ggplot(ran,aes(sample = u04)) + 
      ggtitle(expression(paste(hat(U)['3j']))) + 
      stat_qq() + geom_abline(slope = sl,intercept = int) + 
      theme(text = element_text(size = 6.5),
        plot.title = element_text(hjust = 0.5))

grid.arrange(p1,p2,p3,p4, ncol = 4)