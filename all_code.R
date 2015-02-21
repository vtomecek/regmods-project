library(datasets); data(mtcars); require(stats); require(graphics);

## sampling
smp_size <- floor(0.67 * nrow(mtcars))	# 67% of the sample size
set.seed(666)	# set the seed for reproductibility
train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)
train <- mtcars[train_ind, ]
test <- mtcars[-train_ind, ]

## stepwise regression
null <- lm(mpg ~ 1, data = train);
full <- lm(mpg ~ ., data = train);
forward <- step(null, scope=list(lower=null, upper=full), direction="forward");

## correlations
cor(predict(forward, test), test$mpg)
cor(predict(forward, train), train$mpg)

## best model without am
fit <- lm(mpg ~ cyl+wt+hp+carb, data = train);
cor(predict(fit, test), test$mpg);



## figure1
transmission <- factor(mtcars$am, levels=0:1, labels=c("automatic", "manual"))
plot(transmission, mtcars$mpg, main="Fuel consumption by transmission", xlab="transmission", ylab="mpg")

## figure2
pairs(mtcars, panel=panel.smooth, main="mtcars data", col=3+mtcars$am)

## figure3+4+5
par(mfrow = c(2, 2))
plot(fit)
summary(fit)
anova(fit)

best model (cyl+wt+hp+carb) on the training set



## confidence intervals for am impact (we don't need it)
# 65% of am variance can be explained by wt+cyl+hp+carb model
# so we will purify mpg first from wt+cyl+hp+carb influences
# and then fit purified mpg(diff) with am
fit <- lm(mpg ~ wt+cyl+hp+carb, data = mtcars)
mtcars[,'diff'] <- mtcars$mpg - predict(fit,mtcars)
fit <- lm(diff ~ am, data = mtcars)
confint(fit)
