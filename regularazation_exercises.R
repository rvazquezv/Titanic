options(digits=7)
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))


set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))


scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))


####Q1

schools %>% top_n(10, score) %>% arrange(desc(score))

t10_schools<-schools %>% top_n(10, score) %>% arrange(desc(score))

t10_schools$score[10]


####Q2

median(schools$size)
median(t10_schools$size)



####Q3

schools %>% top_n(-10, score) %>% arrange(score) %>% .$size %>% median()

####Q4

schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)



####Q5

overall <- mean(sapply(scores, mean))
alfa<-25


schools <- schools %>% mutate(new_score = overall+(size*(score-overall)/(alfa+size)))


schools %>% top_n(10, new_score) %>% arrange(desc(new_score))



####Q6

alfas <- seq(10, 250)

RMSE<-function(x,y){sqrt(sum((x-y)^2)/length(x))}

rmses <- sapply(alfas, function(a){
  predicted_schools <- schools %>% 
  mutate(new_score = overall+(size*(score-overall)/(a+size)))
  return(RMSE(predicted_schools$quality,predicted_schools$new_score))
})
qplot(alfas, rmses)  
alfas[which.min(rmses)]

####Q7


overall <- mean(sapply(scores, mean))
alfa<-alfas[which.min(rmses)]


schools <- schools %>% mutate(new_score = overall+(size*(score-overall)/(alfa+size)))


schools %>% top_n(10, new_score) %>% arrange(desc(new_score))



####Q8




alfas <- seq(10, 250)

RMSE<-function(x,y){sqrt(sum((x-y)^2)/length(x))}

rmses <- sapply(alfas, function(a){
  predicted_schools <- schools %>% 
    mutate(new_score = ((size*score)/(a+size)))
  return(RMSE(predicted_schools$quality,predicted_schools$new_score))
})
qplot(alfas, rmses)  
alfas[which.min(rmses)]




alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]