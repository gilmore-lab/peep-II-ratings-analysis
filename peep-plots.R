fam.only <- rnorm(10)
nov.only <- rnorm(10)
both <- rnorm(10)
cond <- factor(rep(c("fam", "nov", "fam", "nov"), each=10))
id.only <- c(1:20)
id.both <- rep(c(21:30), 2)
f0.df <- data.frame(id = c(id.only, id.both), f0 = c(fam.only, nov.only, both, both), cond = cond)

library(magrittr)
library(ggplot2)
f0.df %>%
  ggplot(.) +
  aes(., x = cond, y = f0) +
  geom_violin() +
  geom_point() +
  geom_line(aes(group = id))

f0.df %>%
  ggplot(.) +
  aes(., x = cond, y = f0) +

f0.ang <- rnorm(40)
f0.neu <- f0.ang - 0.2
f0.hap <- f0.ang - 0.3
f0.sad <- f0.ang - 0.5
id <- c(1:40)
cond.fam <- rep("fam", 40)
cond.nov <- rep("nov", 40)
emo <- rep(c("ang", "neu", "hap", "sad"), each = 80)

f0.df <- data.frame(id = rep(id, 8), cond = rep(c(cond.fam, cond.nov), each = 4),
                   f0 = c(f0, sample(f0, size = 40, replace = FALSE)))

f0.df %>%
  ggplot(.) +
  aes(., x = cond, y = f0) +
  geom_violin() +
  geom_point() +
  geom_line(aes(group = id))



