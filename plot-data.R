library(ggplot2)

data <- read.csv("data/npb_hitter_19_allinfo.csv")
head(data)
allsal <- data$年俸.万円.
hist(allsal)
hist(log(allsal))

g <- ggplot(
  data = data,
  aes(x = log(安打),
      y = log(年俸.万円.),
      color = チーム)
) 
g <- g + geom_point()
g

g <- ggplot(
  data = data,
  aes(x = OPS,
      y = log(年俸.万円.),
      color = チーム)
) 
g <- g + geom_point()
g


ggplot(
  data = data,
  aes(x = チーム,
      y = log(年俸.万円.),
      color = チーム)
) + 
  geom_violin(trim=FALSE) + 
  geom_dotplot(binaxis="y",
               stackdir = "center",
               binwidth = 0.1)
data$チーム

# 対数正規分布でフィット
library(rstan)
library(bayesplot)
library(ggfortify)
library(dplyr)
library(brms)
rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())

salary <- data$年俸.万円.
sample_size <- length(salary)
data_list <- list(
  salary = salary,
  N = sample_size
)
mcmc_result <- stan(
  file = "fit_salary-dist.stan",
  data = data_list,
  seed = 1
)

print(
  mcmc_result,
  probs = c(0.025, 0.5, 0.975)
)

mcmc_sample <- rstan::extract(mcmc_result, permuted=FALSE)
dimnames(mcmc_sample)
traceplot(mcmc_result)

mcmc_hist(mcmc_sample, pars = c("mu", "sigma"))
mcmc_combo(mcmc_sample, pars = c("mu", "sigma"))
mcmc_dens(mcmc_sample, regex_pars=c("salary_pred."))

mu_eap <- mean(mcmc_sample[,,"mu"])
sigma_eap <- mean(mcmc_sample[,,"sigma"])

hist(salary, freq=FALSE, breaks=seq(0,60000,1000))
curve(dlnorm(x, mu_eap, sigma_eap), from=0, to=60000, add=TRUE)

# OPSで説明してみる
head(data[order(data$OPS, decreasing=T), ])

# 試合数が少ないやつはいらん
hist(data$試合, breaks=seq(0, 120, 5))

matches <- max(data$試合, na.rm=TRUE)

data_sel <- filter(data, 打席数>=10)

g <- ggplot(
  data = data_sel,
  aes(x = log(安打),
      y = log(年俸.万円.),
      color = チーム)
) 
g <- g + geom_point()
g

g <- ggplot(
  data = data_sel,
  aes(x = OPS,
      y = log(年俸.万円.),
      color = チーム)
) 
g <- g + geom_point()
g

# OPSと安打数で年俸を予測 (ランダム効果: チーム)

teams = data.frame(
  チーム = c("ソフトバンク",
          "ロッテ",
          "西武",
          "楽天",
          "日本ハム",
          "オリックス",
          "巨人",
          "阪神",
          "中日",
          "DeNA",
          "広島",
          "ヤクルト")
)

glmm_ops_lnorm <- brm(
  formula = 年俸.万円. ~ OPS + (OPS|チーム), 
  family = gaussian(link='log'),
  data = data_sel,
  seed = 1,
  iter = 10000,
  warmup = 9000,
  control = list(adapt_delta = 0.97, max_treedepth=15)
)

print(glmm_ops_lnorm,
      probs = c(0.025, 0.5, 0.975))

eff_ops_lnorm <- conditional_effects(glmm_ops_lnorm,
                                     effects="OPS",
                                     re_formula=NULL,
                                     conditions=teams)
plot(eff_ops_lnorm, points=TRUE)

glmm_ops_gamma_identity <- brm(
  formula = 年俸.万円. ~ OPS + (OPS|チーム), 
  family = Gamma(link='identity'),
  data = data_sel,
  seed = 1,
  iter = 6000,
  warmup = 5000
)

glmm_ops_gamma_log <- brm(
  formula = 年俸.万円. ~ OPS + (OPS|チーム), 
  family = Gamma(link='log'),
  data = data_sel,
  seed = 1,
  iter = 6000,
  warmup = 5000
)


