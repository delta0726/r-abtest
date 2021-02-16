# ***********************************************************************************************
# Function  : BinUplift
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/17
# URL       : https://www.rdocumentation.org/packages/tools4uplift/versions/1.0.0/topics/BinUplift
# ***********************************************************************************************


# ＜構文＞
# - QiniベースのLHS(Latin Hypercube Sampling)のUpliftモデル


# ＜構文＞
# qLHS(data, treat, outcome, predictors,
#     lhs_points = 50, lhs_range = 1,
#     adjusted = TRUE, rank.precision = 2, equal.intervals = FALSE,
#     nb.group = 10, validation = TRUE, p = 0.3)


# ＜目次＞
# 0 準備
# 1 関数実行


# 0 準備 -------------------------------------------------------------------------

library(tidyverse)
library(tools4uplift)


# データ準備
data("SimUplift")

# データ確認
SimUplift %>% as_tibble()
SimUplift %>% glimpse()


# 1 関数実行 ------------------------------------------------------------------------

upliftLHS <-
  SimUplift %>%
    qLHS(treat = "treat",
         outcome = "y",
         predictors = colnames(SimUplift[,3:7]),
         lhs_points = 5,
         lhs_range = 1,
         adjusted = TRUE,
         equal.intervals = TRUE,
         nb.group = 5,
         validation = FALSE)