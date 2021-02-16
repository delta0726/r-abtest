# ***********************************************************************************************
# Function  : PerformanceUplift
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/17
# URL       : https://www.rdocumentation.org/packages/tools4uplift/versions/1.0.0/topics/PerformanceUplift
# ***********************************************************************************************


# ＜構文＞



# ＜構文＞
# PerformanceUplift(data, treat, outcome, prediction, nb.group = 10,
#                  equal.intervals = TRUE, rank.precision = 2)


# ＜目次＞
# 0 準備
# 1 モデリング
# 2 モデル評価


# 0 準備 -------------------------------------------------------------------------

library(tidyverse)
library(tools4uplift)


# データ準備
data("SimUplift")

# データ確認
SimUplift %>% as_tibble()
SimUplift %>% glimpse()

# データ確認
# --- 介入群と統制群は1/0のフラグで区別する
# --- 1: treatment（介入群）
# --- 0: control（統制群）
SimUplift$treat %>% table()


# 1 モデリング --------------------------------------------------------------------

model1 <-
  SimUplift %>%
    BinUplift2d( "X1", "X2", "treat", "y")


# 2 モデル評価 --------------------------------------------------------------------

perf1 <-
  model1 %>%
    PerformanceUplift(treat = "treat", outcome = "y", prediction = "Uplift_X1_X2",
                      equal.intervals = TRUE, nb.group = 3)

# 確認
perf1 %>% print()
