# ***********************************************************************************************
# Function  : BestFeatures
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/17
# URL       : https://www.rdocumentation.org/packages/tools4uplift/versions/1.0.0/topics/BestFeatures
# ***********************************************************************************************


# ＜概要＞
# - Qini係数ベースの特徴選択を行う
# - 正則化パラメーターは、Qini係数を最大化するUpliftモデルに基づいて選択される
#   --- LASSOペナルティを使用すると一部の特徴量の係数はゼロに設定される


# ＜構文＞
# BestFeatures(data, treat, outcome, predictors, rank.precision = 2,
#                            equal.intervals = FALSE, nb.group = 10,
#                            validation = TRUE, p = 0.3)


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

# データ確認
# --- 介入群と統制群は1/0のフラグで区別する
# --- 1: treatment（介入群）
# --- 0: control（統制群）
SimUplift$treat %>% table()


# 1 関数実行 ---------------------------------------------------------------------------

#
features <-
  SimUplift %>%
    BestFeatures(treat = "treat",
                 outcome = "y",
                 predictors = colnames(SimUplift[,3:7]),
                 equal.intervals = TRUE,
                 nb.group = 5,
                 validation = FALSE)

# 確認
features %>% print()
features %>% glimpse()