# ***********************************************************************************************
# Function  : predict
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/17
# URL       : https://www.rdocumentation.org/packages/tools4uplift/versions/1.0.0/topics/predict.DualUplift
# ***********************************************************************************************


# ＜概要＞



# ＜構文＞
# predict(object, newdata, ...)


# ＜目次＞
# 0 準備
# 1 学習
# 2 予測


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


# 1 学習 --------------------------------------------------------------------------

# モデル1
# --- アップリフトモデル
fit_1 <-
  SimUplift %>%
    DualUplift("treat", "y", predictors = colnames(SimUplift[, 3:12]))

# モデル2
# --- アップリフトモデル（区間推定）
fit_2 <-
SimUplift %>%
  BinUplift(treat = "treat", outcome = "y", x = "X1",
            n.split = 100, alpha = 0.01, n.min = 30)

# モデル3
# --- アップリフトモデル（相互効果あり）
fit_3 <-
  SimUplift %>%
    InterUplift( "treat", "y", colnames(SimUplift[, 3:12]))


# 2 予測 -----------------------------------------------------------------------

# 予測
fit_1 %>% predict(SimUplift) %>% as_tibble()
fit_2 %>% predict(SimUplift$X1) %>% as_tibble()
fit_3 %>% predict(SimUplift, "treat") %>% as_tibble()
