# ***********************************************************************************************
# Function  : UpliftPerCat
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/17
# URL       : https://www.rdocumentation.org/packages/tools4uplift/versions/1.0.0/topics/UpliftPerCat
# ***********************************************************************************************


# ＜構文＞
# - カテゴリごとに観測されたUpliftを計算し、バープロットを作成します。


# ＜構文＞
# UpliftPerCat(data, treat, outcome, x, ...)


# ＜目次＞
# 0 準備
# 1 モデリング
# 2 プロット


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


# 1 モデリング ------------------------------------------------------------------------

# 学習
binX1 <-
  SimUplift %>%
    BinUplift(treat = "treat", outcome = "y", x = "X1",
              n.split = 100, alpha = 0.01, n.min = 30)

# 予測
SimUplift$quantizedX1 <-
  binX1 %>%
    predict(SimUplift$X1)

# 2 プロット ------------------------------------------------------------------------

SimUplift %>%
  UpliftPerCat(treat = "treat", outcome = "y", x = "quantizedX1",
               xlab='Quantized X1', ylab='Uplift', ylim=c(-1,1))
