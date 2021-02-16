# ***********************************************************************************************
# Function  : SplitUplift
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/17
# URL       : https://www.rdocumentation.org/packages/tools4uplift/versions/1.0.0/topics/SplitUplift
# ***********************************************************************************************


# ＜概要＞
# - データセットを訓練データと検証データに分割する


# ＜構文＞
# SplitUplift(data, p, group)


# ＜目次＞
# 0 準備
# 1 分布の作成
# 2 基本フロー


# 0 準備 -------------------------------------------------------------------------

library(tidyverse)
library(tools4uplift)
library(Hmisc)


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


# 1 データ分割 -----------------------------------------------------------------------

# データ分割
# --- 介入群と統制群が均等になるように分割
split <- SimUplift %>% SplitUplift(0.8, c("treat", "y"))

# データ構造
split %>% glimpse()

# データ格納
train <- split[[1]]
valid <- split[[2]]
