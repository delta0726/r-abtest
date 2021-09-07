#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 2章 介入効果を測るための回帰分析
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/8
# Page      : P40 - P66
#***************************************************************************************


# ＜概要＞
# - 回帰分析は最も基本的なセレクションバイアスの削減方法で、他の手法は回帰分析を応用したものといえる
# - 回帰分析でバイアス要因を調整したうえで、介入有無の効果を測定することが可能


# ＜ポイント＞
# - 回帰分析に以下の３つの変数を導入する
#   被説明変数(Y) ：介入による効果を確認したい変数(購買量)
#   介入変数(Z)   ：介入有無を示すダミー変数（介入の有無）
#   共変量(X)     ：セレクションバイアスを発生させると分析者が想定する変数（バイアス要因）


# ＜目次＞
# 0 準備
# 1 バイアスありデータの作成
# 2 回帰分析の実行
# 3 回帰分析におけるバイアス
# 4 脱落変数バイアス(OVB)
# 5 OVBをmap()を使ってアプローチ
# 6 モデルに不要な変数を入れる弊害


# 0 準備 -------------------------------------------------------------------------

# ＜ポイント＞
# - ECサイトのユーザーに対してRCTを適用したメールマーケティングを行ったデータを使用（第1章と同様）

# ＜該当ページ＞
# - P25 - 26


# ライブラリ
library(tidyverse)
library(broom)

# データ準備
# --- ECサイトのユーザーに対してRCTを適用したメールマーケティングを行ったデータ
email_data <- read_csv("book/causal_inference/csv/E-MailAnalytics.csv")

# データの準備
# --- 女性向けメールが配信されたデータを削除したデータを作成（問題の単純化するため）
# --- treatment: 男性でEmailを送った人
male_df <-
  email_data %>%
    filter(segment != "Womens E-Mail") %>%
    mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0))

# 確認
male_df$segment %>% table()


# 1 バイアスありデータの作成 ----------------------------------------------------------

# ＜該当ページ＞
# - P30 - 31

# seedを固定する
set.seed(1)

# 閾値の設定
# --- 条件に反応するサンプルの量を半分にする
obs_rate_c <- 0.5
obs_rate_t <- 0.5

# バイアスのあるデータの作成
biased_data <-
  male_df %>%
    mutate(obs_rate_c = ifelse((history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1),
           obs_rate_t = ifelse((history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t),
           random_number = runif(n = NROW(male_df))) %>%
    filter((treatment == 0 & random_number < obs_rate_c) |
             (treatment == 1 & random_number < obs_rate_t))


# 2 回帰分析の実行 ---------------------------------------------------------------------

# ＜ポイント＞
# - 介入変数をダミー変数にして回帰モデルを解くことで効果を測定する
# - 因果推論における回帰では介入変数のみに着目（共変量は解釈する必要はない）

# ＜変数の説明＞
# - 目的変数(Y)： spend:(購入額)
# - 介入変数(Z)： treatment: 1(配信した) / 0(配信しなかった)
# - 共変量  (X)： history: 昨年の購入額

# ＜該当ページ＞
# - P46 - 48


# データ確認
# --- 該当データを抽出
biased_data %>% select(spend, treatment, history)

# 回帰分析の実行
# --- ｢バイアスのあるデータ｣を用いて実施
biased_reg <- lm(spend ~ treatment + history, data = biased_data)

# サマリー
# --- 回帰分析の伝統的な出力（参考）
biased_reg %>% summary()

# 回帰係数のみ確認
# --- 介入効果の推定することが目的なので、treatmentの回帰係数に興味がある
# --- treatmentは0.9026でp値も小さいので帰無仮説を棄却することができる
# --- メールを送信することで売上が平均0.9程度増加することを意味する
biased_reg_coef <- biased_reg %>% tidy()
biased_reg_coef %>% print()


# 3 回帰分析におけるバイアス --------------------------------------------------------------

# ＜ポイント＞
# - 回帰分析でセレクションバイアスを排除するには共変量を正しく選択する必要がある
#   --- 共変量とセレクションバイアスの関係を知る必要がある
# - バイアスを及ぼす共変量を回帰モデルに加えることでバイアスを緩和することができる
#   ---- 完全にOVBとなるわけではない

# ＜該当ページ＞
# - P49 - 51


# 回帰分析の比較（共変量なし）
# --- RCTデータ
# --- バイアスのあるデータ
rct_reg_coef <- lm(spend ~ treatment, data = male_df) %>% tidy()
nonrct_reg_coef <- lm(spend ~ treatment, data = biased_data) %>% tidy()

# 確認
# --- RCTデータ： 0.770
# --- バイアスのあるデータ： 0.979（セレクションバイアスで効果が過剰に推定されている）
rct_reg_coef %>% print()
nonrct_reg_coef %>% print()


# 回帰分析（共変量あり）
# --- バイアスをかけたデータを回帰係数として追加
# --- 回帰係数は0.847（RCTデータの結果に近づいた）
nonrct_mreg_coef <- lm(spend ~ treatment + recency + channel + history, biased_data) %>% tidy()
nonrct_mreg_coef %>% print()


# 4 脱落変数バイアス(OVB) ---------------------------------------------------------------------

# ＜方針＞
# - 共変量(X)には、目的変数(Y)と介入変数(Z)の両方に相関を持つ係数を加えるべき
#   --- 交絡変数を回帰モデルに加えることで影響を排除する

# ＜ポイント＞
# - ｢脱落変数｣とは本来モデルで必要だがリサーチャーの判断で脱落してしまった変数をいう
# - 統計的に有意でない共変量をモデルから除外してもOVBは発生する
#   --- 完全なモデルを構築するのは困難なので、現実にはある程度のOVBは生じる


# モデルAの評価 ***************************************

# モデルA
# --- モデルAは共変量である｢history｣を見落としたモデル
# --- ｢history｣抜きの回帰分析（バイアスをかけた変数を1つ見逃す）
short_coef <-
   lm(spend ~ treatment + recency + channel, data = biased_data) %>%
     tidy()

# 確認
# --- 回帰係数は0.875（RCTデータから遠ざかった）
short_coef %>% print()

# パラメータ抽出
# --- alpha_1 ：treatmentを見落とした場合の介入効果
alpha_1 <- short_coef %>% filter(term == "treatment") %>% pull(estimate)


# モデルBの評価 ***************************************

# モデルB
# - 共変量を全て含めたモデル
long_coef <-
   lm(spend ~ treatment + recency + channel + history, data = biased_data) %>%
    tidy()

# 確認
# --- 回帰係数は0.847（RCTデータに近づいた）
long_coef %>% print()

# パラメータ抽出
# --- beta_1 ：共変量を全て調整した場合の介入効果
# --- beta_2 ：共変量を全て調整した場合のhistoryの影響度
beta_1 <- long_coef %>% filter(term == "treatment") %>% pull(estimate)
beta_2 <- long_coef %>% filter(term == "history") %>% pull(estimate)


# モデルCの評価 ***************************************

# モデルC
# --- 脱落した変数(history)を介入変数(Z)と他の共変量(X)で回帰
# --- historyに対する介入効果を算出（他の共変量を含めることで影響を排除）
omitted_coef <-
  lm(history ~ treatment + channel + recency, data = biased_data) %>%
    tidy()

# パラメータ抽出
# --- gamma_1 ：historyに対する介入効果
gamma_1 <- omitted_coef %>% filter(term == "treatment") %>% pull(estimate)


# 脱落バイアス(OVB)の確認 *******************************

# ＜ポイント＞
# - 共変量を追加したモデル(A)と追加しなかったモデル(B)で推定される効果の差はOVBに一致する
# - 共変量を追加することで推定される効果の値に変化が生じるのはOBVが消失したことに由来

# ＜インプリケーション＞
# - OVBはモデルAを作らなくても、モデルBの結果と派生のモデルCの結果から算出可能
# - 共変量(X)には、目的変数(Y)と介入変数(Z)の両方に相関を持つ係数を加えるべき（交絡因子の排除）

# ＜公式＞
# alpha1 - beta1 = gamma1 * beta2 (P53)

# OVBの算出
# --- (脱落変数に対する介入効果) * (脱落変数の影響度)
# --- 脱落変数の影響度 ：共変量を全て調整した場合のhistoryの影響度
gamma_1 * beta_2

# OVBの算出
# --- モデルAとモデルBのtreatmentの係数の差分として定義される
alpha_1 - beta_1


# 5 OVBをmap()を使ってアプローチ ---------------------------------------------------------------

# ＜ポイント＞
# - 4でやったことをRのモダンなアプローチで実行する
#   --- 計算的にはスマートだが、OVBの仕組みを理解するには個別にモデルを見たほうが解りやすい

# モデル式の定義
# --- ベクトルで用意
formula_vec <-
  c(spend ~ treatment + recency + channel,
    spend ~ treatment + recency + channel + history,
    history ~ treatment + channel + recency) %>%
    set_names(paste("reg", LETTERS[1:3], sep ="_"))

# tibbleに格納
# --- モデル式のデータフレーム化
models <-
  formula_vec %>%
    enframe(name = "model_index", value = "formula")

# 回帰分析の実行
# --- purrr::map()で一括処理
df_models <-
  models %>%
    mutate(model = map(.x = formula, .f = lm, data = biased_data)) %>%
    mutate(lm_result = map(.x = model, .f = tidy))

# モデル結果の整形
df_results <-
  df_models %>%
    mutate(formula = as.character(formula)) %>%
    select(formula, model_index, lm_result) %>%
    unnest(cols = lm_result)

# パラメータ抽出
# --- 各モデルのtreatmentのパラメータを抜き出す
treatment_coef <-
  df_results %>%
    filter(term == "treatment") %>%
    pull(estimate)

# パラメータ抽出
# --- モデルBからhistoryのパラメータを抽出
history_coef <-
  df_results %>%
    filter(model_index == "reg_B", term == "history") %>%
    pull(estimate)


# OVBの確認
OVB <- history_coef * treatment_coef[3]
coef_gap <- treatment_coef[1] - treatment_coef[2]

# 出力
OVB %>% print()
coef_gap %>% print()


# 6 モデルに不要な変数を入れる弊害 ------------------------------------------------

# ＜ポイント＞
# - セレクションバイアスが減る可能性から、OVBがゼロでない変数は全てモデルに入れてよいわけではない
#   --- Post Treatment Bias


# ＜Post Treatment Bias＞
# - これを避けるには、介入よりも後のタイミングで値が決まるような変数は使ってはいけない
#   --- リサーチャーが適切に判断できるとは限らない


# 回帰分析で高相関な係数を探す
# --- treatmentと相関の高い係数を探す
# --- visitが0.144と有意に正
cor_visit_treatment <-
  lm(treatment ~ visit + channel + recency + history, data = biased_data) %>%
   tidy()

# 不要な変数を含むモデル
# --- 不要な変数： visit
bad_control_reg <-
  lm(spend ~ treatment + channel + recency + history + visit, data = biased_data) %>%
   tidy()

# 結果確認
# --- treatは0.294と大きく低下（RCTをも下回って実験イメージと合わない）
# --- 多重共線性が悪影響を及ぼしている
bad_control_reg %>% print()
