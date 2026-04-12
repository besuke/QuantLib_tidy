# QuantLib_tidy
QuantLib (R + SWIG) Build Guide for Windows (Rtools45)

==================================================
Overview / 概要
==================================================

This document explains how to build QuantLib for R on Windows using:

本ドキュメントでは、Windows環境で以下を用いて QuantLib（R版）をビルドする方法を説明する。

- R 4.5+
- Rtools45
- QuantLib (C++)
- QuantLib-SWIG (R bindings)

Goal / 目的:

library(QuantLib)


==================================================
Key Principles / 重要原則
==================================================

1. Use ONE toolchain only / ツールチェーンは必ず1つに統一

Use:
- Rtools45 (gcc, g++, make)

Do NOT use:
- MSVC (Visual Studio)
- vcpkg
- Mixed compilers

→ コンパイラ混在はDLL不整合の原因


--------------------------------------------------

2. Match versions EXACTLY / バージョン完全一致

QuantLib: 1.41
QuantLib-SWIG: 1.41

→ 不一致はビルドエラーの原因


--------------------------------------------------

3. Use Rtools Bash / RtoolsのBashを使う

All commands must be run in:

Rtools45 → Bash

→ PowerShellやcmdは使わない


==================================================
Prerequisites / 事前準備
==================================================

- R 4.5.x
- Rtools45
- Git

確認コマンド:

which gcc
gcc --version

which g++
g++ --version

which make
which autoreconf


==================================================
Step 1 — Build QuantLib (C++)
==================================================

cd /c
git clone https://github.com/lballabio/QuantLib.git
cd QuantLib

git fetch --tags
git checkout v1.41


ビルド:

./configure --prefix=/c/ql-install
make -j4
make install


確認:

/c/ql-install/bin/quantlib-config --version

→ インストール先: C:\ql-install


==================================================
Step 2 — Build QuantLib-SWIG
==================================================

cd /c
git clone https://github.com/lballabio/QuantLib-SWIG.git
cd QuantLib-SWIG

git fetch --tags
git checkout v1.41


==================================================
Step 3 — Configure SWIG
==================================================

PATH追加:

export PATH="/c/ql-install/bin:$PATH"


実行:

./autogen.sh

./configure \
  --with-quantlib-prefix=/c/ql-install \
  --disable-python \
  --disable-java \
  --disable-csharp \
  --disable-scala

→ Rだけビルドする


==================================================
Step 4 — Build R bindings
==================================================

make -C R


==================================================
Step 5 — Install into R
==================================================

R CMD INSTALL R


==================================================
Step 6 — Verify in R
==================================================

library(QuantLib)

today <- DateParser_parseISO("2022-05-10")
Settings_instance()$setEvaluationDate(today)


==================================================
File Locations / ファイル配置
==================================================

QuantLib install:
C:\ql-install\

SWIG source:
C:\QuantLib-SWIG\

R package:
C:\Users\USER\Documents\R\win-library\4.5\QuantLib

DLL:
...\QuantLib\libs\x64\QuantLib.dll


==================================================
Important Usage Notes (R SWIG)
==================================================

1. Date handling / 日付

NG:
Date(10, "May", 2022)

OK:
DateParser_parseISO("2022-05-10")

→ 文字列形式が安全


--------------------------------------------------

2. Method calling / 呼び方

Object style:
swap$NPV()
swap$fairRate()
quote$value()
quote$setValue(x)

Function style:
Instrument_setPricingEngine(swap, engine)
Index_addFixing(index, date, rate)

→ SWIGは2系統API


--------------------------------------------------

3. SimpleQuote

NG:
SimpleQuote_value(q)
SimpleQuote_setValue(q, x)

OK:
q$value()
q$setValue(x)


--------------------------------------------------

4. Curve inspection

NG:
curve$nodes()

→ stack overflowの原因

OK:
curve$dates()
curve$discount(d)


--------------------------------------------------

5. RateHelperVector

Rでは自動変換されないため手動で作る:

v <- RateHelperVector()
for (h in helpers) {
  RateHelperVector_append(v, h)
}


==================================================
Common Errors / よくあるエラー
==================================================

1. Version mismatch

Error:
no member named ...

Fix:
→ QuantLibとSWIGのバージョンを揃える


--------------------------------------------------

2. quantlib-config not found

Fix:
export PATH="/c/ql-install/bin:$PATH"


--------------------------------------------------

3. autoreconf not found

Fix:
→ Rtools Bashを使う


--------------------------------------------------

4. DLL load failure

Fix:
- MSVCを使わない
- Rtoolsで再ビルド


==================================================
Minimal Test / 最小テスト
==================================================

library(QuantLib)

today <- DateParser_parseISO("2022-05-10")
Settings_instance()$setEvaluationDate(today)

curve <- FlatForward(today, 0.03, Actual365Fixed())
handle <- YieldTermStructureHandle(curve)

engine <- DiscountingSwapEngine(handle)


==================================================
Summary / まとめ
==================================================

- Rtools45のみ使用
- QuantLibとSWIGのバージョン一致
- Python流APIは使わない
- $method()を基本にする
- RateHelperVectorは手動構築


==================================================
Next Steps / 次のステップ
==================================================



==================================================
Part R — QuantLib-SWIG (R) Practical Guide
==================================================

This section summarizes rules and templates for safely using QuantLib-SWIG in R.

--------------------------------------------------
R-1. Basic Rules
--------------------------------------------------

1. Always use object-style method calls:
   swap$NPV()
   quote$setValue(x)

2. Function-style APIs are auxiliary:
   Instrument_setPricingEngine(swap, engine)

3. SimpleQuote must use:
   q$value()
   q$setValue(x)

   NG:
   SimpleQuote_value(q)

4. Date must be created via ISO string:

   OK:
   DateParser_parseISO("2024-04-12")

   NG:
   Date(12, "Apr", 2024)

5. Always store Handles in variables:

   h <- YieldTermStructureHandle(curve)

6. RelinkableHandle is NOT auto-updating:

   h$linkTo(newCurve)

7. RateHelperVector is NOT auto-converted in R:

   must append manually


--------------------------------------------------
R-2. Date Handling (CRITICAL)
--------------------------------------------------

today <- DateParser_parseISO("2024-04-12")
Settings_instance()$setEvaluationDate(today)

DO NOT use Date() constructor.


--------------------------------------------------
R-3. SimpleQuote
--------------------------------------------------

q <- SimpleQuote(0.03)

q$value()
q$setValue(0.031)


--------------------------------------------------
R-4. Handle / RelinkableHandle
--------------------------------------------------

q <- SimpleQuote(0.03)

h <- RelinkableYieldTermStructureHandle()

curve <- FlatForward(today, q$value(), Actual365Fixed())

h$linkTo(curve)

q$setValue(0.035)


--------------------------------------------------
R-5. RateHelperVector (MANDATORY)
--------------------------------------------------

v <- RateHelperVector()

for (h in helpers) {
  RateHelperVector_append(v, h)
}


--------------------------------------------------
R-6. Common NG / OK Patterns
--------------------------------------------------

NG: curve$nodes()
OK: curve$dates(), curve$discount(t)

NG: SimpleQuote_value(q)
OK: q$value()

NG: Date(10, "May", 2022)
OK: DateParser_parseISO("2022-05-10")

NG: swap.NPV()
OK: swap$NPV()


==================================================
Part T — Practical Templates
==================================================

--------------------------------------------------
T-1. Flat Curve
--------------------------------------------------

library(QuantLib)

today <- DateParser_parseISO("2024-04-12")
Settings_instance()$setEvaluationDate(today)

curve <- FlatForward(today, 0.03, Actual365Fixed())
h <- YieldTermStructureHandle(curve)


--------------------------------------------------
T-2. Swap Pricing
--------------------------------------------------

library(QuantLib)

today <- DateParser_parseISO("2024-04-12")
Settings_instance()$setEvaluationDate(today)

curve <- FlatForward(today, 0.03, Actual365Fixed())
h <- YieldTermStructureHandle(curve)

engine <- DiscountingSwapEngine(h)

fixedRate <- 0.025
tenor <- Period(5, "Years")

schedule <- Schedule(
  today,
  today + tenor,
  Period(6, "Months"),
  TARGET(),
  "ModifiedFollowing",
  "ModifiedFollowing",
  "Forward",
  FALSE
)

swap <- VanillaSwap(
  Swap_Payer_get(),
  1000000,
  schedule,
  fixedRate,
  Thirty360(),
  schedule,
  Euribor6M(h),
  0.0
)

swap$setPricingEngine(engine)

swap$NPV()
swap$fairRate()


--------------------------------------------------
T-3. Bond Pricing
--------------------------------------------------

library(QuantLib)

today <- DateParser_parseISO("2024-04-12")
Settings_instance()$setEvaluationDate(today)

curve <- FlatForward(today, 0.03, Actual365Fixed())
h <- YieldTermStructureHandle(curve)

schedule <- Schedule(
  today,
  today + Period(5, "Years"),
  Period(1, "Years"),
  TARGET(),
  "ModifiedFollowing",
  "ModifiedFollowing",
  "Forward",
  FALSE
)

bond <- FixedRateBond(
  2,
  100,
  schedule,
  c(0.02),
  ActualActual()
)

engine <- DiscountingBondEngine(h)
bond$setPricingEngine(engine)

bond$cleanPrice()
bond$yield(ActualActual(), "Compounded", "Annual")


--------------------------------------------------
T-4. Bootstrapping (Deposit + Swap)
--------------------------------------------------

library(QuantLib)

today <- DateParser_parseISO("2024-04-12")
Settings_instance()$setEvaluationDate(today)

helpers <- list(

  DepositRateHelper(
    QuoteHandle(SimpleQuote(0.01)),
    Period(1, "Months"),
    2,
    TARGET(),
    "ModifiedFollowing",
    TRUE,
    Actual360()
  ),

  SwapRateHelper(
    QuoteHandle(SimpleQuote(0.025)),
    Period(5, "Years"),
    TARGET(),
    "Annual",
    "ModifiedFollowing",
    Thirty360(),
    Euribor6M()
  )
)

v <- RateHelperVector()

for (h in helpers) {
  RateHelperVector_append(v, h)
}

curve <- PiecewiseLogLinearDiscount(
  0,
  TARGET(),
  v,
  Actual365Fixed()
)

curve$discount(1.0)
curve$discount(2.0)


==================================================
Part W — Windows Operations
==================================================

--------------------------------------------------
W-1. Makevars.win
--------------------------------------------------

QL_PREFIX=C:/ql-install

PKG_CPPFLAGS = -I$(QL_PREFIX)/include
PKG_LIBS = -L$(QL_PREFIX)/lib -lQuantLib


--------------------------------------------------
W-2. DLL dependency check
--------------------------------------------------

ntldd QuantLib.dll


--------------------------------------------------
W-3. Fast rebuild
--------------------------------------------------

cd /c/QuantLib-SWIG

make clean -C R
make -C R
R CMD INSTALL R


==================================================
Part E — Errors and Fixes
==================================================

1. DLL load failed

Cause:
- PATH issues
- MSVC mixed
- version mismatch

Fix:
export PATH="/c/ql-install/bin:$PATH"


--------------------------------------------------

2. stack overflow

Cause:
curve$nodes()

Fix:
curve$dates()
curve$discount()


--------------------------------------------------

3. no member named ...

Cause:
QuantLib / SWIG version mismatch


==================================================
Part Tips — Practical Tips
==================================================

- Always use ISO date strings
- Always store Handles explicitly
- RelinkableHandle does NOT auto-update
- RateHelperVector must be built manually
- Always use $method() style