@echo off
setlocal enabledelayedexpansion

REM ==== 1) KHAI BÁO TOP TEST ====
set TESTS=tdr_test tcr_test tsr_test null_address mixed_address fake_over_under_flow updw_reset_load_dwup_pclk2 upup_pause_dwdw_pclk2 countup_forkjoin_pclk2 countdw_forkjoin_pclk2

REM ==== 2) DỌN & CHUẨN BỊ ====
if exist work rmdir /s /q work
vlib work
if exist cov rmdir /s /q cov
mkdir cov

REM ==== 3) COMPILE 1 LẦN ====
vlog -coveropt 3 +cover +acc ^
  Def_name.v Rw_register.v Control_logic.v select_clock.v Timer_counter.v IP_Timer_8bit.v ^
  tdr_test.v tcr_test.v tsr_test.v null_address.v mixed_address.v fake_over_under_flow.v ^
  updw_reset_load_dwup_pclk2.v upup_pause_dwdw_pclk2.v ^
  countup_forkjoin_pclk24816.v countdw_forkjoin_pclk24816.v
if errorlevel 1 exit /b 1

REM ==== 4) CHẠY TỪNG TEST (MỖI TEST → 1 UCDB) ====
set "UCDBS="

for %%T in (%TESTS%) do (
  echo.
  echo ===== RUN %%T =====
  REM xài **relative path** → tránh khoảng trắng, tránh escape
  vsim -coverage work.%%T -c -do ^
    "coverage save -onexit -directive -codeAll {cov/%%T.ucdb}; run -all; quit -f"
  if errorlevel 1 echo [WARN] %%T ended with errors.

  if exist "cov\%%T.ucdb" (
    echo [OK] cov\%%T.ucdb
    set UCDBS=!UCDBS! cov/%%T.ucdb
  ) else (
    echo [ERR] KHÔNG tạo được cov\%%T.ucdb  – Xem transcript trong vsim log
  )
)

REM ==== 5) MERGE ⇢ coverage_all.ucdb ====
if "x!UCDBS!"=="x" (
  echo [ERROR] Không có UCDB nào – dừng.
  exit /b 1
)
vcover merge -out coverage_all.ucdb !UCDBS!

REM ==== 6) XUẤT HTML ====
rmdir /s /q htmlreport 2>nul
vcover report -html -htmldir htmlreport -directive coverage_all.ucdb

echo.
echo DONE – mở htmlreport\index.html
endlocal
pause
