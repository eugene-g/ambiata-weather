@echo off

REM This is an alias for "paket" command

REM install latest paket version
IF NOT EXIST ".paket\paket.exe" ".paket\paket.bootstrapper.exe"

.paket\paket.exe %*