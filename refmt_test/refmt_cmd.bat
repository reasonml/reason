@echo off
setlocal EnableDelayedExpansion

set "REFMT=%~dp0..\src\refmt\refmt_impl.exe"
set "ARGS=%*"
set "ARGS=!ARGS:"=""!"

"%REFMT%" !ARGS!
