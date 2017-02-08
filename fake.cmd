@echo off

REM This is an alias for "fake" command

"packages\FAKE\tools\Fake.exe" %*
exit /b %errorlevel%