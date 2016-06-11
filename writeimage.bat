@echo off
set /p drv=drive: 
dd if=osimg.img of=\\.\%drv%:
pause