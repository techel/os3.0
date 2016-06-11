@echo off
set /p drv=drive to write to: 
set /p fs=drives' filesystem: 
make install arch=x86 fs=%fs% "osimg=\\.\%drv%:" "osdir=%drv%:/
pause