@echo off

pause

for /r . %%c in ("*.csv") do ren "%%c" *.txt

pause