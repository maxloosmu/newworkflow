@echo off

if "%1"=="all" goto all
if "%1"=="clean" goto clean

:all
ghc --make Main.hs -o transform.exe
goto end

:clean
del *.o *.hi
goto end


:end



