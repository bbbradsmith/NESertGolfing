
REM build SPC code
wla-dx\wla-spc700 -o spc.o snes.spc.s
@IF ERRORLEVEL 1 GOTO badbuild
wla-dx\wlalink -b snes.spc.link spc.bin 
@IF ERRORLEVEL 1 GOTO badbuild

@echo.
@echo.
@echo Build complete and successful!
@IF NOT "%1"=="" GOTO endbuild
@pause
@GOTO endbuild

:badbuild
@echo.
@echo.
@echo Build error!
@IF NOT "%1"=="" GOTO endbuild
@pause
:endbuild