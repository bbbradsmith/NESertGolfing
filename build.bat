@del temp\*.s
@del temp\*.o
@del temp\dgolf.sfc
@del temp\dgolf.dbg
@del temp\dgolf.map

cc65\bin\cc65 -o temp\dgolf.c.s -O -T -g dgolf.c
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\dgolf.c.o -g temp\dgolf.c.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\dgolf.o -g dgolf.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\snes.o -g snes.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\blend.o -g blend.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o temp\dgolf.sfc -m temp\dgolf.map --dbgfile temp\dgolf.dbg -C dgolf.cfg temp\blend.o temp\dgolf.o temp\dgolf.c.o temp\snes.o temp\runtime.lib
@IF ERRORLEVEL 1 GOTO error

python checksum.py LOROM temp\dgolf.sfc

@echo.
@echo.
@echo Build successful!
@pause
@GOTO end
:error
@echo.
@echo.
@echo Build error!
@pause
:end