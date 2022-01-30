@cd codemaps
@	del ram0.o
@cd ..

rem attention new
cd songs
zx0 -f bad_nationwide.pt3 nationwide.zx0
zx0 -f cheapsunglasses.pt3 cheapsunglasses.zx0
zx0 -f lagrange2.pt3 lagrange2.zx0

move "nationwide.zx0" "..\"
move "cheapsunglasses.zx0" "..\"
move "lagrange2.zx0" "..\"
cd ..

@rem this creates an object file
zcc +zx -vn -SO3 -c -clib=new -pragma-include:zpragma.inc -o RAM0.o --fsigned-char @ram0.lst


@if not exist "RAM0.o" (
call error.bat
)

@copy "ram0.o" "..\"
@move "ram0.o" "codemaps\"

@REM Cleanup
@del zcc_opt.def
@del "nationwide.zx0"
@del "cheapsunglasses.zx0"
@del "lagrange2.zx0"

REM a nice map view
@cd codemaps
	@REM all these objects match up
	z80nm ram0.o
	z80nm ram0.o > ram0.txt
	@copy "ram0.txt" "..\"
	@echo off
@cd ..

@call beep.bat

