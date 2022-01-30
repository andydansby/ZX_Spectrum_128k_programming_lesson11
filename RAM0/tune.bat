SET PATH=c:\z88dk199c;c:\z88dk199c\bin;c:\z88dk199c\lib\;c:\z88dk199c\lib\clibs;c:\z88dk199c\lib\config

rem attention temp
cls

cd ..
    cd music
        copy "WYZPROPLAY47c_ZX2.ASM" "..\RAM0"
		copy "WYZ_variables.asm" "..\RAM0"
        copy "SFX.ASM" "..\RAM0"
        copy "instrumentos.asm" "..\RAM0"
		copy "wyz.asm" "..\RAM0"
		copy "efectos.asm" "..\RAM0"
        copy "songs.asm"  "..\RAM0"
    cd ..
cd RAM0


rem attention temp

cd codemaps
	del ram0.o
cd ..

@rem zx0 -f funkyfun.mus funkyfun.zx0
apack c funkyfun.mus funkyfun.zx0

@rem this creates an object file
zcc +zx -vn -SO3 -c -clib=new -pragma-include:zpragma.inc -o RAM0.o --fsigned-char @ram0.lst

if not exist "RAM0.o" (
call error.bat
)

copy "ram0.o" "..\"
move "ram0.o" "codemaps\"

@REM Cleanup
del zcc_opt.def


REM a nice map view
cd codemaps
	echo on
	@REM all these objects match up
	z80nm ram0.o
	z80nm ram0.o > ram0.txt
	copy "ram0.txt" "..\"
	echo off
cd ..

@call beep.bat
