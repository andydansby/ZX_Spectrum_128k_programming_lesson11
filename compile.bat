SET PATH=c:\z88dk199c;c:\z88dk199c\bin;c:\z88dk199c\lib\;c:\z88dk199c\lib\clibs;c:\z88dk199c\lib\config

cls

echo off

cd utils
    copy "bas2tap.exe" "..\magic"
    copy "loader.bas" "..\magic"
    call bankloader.bat
cd ..


rem attention new
cd gfx
    copy "hotrod1.scr" "..\utils"
    copy "hotrod2.scr" "..\utils"
    copy "hotrod3.scr" "..\utils"
    copy "hotrod4.scr" "..\utils"
    copy "hotrod5.scr" "..\utils"
    copy "hotrod6.scr" "..\utils"
    copy "hotrod7.scr" "..\utils"
cd ..

rem attention new
cd utils
    zx0_V2 -f hotrod1.scr hotrod1.zx0
    zx0_V2 -f hotrod2.scr hotrod2.zx0
    zx0_V2 -f hotrod3.scr hotrod3.zx0
    zx0_V2 -f hotrod4.scr hotrod4.zx0
    zx0_V2 -f hotrod5.scr hotrod5.zx0
    zx0_V2 -f hotrod6.scr hotrod6.zx0
    zx0_V2 -f hotrod7.scr hotrod7.zx0

    move "hotrod1.zx0" "..\RAM1"
    move "hotrod2.zx0" "..\RAM1"
    move "hotrod3.zx0" "..\RAM1"
    move "hotrod4.zx0" "..\RAM1"
    move "hotrod5.zx0" "..\RAM3"
    move "hotrod6.zx0" "..\RAM4"
    move "hotrod7.zx0" "..\RAM6"

    del "hotrod1.scr"
    del "hotrod2.scr"
    del "hotrod3.scr"
    del "hotrod4.scr"
    del "hotrod5.scr"
    del "hotrod6.scr"
    del "hotrod7.scr"
cd ..

echo on


cd CONTENDED
	call ramlow.bat
	move "contended.txt" "..\"
cd ..

cd RAM0
	call ram0.bat
	move "ram0.txt" "..\"
cd ..

rem pause

cd RAM1
    call ram1.bat
    move "ram1.txt" "..\"
cd ..

rem pause

cd RAM3
    call ram3.bat
    move "ram3.txt" "..\"
cd ..

rem pause

cd RAM4
    call ram4.bat
    move "ram4.txt" "..\"
cd ..

rem pause

cd RAM6
    call ram6.bat
    move "ram6.txt" "..\"
cd ..

rem pause

cd UNCONTENDED
	call uncontended.bat
	move "uncontended.txt" "..\"
cd ..

rem pause

echo on

move "contended.o" "OBJECTS\"
move "ram0.o" "OBJECTS\"
move "ram1.o" "OBJECTS\"
move "ram3.o" "OBJECTS\"
move "ram4.o" "OBJECTS\"
move "ram6.o" "OBJECTS\"
move "uncontended.o" "OBJECTS\"

cd OBJECTS
	call magic.bat
cd ..

rem pause

cd UNCONTENDED
    call rammain.bat
cd ..


cd magic
    call voodoo.bat
    move "1output.tap" "..\"
cd ..


REM FINISHED COMPILE
call beep.bat
