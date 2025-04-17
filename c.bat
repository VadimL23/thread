tasm /z/zi/n System.asm
tasm /z/zi/n Paramstr.asm
tasm /z/zi/n File.asm
tasm /z/zi/n Draw_w.asm
tasm /z/zi/n readwrit.asm
tlink /v readwrit System Draw_w File Paramstr
@REM  pause
@REM readwrit.exe test1.txt

tasm /z/zi/n System.asm
tasm /z/zi/n Paramstr.asm
tasm /z/zi/n Title.asm
tlink /v Title System Paramstr
@REM @REM pause
@REM @REM td title.exe 1


tasm /z/zi/n System.asm
tasm /z/zi/n Paramstr.asm
tasm /z/zi/n Draw_w.asm
tasm /z/zi/n ball.asm
tasm /z/zi/n int.asm
tlink /v ball int System Paramstr Draw_w 
@REM @REM pause
@REM @REM  ball.exe

tasm /z/zi/n System.asm
tasm /z/zi/n Paramstr.asm
tasm /z/zi/n clock.asm
tlink /v clock System Paramstr
@REM @REM @REM pause
@REM @REM @REM td clock.exe 11

tasm /z/zi/n exec.asm
pause
tasm /z/zi/n Paramstr.asm
pause
tasm /z/zi/n System.asm
pause
tasm /z/zi/n Draw_w.asm
pause
tasm /z/zi/n readwrit.asm
pause
tasm /z/zi/n int.asm
pause
tasm /z/zi/n Schedule.asm
@REM pause
tasm /z/zi/n main.asm
tlink /v main exec Draw_w System Paramstr int Schedule 
@REM @REM pause
 main.exe 1 test1.txt 01  
@REM td main.exe 

@REM tasm /z/zi/n System.asm
@REM tasm /z/zi/n int.asm
@REM tlink /v int System
@REM pause
@REM int.exe

@REM tasm /z/zi/n System.asm
@REM tasm /z/zi/n Sheduler.asm
@REM tlink /v Sheduler System
@REM pause
@REM int.exe
@REM tasm

@REM tasm /z/zi/n Draw_w.asm
@REM tasm /z/zi/n test.asm
@REM tlink /v test Draw_w System
@REM pause
@REM td test.exe

:: 
::tasm /z/zi/n cmdtail.asm
::tlink /v cmdtail
::pause
::td cmdtail.exe    text.txt 1 2 3 test///