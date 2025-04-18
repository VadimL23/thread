@REM bat файл компилирует и собирает программы

@REM программа вывода чтения файла и вывода на экран
tasm /z/zi/n System.asm
tasm /z/zi/n Paramstr.asm
tasm /z/zi/n File.asm
tasm /z/zi/n Draw_w.asm
tasm /z/zi/n readwrit.asm
tlink /v readwrit System Draw_w File Paramstr
@REM  pause
@REM readwrit.exe test1.txt

@REM программа вывода заголовка
tasm /z/zi/n System.asm
tasm /z/zi/n Paramstr.asm
tasm /z/zi/n Title.asm
tlink /v Title System Paramstr
@REM @REM pause
@REM @REM td title.exe 1

@REM программа движущегося обьекта
tasm /z/zi/n System.asm
tasm /z/zi/n Paramstr.asm
tasm /z/zi/n Draw_w.asm
tasm /z/zi/n ball.asm
tasm /z/zi/n int.asm
tlink /v ball int System Paramstr Draw_w 
@REM @REM  ball.exe

@REM программа вывода часов
tasm /z/zi/n System.asm
tasm /z/zi/n Paramstr.asm
tasm /z/zi/n clock.asm
tlink /v clock System Paramstr
@REM @REM @REM pause
@REM @REM @REM td clock.exe 11

@REM основная программа, запускает другие процессы, планировшик 
@REM и устанавливает таблицу векторов
tasm /z/zi/n exec.asm
tasm /z/zi/n Paramstr.asm
tasm /z/zi/n System.asm
tasm /z/zi/n Draw_w.asm
tasm /z/zi/n readwrit.asm
tasm /z/zi/n int.asm
tasm /z/zi/n Schedule.asm
tasm /z/zi/n main.asm
tlink /v main exec Draw_w System Paramstr int Schedule 
@REM @REM pause
 main.exe 1 test1.txt 01  
@REM td main.exe 