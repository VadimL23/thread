@REM bat 䠩� ���������� � ᮡ�ࠥ� �ணࠬ��

@REM �ணࠬ�� �뢮�� �⥭�� 䠩�� � �뢮�� �� �࠭
tasm /z/zi/n System.asm
tasm /z/zi/n Paramstr.asm
tasm /z/zi/n File.asm
tasm /z/zi/n Draw_w.asm
tasm /z/zi/n readwrit.asm
tlink /v readwrit System Draw_w File Paramstr
@REM  pause
@REM readwrit.exe test1.txt

@REM �ணࠬ�� �뢮�� ���������
tasm /z/zi/n System.asm
tasm /z/zi/n Paramstr.asm
tasm /z/zi/n Title.asm
tlink /v Title System Paramstr
@REM @REM pause
@REM @REM td title.exe 1

@REM �ணࠬ�� �����饣��� ��쥪�
tasm /z/zi/n System.asm
tasm /z/zi/n Paramstr.asm
tasm /z/zi/n Draw_w.asm
tasm /z/zi/n ball.asm
tasm /z/zi/n int.asm
tlink /v ball int System Paramstr Draw_w 
@REM @REM  ball.exe

@REM �ணࠬ�� �뢮�� �ᮢ
tasm /z/zi/n System.asm
tasm /z/zi/n Paramstr.asm
tasm /z/zi/n clock.asm
tlink /v clock System Paramstr
@REM @REM @REM pause
@REM @REM @REM td clock.exe 11

@REM �᭮���� �ணࠬ��, ����᪠�� ��㣨� ������, �����஢訪 
@REM � ��⠭�������� ⠡���� ����஢
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