; ***************************************************************
; *     ��������� ������ ����������� ������� �� �����           *
; * �ᯮ�짮�����: ball.exe                                     *
; *                                                             *
; ***************************************************************

.MODEL SMALL

include struc.inc
include prc_ids.inc
include utils.inc
include int_macr.inc

EXTRN gotoxy:PROC, draw_window:PROC, write:PROC, _getxy:PROC, open:PROC
EXTRN read:PROC, clrscr:PROC, paramstr_array:BYTE, paramstr:PROC, seek_start:PROC
EXTRN put_char:PROC, put_colored_char:PROC 

public is_random_move
public row_k
public col_k

str_length equ 2
video equ 0b800h                                ; ������� ����� ����������
text_color equ 02h                              ; ���� �����饣��� ��쥪�

data segment PARA PUBLIC 'DATA'
    string db 1h, text_color                    ; ��ப� �����饣��� ��쥪�
    string_space db 20h, 0                      ; ����⠭� �஡���
    allredy_installed_err db 'Allredy installed!$' ; �訡��, �� १����� 㦥 � �����
    row_k db -1                                 ; ���樥�� ��� ��ப
    col_k db -1                                 ; ����樥�� ��� �������
    is_random_move db 1                         ; ���� ࠭������ ��� ��筮� ��६�饭��
    coord dw 0                                  ; ����騥 ��न���� ��쥪�
data ends

code segment PARA PUBLIC 'code'
    assume cs:code, ds:data, es:data, SS:stack
    jmp init
    
;     ball_process_id equ 3                     ; id ����� � ⠡��� ����ᮢ
    installed equ 0ffh                          ; ����⠭� ��� �஢�ન 㦥 ����饭 ��楨�� ���, ��� ��������� �㡫� १����� � �����
    process_function_2f equ 0c0h + ball_process_id -1 ; �㭪�� �����, ���뢠��� 2f

   ;------ window --------
    wnd TWind<1,40,15,79,2,41,14,78>            ; ������� ��� ����஥��� ���� �ணࠬ��

    program_length dw 0                         ; �࠭�� ����� �ணࠬ�� � ��ࠣ���
    signal_stop_process dw 0                    ; ������ ��⠭���� �����
    
    ;------ Vectors -------------
    old_2fh dd 0                                ; �࠭�� ���祭�� ����� 2f
    ;-----------------------------

; ��ࠡ��稪 ���뢠��� 2f

new_2fh proc                                    ; ��९�襬 ���뢠��� 2f, ��⠭���� ���짮��⥫���� �㭪��, ���
    local                                       ; �஢�ન १����� � ����� � ��� ��ࠡ�⪨ ᨣ���� �����襭�� �ணࠬ��
    cmp ah, process_function_2f                 ; �஢��塞 ��� �㭪�� �맢��� ��� ���
    jne @exit
    cmp al,0
    jne @title_2f_0
    mov al,0ffh
    jmp @exit
@title_2f_0:
    cmp al,0ffh
    jne @title_2f_ff
    mov cs:signal_stop_process,1                ; �맢��� �㭪�� ��⠭���� �ணࠬ��
    @restore_vect 2fh cs:old_2fh
    jmp @exit
@title_2f_ff:
@exit:    
    jmp cs:old_2fh
new_2fh endp

init: 
        mov ax, data
        mov ds,ax                                ; ���᫥��� ࠧ��� �ணࠬ�� � ��ࠣ���
        mov ax,zzzz                              ; ��� ��⠢����� �ணࠬ�� � ����� १����⭮� TSR
        mov dx, es                               ; es = PSP
        sub ax,dx                                ; ax = ����� �ணࠬ�� � ��ࠣ���
        mov cs:program_length,ax
        xor ax,ax
        mov ah,process_function_2f
        int 2fh
        cmp al, installed                        ; �஢�ਬ �� ��� �㡫� �ணࠬ�� � �����
        jne @not_installed
        call do_exit

@not_installed:        
        @change_vect 2fh new_2fh cs:old_2fh
        mov ax, seg code
        mov ds, ax

@without_param:
        call init_window                          ; ����㥬 ���� �ணࠬ��
        mov ax, data
        mov ds, ax  

        @fork ball_process_id, @start_process      ; ��⠭���� ��⮪ � ⠡��� ��⮪��
        call init_process_structure_table          ; ��楤�� �����뢠�� ��뫪� �� ��६���� row_k, col_k

; @cycle1:      
;         call process
;          delay 1
; @queue_empty1:
;         jmp @cycle1

outprog:
        mov dx,cs:program_length                        ; ��室�� �� �ணࠬ�� � ��⠢�塞 १����⭮�
        mov ax, 3100h
        int 21h

@start_process:                                         ; ���� �ணࠬ��   
@cycle:      
        mov ax,cs:signal_stop_process
        or ax,ax
        jne @stop_TSR         
        call process
        delay 1                                         ; ������ ����প� �� ��६�饭�� ��쥪�
@queue_empty:
        jmp @cycle

@stop_TSR:
        @deactivate_process ball_process_id             ; ��⠭���� �ணࠬ�� � �᢮����� ������, ����⠭�������� �����
@rrr:
        jmp @rrr        ; This is STUB 

do_exit proc                                            ; �믮��塞 ��室 �� �ணࠬ��,
        mov ax,0d23h                                    ; � �訡���, �ணࠬ�� 㦥 ��⠭������ � �����
        push ax                                         
        call gotoxy
        mov ax, seg allredy_installed_err
        mov ds,ax
        lea dx, allredy_installed_err
        mov ah,09h
        int 21h
        mov ax, 4Cffh
        int 21h
        ret
do_exit endp

; ��楤�� �뢮��� �� �஡��

print_space proc
        push bp
        mov bp,sp
        push ax
        lea ax,string_space
        push ax
        mov ax,str_length
        push ax
        call print
        pop ax
        mov bp,sp
        pop bp
        ret
print_space endp

; ��楤�� �뢮��� �� �࠭ �����騩�� ��쥪�

print_symbol proc
        push bp
        mov bp,sp
        push ax
        lea ax,string
        push ax
        mov ax,str_length
        push ax
        call print
        pop ax
        mov bp,sp
        pop bp
        ret
print_symbol endp

; ��楤�� �뢮��� �� �࠭ ᨬ���

print proc
        push BP
        mov bp,sp
        push es di cx
        mov ax, video  
        mov es, ax
        mov cx, coord
        mov ax, cx
        mov al, ah
        mov ah,160
        mul ah
        xor ch,ch
        shl cl,1
        cmp cl, byte ptr [bp+4]
        jbe @normalized_col
        sub cl, byte ptr [bp+4]
@normalized_col:
        add ax, cx
        mov di, ax
        mov si,[bp+6]
        mov cx,word ptr [bp+4]
rep     movsb
        pop cx di es
        mov sp, bp
        pop bp
        ret 4
print endp

; ��楤�� �����뢠�� ��뫪� �� ��६���� row_k, col_k
; � ����ணࠬ��� ������� ����

init_process_structure_table proc
        push bp
        mov bp, sp
        push es di ax
        xor ax,ax
        mov al,40h
        mov es, ax
        mov al, 0f4h
        mov di, ax
        lea ax, row_k
        mov es:[di], ax
        mov ax, seg row_k
        mov es:[di+2], ax
        pop ax di es
        mov sp, bp
        pop bp
        ret
init_process_structure_table endp

; �᭮���� ��⮪

process proc
        call print_space
        mov ax, coord
        cmp is_random_move,1 
        jne @not_random_move
        call step_random
        jmp @do_move
 @not_random_move:
        call hand_step
 @do_move:
        call print_symbol
        ret
process endp

; ��楤�� ��ࠡ��뢠�� ����⨥ �� ��४�

hand_step proc
        cmp ah, wnd.inner_left_top_row
        jne @hand_step_dont_change_row_k_left
        cmp row_k,-1
        jne @hand_step_dont_change_row_k_left 
        mov row_k,0
@hand_step_dont_change_row_k_left:
        cmp ah, wnd.inner_right_bottom_row
        jne @hand_step_dont_change_row_k_right
        cmp row_k,1
        jne @hand_step_dont_change_row_k_right 
        mov row_k,0
@hand_step_dont_change_row_k_right:
        add ah, row_k
        mov cl, wnd.inner_left_top_col
        inc cl
        cmp al, cl
        jne @hand_step_dont_change_col_k_left
        cmp col_k,-1
        jne @hand_step_dont_change_col_k_left 
        mov col_k,0
@hand_step_dont_change_col_k_left:
        cmp al, wnd.inner_right_bottom_col
        jne @hand_step_dont_change_col_k_right
        cmp col_k,1
        jne @hand_step_dont_change_col_k_right 
        mov col_k,0
@hand_step_dont_change_col_k_right:
        add al, col_k
        mov coord, ax
        mov col_k, 0
        mov row_k,0
        ret
hand_step endp

; ��楤�� ������ ���न���� ᫥���饣� 蠣� ��६�饭��

step_random proc
        push bp
        mov bp,sp
        cmp ah, wnd.inner_left_top_row
        je @step_random_change_row_k 
        cmp ah, wnd.inner_right_bottom_row
        jne @step_random_dont_change_row_k
@step_random_change_row_k:
        neg row_k
@step_random_dont_change_row_k:
        add ah, row_k
        mov cl,  wnd.inner_left_top_col
        inc cl
        cmp al, cl
        je @step_random_change_col_k 
        cmp al, wnd.inner_right_bottom_col
        jne @step_random_dont_change_col_k
@step_random_change_col_k:
        neg col_k
@step_random_dont_change_col_k:
        add al, col_k      
        mov coord,ax
        mov sp, bp
        pop bp
        ret
step_random endp

; ��楤�� ���樠����樨 ���� �ணࠬ��

init_window proc
        push bp
        mov bp,sp
        push ax dx
        ; call clrscr     ; TODO:: commit it after test !!!
        mov ax, seg wnd
        push ax 
        mov ax, offset wnd
        push ax
        call far ptr draw_window
        ; *************** Set cursor to the center window *************
        mov ax,seg wnd
        mov ds,ax
        push bx
        xor ax,ax
        mov al,byte ptr wnd.inner_right_bottom_row
        mov ah, byte ptr wnd.inner_left_top_row
        sub al, ah
        mov ah,0
        mov dl,2
        div dl
        mov ah, byte ptr wnd.inner_left_top_row
        add al, ah
        mov bh, al
        xor ax, ax
        mov al,byte ptr wnd.inner_right_bottom_col
        mov ah,byte ptr wnd.inner_left_top_col
        sub al,ah
        mov ah,0
        div dl
        mov ah,byte ptr wnd.inner_left_top_col
        add al, ah
        mov bl, al
        xchg dx, bx
        pop bx
        mov ax, data
        mov ds,ax
        mov dx,0430h 
        mov coord,dx
        ; *************** Set cursor to the center window *************
        pop dx ax
        mov sp, bp
        pop bp
        ret
init_window endp
code ends 

; ��१�ࢨ�㥬 �⥪

stack segment para stack
        dw 200h dup(0)
stack ends

; ������� ����室�� ��� ���᫥��� ࠧ��� �ணࠬ��

zzzz segment
zzzz ends
end