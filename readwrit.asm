.MODEL SMALL
include prc_ids.inc
include struc.inc
; 4a61:0120
; 4a610026
EXTRN gotoxy:PROC, draw_window:PROC, write:PROC, _getxy:PROC, open:PROC
EXTRN read:PROC, clrscr:PROC, paramstr_array:BYTE, paramstr:PROC, seek_start:PROC

data segment PARA PUBLIC 'DATA'
   ;------- file ---------
    file_name db 'text.txt',0
    db 100 dup(0)
    handle dw 0
    file_not_found_err db 'File not found!$'
    allredy_installed_err db 'Allredy installed!$'
   ;------- file ---------
   
    ;---------------------
    str_status_active db '��⨢��','$'
    str_status_wait db '�������$'
    str_status_suspended db '�ਮ�⠭�����$'
    str_status_error db 'Status unknown$'

    str_thread_first db '��⮪ �1: $'
    str_thread_second db '��⮪ �2: $'
    str_thead_length equ $-str_thread_second
    ;---------------------

    status_r db status_wait
    status_w db status_wait

    ;------ queue ---------
    head dw 0
    tail dw 0
    buff_size equ 2             ; ������ ��।� (������⢮ ��ப)
    buff_string_length equ 5   ; ������ ��ப� (1 ���� �� ����� + 30 ���� �� �����)
    bsl equ 10
    buff_top db 0
    buff_str db buff_size dup (buff_string_length dup(20h)) ; ���� ��� ��।�
    ;------ queue ---------

data ends

code segment PARA PUBLIC 'code'
    assume cs:code, ds:data, es:data, SS:stack
    jmp init

    stdout equ 1
    cr     equ 10
    lf     equ 13 

    status_active  equ 1
    status_wait      equ 2
    status_suspended equ 4

    include utils.inc
    include int_macr.inc

   ;------ screen --------
    leftTop equ 201
    rightTop equ 187
    horizontal equ 205
    vertical equ 186
    leftBottom equ 200
    rightBottom equ 188
    col_max = 79
    row_max = 24
    cursor dw 0
    
;     readwrite_process_id equ 1
    process_write_id equ 2
    installed equ 0ffh
    process_function_2f equ 0c0h + readwrite_process_id -1

    include struc.inc
   ;------ screen --------
        
    wnd TWind<1,0,15,39,1,1,14,38>
    program_length dw 0
    signal_stop_process dw 0
    ;------ Vectors -------------
    old_2fh dd 0 ; bc de   38 01 f1 48
    ;-----------------------------

new_2fh proc
    local
    cmp ah, process_function_2f
    jne @exit
    cmp al,0
    jne @title_2f_0
    mov al,0ffh
    jmp @exit
@title_2f_0:
    cmp al,0ffh
    jne @title_2f_ff
    mov cs:signal_stop_process,1
    @restore_vect 2fh cs:old_2fh
    jmp @exit
@title_2f_ff:
@exit:    
    jmp cs:old_2fh
new_2fh endp

init: 
        mov ax, data
        mov ds,ax
        mov ax,zzzz
        mov dx, es
        sub ax,dx
        mov cs:program_length,ax
        xor ax,ax
        mov ah,process_function_2f
        int 2fh
        cmp al, installed
        jne @not_installed
        call do_exit

@not_installed:        
        @change_vect 2fh new_2fh cs:old_2fh

        mov ax, seg code
        mov ds, ax

        call get_file_name_from_paramstr

@without_param:
        ;call clrscr     ; TODO:: commit it after test !!!
        mov ax, seg wnd
        push ax 
        mov ax, offset wnd
        push ax
        call far ptr draw_window
        
        mov ax,seg wnd
        mov ds,ax
        mov dh,byte ptr wnd.inner_right_bottom_row
        mov dl,1
        push dx
        call gotoxy
      
        mov ax, data
        mov ds, ax  

        @init_process readwrite_process_id, @start_process_read
       ; @init_process process_write_id, @start_process_write

;   call open_file
; @cycle_1:          
                
;         call process_read_from_file
;         call process_stdout
; @queue_empty_1:
;         jmp @cycle_1


outprog:
        mov dx,cs:program_length
        mov ax, 3100h
        int 21h

@start_process_read:
       call open_file
@cycle:                         ;4A61:011B
        mov ax,cs:signal_stop_process
        or ax,ax
        jne @stop_TSR         
        call process_read_from_file
        call process_stdout
;@queue_empty:
        jmp @cycle

; @start_process_write:    
;         mov ax,cs:signal_stop_process
;         or ax,ax
;         jne @stop_TSR         
;         ; call process_read_from_file
;         ;call process_stdout
; ; @queue_empty:
;         jmp @start_process_write

@stop_TSR:
        @deactivate_process readwrite_process_id
@rrr:
        jmp @rrr        ; This is STUB 

do_exit proc
        mov ax,0d23h
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

open_file proc
        push bp
        mov bp,sp
        mov ax,data
        push ax
        mov ds, ax
        lea ax, file_name
        push ax
        mov ax, 0000h
        push ax        ; ���� ��஥� �� �⥭��
        call open 
        jnc @file_found
        print_str file_not_found_err
@file_found:

        mov handle, ax
        push ax
        call seek_start  
        mov sp, bp
        pop bp
        ret
open_file endp

process_stdout proc
       push bp
       mov bp,sp
       push di bx cx 
       mov di,0
@stdout_next_str: 

        mov ax, [tail]
        
        mov di, buff_string_length
        mul di
        xchg di, ax
        lea bx, buff_str[di]
        xor cx,cx
        mov cl, [bx]            ; ������⢮ ᨬ����� � ��ப�
        inc bx                  ; 㪠��⥫� �� ��砫� ��ப�

        call stdout_string
     
        mov bx, [tail]
        inc bx
        cmp bx, buff_size
        jl @no_wrap_tail
        xor bx, bx                 ; ���� tail, �᫨ ���⨣��� ����� ����
@no_wrap_tail:
        mov tail, bx
        mov ax, [head]
        cmp ax,bx
        je @go_out_process_stdout

 jmp @stdout_next_str
 @go_out_process_stdout:
        pop cx bx di
        mov sp,bp
        pop bp 
        ret
process_stdout endp

        ; �뢮��� ᨬ���� � 横��
        ; bp - increment
        ; cx - ������⢮ ᨬ����� � ��ப�
stdout_string proc
        push bp
        mov bp, sp
@stdout_buff_string:
        ; call _getxy
        ;******* getxy ******
        push cx dx bx
        mov bh,0
        mov ah,3h
        int 10h
        mov ax, dx
        pop bx dx cx
        ;******* getxy ******
        cmp al,cs:wnd.inner_right_bottom_col
        jne @below_row
        mov al,0ah
        inc cx
        dec bx
        jmp @do_scroll
@below_row:
        mov al, [bx]
@do_scroll:
        cmp al,0ah
        jne @stdout_write
        mov dx, offset wnd
        push dx
        call scroll_window_up
        jmp @next_char
@enter_push:

@stdout_write:
         call write
@next_char:
        inc bx
        loop @stdout_buff_string
        mov sp, bp
        pop bp
        ret
stdout_string endp

process_read_from_file proc   ; 4a61:0120
        push bp
        mov bp, sp

        mov ax, [head]
        mov bx, [tail]
        cmp ax, buff_size - 1
        jl @process_read
        xor ax, ax                 ; ���� head, �᫨ ���⨣��� ����� ����
@process_read:      
        mov di, buff_string_length
        mul di
        xchg di, ax
        mov ax, data
        mov ds,ax
        lea dx, buff_str[di+1]
        
        push handle
        mov ax, buff_string_length-1
        push ax
        call read

        dec dx
        mov bx, dx
        mov byte ptr [bx],al
        cmp al, buff_string_length-1
        je @not_eof 
        push word ptr handle
        call seek_start

@not_eof:
        mov ax, [head]     ; �஢�ઠ �� ��९�������
        mov bx, [tail]
        inc ax
        cmp ax, buff_size 
        jl @no_wrap
        xor ax, ax                 ; ���� head, �᫨ ���⨣��� ����� ����
@no_wrap:
        mov head, ax
        cmp ax, bx
        je @queue_full             ; �᫨ head == tail, ��।� ��९������
    
        jmp @process_read

@queue_full:
        mov sp, bp
        pop bp
        ret
process_read_from_file endp

;bp+4 - TWind
scroll_window_up proc
        local
        push bp
        mov bp, sp
        push bx cx dx ax
        mov bx,[bp+4]
        ; call _getxy
        ;******* getxy ******
        push cx dx bx
        mov bh,0
        mov ah,3h
        int 10h
        mov ax, dx
        pop bx dx cx
        ;******* getxy ******
        cmp ah, cs:[bx].TWind.inner_left_top_row
          
        delay 10
        mov ah, byte ptr cs:[bx].TWind.inner_left_top_row
        inc ah
        mov al, byte ptr cs:[bx].TWind.inner_left_top_col
        push ax
        mov ah, byte ptr cs:[bx].TWind.inner_right_bottom_row
        mov al, byte ptr cs:[bx].TWind.inner_right_bottom_col
        push ax
        mov ax,1
        push ax
        mov ax, 0fh
        push ax
        
        call scroll_up
       
        mov dh, byte ptr cs:[bx].TWind.inner_right_bottom_row
        mov dl, byte ptr cs:[bx].TWind.inner_left_top_col
        push dx
        call gotoxy
@done:
        pop ax dx cx bx
        mov sp, bp
        pop bp
        ret 2
scroll_window_up endp


; ��஫��� �����
; bp+10(��) ? ���न���� ������ ���孥�� 㣫� ��אַ㣮�쭮� ������ �࠭� (ch ? ��ப�, cl ? �⮫���), 
; bp+8(dx) ? ���न���� �ࠢ��� ������� 㣫� (dh ? ��ப�, dl ? �⮫���), 
; bp+6(al) ? �� ᪮�쪮 ��ப �ப��稢��� �������� ���� (�� al = 0 �� �������� ���� ��頥���), 
; bp+4(bh) - ��ਡ��� ��� ���������� �᢮���������� ��ப (7 - ���� �� �୮��)

scroll_up proc
        push BP
        mov bp,sp

        push ax bx cx
        mov cx, [bp+10]
        mov dx, [bp+8]
        mov al, [bp+6]
        mov bh, [bp+4]
        mov ah, 06h
        ;mov bh, 0fh
        int 10h
        pop cx bx ax

        mov sp, bp
        pop bp
        ret 8
scroll_up endp

; ���⠥� ����� ��⮪�
; �室: ah - ����� ��⮪�

print_status proc
        local
        push bp
        mov bp,sp
        push dx ax
        push ax
        ; call _getxy
        ;******* getxy ******
        push cx dx bx
        mov bh,0
        mov ah,3h
        int 10h
        mov ax, dx
        pop bx dx cx
        ;******* getxy ******
        mov dx, ax

        mov ax,011ah
        push ax
        call gotoxy
 
        pop ax
       
        cmp ah, status_active
        jz @active
        cmp ah, status_suspended
        jz @suspended
        cmp ah, status_wait
        jz @wait
        
        jmp @status_err
@active:
        print_str str_status_active
        jmp @out
@suspended:
        print_str str_status_suspended
        jmp @out
@wait:
        print_str str_status_wait
        jmp @out
@status_err:
        print_str str_status_error
@out:
        push dx
        call gotoxy
        pop  ax dx
        mov sp,bp
        pop bp
        ret
print_status endp

get_file_name_from_paramstr proc
  
        push bp
        mov bp,sp
        push cx bx es ds
        call paramstr  
        mov al, byte ptr paramstr_array
        or al,al
        je @@done
        xor cx,cx
        lea bx,  [paramstr_array+1]
        mov cl, byte ptr [bx]
        inc cl
        inc bx
        push es
        push ds
        pop es
        mov ax,seg paramstr_array
        mov ds,ax
        lea di,file_name
        mov si,bx
rep     movsb
        pop es
@@done:
        pop es ds es bx cx
        mov sp,bp
        pop bp
        ret
get_file_name_from_paramstr endp
code ends 

stack segment para stack
        dw 200h dup(0)
stack ends

zzzz segment
zzzz ends
end