.MODEL SMALL

include struc.inc
include prc_ids.inc

EXTRN gotoxy:PROC, draw_window:PROC, write:PROC, _getxy:PROC, open:PROC
EXTRN read:PROC, clrscr:PROC, paramstr_array:BYTE, paramstr:PROC, seek_start:PROC
EXTRN put_char:PROC, put_colored_char:PROC 

public is_random_move
public row_k
public col_k

  str_length equ 2
  video equ 0b800h
  text_color equ 02h


data segment PARA PUBLIC 'DATA'
    string db 1h, text_color
    string_space db 20h, 0
    ;---------------------
    str_status_active db 'Активен','$'
    str_status_wait db 'Ожидает$'
    str_status_suspended db 'Приостановлен$'
    str_status_error db 'Status unknown$'
    str_thread_first db 'Поток №1: $'
    str_thread_second db 'Поток №2: $'
    str_thead_length equ $-str_thread_second
    allredy_installed_err db 'Allredy installed!$'
    ;---------------------
    ; status_r db wait
    ; status_w db wait
  
    row_k db -1
    col_k db -1
    is_random_move db 1
    
    coord dw 0
data ends

code segment PARA PUBLIC 'code'
    assume cs:code, ds:data, es:data, SS:stack
    jmp init

    stdout equ 1
    cr     equ 10
    lf     equ 13 

    active  equ 1
    wait      equ 2
    suspended equ 4

    include utils.inc
    include int_macr.inc
    
;     ball_process_id equ 3
    installed equ 0ffh
    process_function_2f equ 0c0h + ball_process_id -1

    include struc.inc
   ;------ screen --------
        
    wnd TWind<1,40,15,79,2,41,14,78>
    program_length dw 0
    signal_stop_process dw 0
    ;------ Vectors -------------
    old_2fh dd 0
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

@without_param:
        call init_window
      
        mov ax, data
        mov ds, ax  

        @init_process ball_process_id, @start_process

        call init_process_structure_table

; @cycle1:      
;         call process
;          delay 1
; @queue_empty1:
;         jmp @cycle1

outprog:
        mov dx,cs:program_length
        mov ax, 3100h
        int 21h

@start_process:
    
@cycle:      
        mov ax,cs:signal_stop_process
        or ax,ax
        jne @stop_TSR         
        call process
        delay 1
@queue_empty:
        jmp @cycle

@stop_TSR:
        @deactivate_process ball_process_id
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

process proc
        ; call print_symbol
       
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

stack segment para stack
        dw 200h dup(0)
stack ends

zzzz segment
zzzz ends
end