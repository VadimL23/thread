; ***************************************************************
; *     ПРОГРАММА ВЫВОДА ЧАСОВ НА ЭКРАН                         *
; * Использование: clock.exe <[00;01;10;11]>                    *
; *   00 - вывод в верхнем левом углу экрана                    *
; *   01 - вывод в верхнем правом углу экрана                   *
; *   10 - вывод в нижнем левом углу экрана                     *
; *   11 - вывод в нижнем правом углу экрана                    *
; *                                                             *
; ***************************************************************

.MODEL SMALL
include prc_ids.inc
include struc.inc

EXTRN gotoxy:PROC 
EXTRN clrscr:PROC, paramstr_array, paramstr:PROC, seek_start:PROC

text_color equ 0ah
str_length equ 16
video equ 0b800h

data segment PARA PUBLIC 'DATA'
    allredy_installed_err db 'Allredy installed!$'
    current_time db '0',text_color, '0',text_color,':',text_color,'0',text_color,'0',text_color,':',text_color,'0',text_color,'0',text_color
    coord dw 0
data ends

code segment PARA PUBLIC 'code'
    assume cs:code, ds:data, es:data, SS:stack
    jmp init

    include utils.inc
    include int_macr.inc

;     clock_process_id equ 4
    installed equ 0ffh
    process_function_2f equ 0c0h + clock_process_id - 1

    include struc.inc

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

        call get_coord

@without_param:
        ;  call clrscr     ; TODO:: commit it after test !!!
      
        mov ax, data
        mov ds, ax  

        @fork clock_process_id, @start_process

;         @cycle1:   
;         call process
; @queue_empt1:
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
@queue_empty:
        jmp @cycle

@stop_TSR:
        @deactivate_process clock_process_id
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

process proc
        push BP
        mov bp,sp
        call get_time
        call print_time
        mov sp, bp
        pop bp
        ret
process endp

print_time proc
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
        cmp cl, str_length
        jbe @normalized_col
        sub cl, str_length
@normalized_col:
        add ax, cx
        mov di, ax
        lea si,current_time
        mov cx,str_length
rep     movsb
        pop cx di es
        mov sp, bp
        pop bp
        ret
print_time endp

get_time proc
        push bp
        mov bp,sp
        push ax cx dx
        mov ah,2ch
        int 21h
        ; ????? ? ?????? ???
        mov ax, cx
        and ax,0ff00h
        shr ax,8
        aam
        add ax,3030h
        ; xchg ah,al
        mov byte ptr current_time,ah
        mov byte ptr [current_time+2],al

        ; ????? ??????
        mov ax, cx
        and ax,0ffh
        aam
        add ax,3030h
        ; xchg ah,al
        mov byte ptr [current_time+6],ah
        mov byte ptr [current_time+8],al

        ; ????? ???
        mov ax, dx
        and ax,0ff00h
        shr ax,8
        aam
        add ax,3030h
        ; xchg ah,al
        mov byte ptr [current_time+12],ah
        mov byte ptr [current_time+14],al
        add ax,3030h

        pop dx cx ax
        mov sp,bp
        pop bp
        ret
get_time endp

get_coord proc
        push bp
        mov bp, sp

        push cx bx es ds
        call paramstr  
        mov al, byte ptr paramstr_array
        or al,al
        je @@done   
        xor cx,cx
        lea bx,  [paramstr_array+1]
        mov cl, byte ptr [bx]
        cmp cl,2
        jne @@done
        inc cl
        inc bx
        push es
        push ds
        pop es
        
        mov ax,seg paramstr_array
        mov ds,ax
        lea di,coord
        mov si,bx
    rep movsb
        pop es

        mov ah, byte ptr coord
        mov al, byte ptr [coord+1]
        sub ax, 3030h
        mov byte ptr [coord+1],ah
        xor ah,ah

        mov cl,80
        mul cl
        or al,al
        je @norm_col
        cmp al, 80
        jbe @norm_col
        mov al,0
@norm_col:
        mov byte ptr [coord],al

        mov al, byte ptr [coord+1]
        mov cl,24
        mul cl
        cmp al, 24
        jbe @norm_row
        mov al,0
@norm_row:
        mov byte ptr [coord+1], al
        
@@done:
        pop ds es bx cx
        mov sp,bp
        pop bp
        ret
get_coord endp

stack segment para stack
        dw 200h dup(0)
stack ends

zzzz segment
zzzz ends
end