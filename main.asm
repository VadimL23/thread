.model SMALL
include prc_ids.inc
include struc.inc
include utils.inc
include int_macr.inc

EXTRN exec:PROC, draw_window:PROC, write:PROC, gotoxy:PROC, clrscr:PROC
EXTRN paramstr:PROC, paramstr_array:BYTE
EXTRN scheduler:PROC, threadsRegistered, disableHardwareEvents, new_09h:PROC, old_09h, old_08h
EXTRN is_all

data segment PARA PUBLIC 'data' 
hello db ' Esc - Вых,'
      db ' F1 - зап./ост. движущийся обьект' 
      db ' F2 - зап./ост. поток вывода на экран информации из файла'
      db ' F3 - зап./ост. поток чтения из файла'
      db ' F4 - зап./ост. вывод заголовка работы'
      db ' F5 - зап./ост. часы'
      db ' q/w - +/- время работы потока чтения из файла'
      db ' +/- вр. работы потока вывода из файла'
      db ' +/- вр. вывода заголовка'
      db ' +/- вр. дв. обьекта'    
hello_length equ $-hello
text_color equ 003h

paramstr_err db 0ch,0dh,'Error in param string.$'

    ;****************** PARAM BLOCS   *********************
    ; ***************** FIRST PROCESS BLOCK ***************
    first_param_block_process dw 0 ; Инициилизурем окружением основного процеесса
                              dd first_process_tail
                              dd 0   ; FCB1
                              dd 0   ; FCB2
    first_child_process_name db 'title.exe',0
    first_process_tail db 0
                 db 0Dh
                 db 253 dup(0Dh)
    exec_title_keep_err db 'Error occured with keep title process.$'
    exec_title_err db 'Something went wrong with the title process.$'
    dw 10 dup (0)


    ; ***************** SECOND PROCESS BLOCK ***************
    second_param_block_process dw 0
                               dd second_process_tail
                               dd 0
                               dd 0
    second_child_process_name db 'readwrit.exe',0
    second_process_tail db 0
                db 0Dh                        
                db 253 dup(0Dh)
    exec_reader_writer_keep_err db 'Error occured with keep reader-writer process.$'
    exec_reader_writer_err db 'Something went wrong with the reader-writer process.$'
    ;*******************************

    ; ***************** THIRD PROCESS BLOCK ***************
    third_param_block_process dw 0
                               dd third_process_tail
                               dd 0
                               dd 0
    third_child_process_name db 'clock.exe',0
    third_process_tail db 0
                db 0Dh
                db 253 dup(0Dh)
    exec_clock_object_keep_err db 'Error occured with keep clock object process.$'
    exec_clock_object_err db 'Something went wrong with the clock object process.$'
    ;*******************************

    ; ***************** FOURTH PROCESS BLOCK ***************
    fourth_param_block_process dw 0
                               dd fourth_process_tail
                               dd 0
                               dd 0
    fourth_child_process_name db 'ball.exe',0
    fourth_process_tail db 0
                db 0Dh
                db 253 dup(0Dh)
    exec_moved_object_keep_err db 'Error occured with keep moved object process.$'
    exec_moved_object_err db 'Something went wrong with the moved object process.$'
    ;*******************************

      wnd TWind<16,0,22,79,17,1,21,78>
   
    col_max = 79
    row_max = 21

    cursor dw 0
    stdot equ 1te
   ;------ screen --------

    thread_array TThread 5 dup(<>)
data ends

code segment PARA PUBLIC 'code'
assume cs:code, ds:data

main proc
    mov ax, data
    mov ds, ax
    
    ; Программа .EXE занимает всю свободную память
    ; освободим ее
    call free_not_used_mem 
    call make_child_process_paramstr  ; TODO :: What a fack here ???

    call init_process_table
    inc cs:threadsRegistered
    @init_process main_process_id, @infinity

    call draw_window_process
 
    call init_reader_writer_process
    inc cs:threadsRegistered
    ; inc cs:threadsRegistered

    call init_title_process
    inc cs:threadsRegistered
  
    call init_ball_process
    inc cs:threadsRegistered
  
    call init_clock_process
    inc cs:threadsRegistered


 
   call init_interrupts
@infinity:
    mov ax,cs:is_all
    or ax,ax
    je @infinity

    mov ax,0c0ffh       ; join process 1
    int 2fh

    mov ax,0c1ffh       ; join process 2
    int 2fh

    mov ax,0c2ffh       ; join process 3
    int 2fh

    mov ax,0c3ffh       ; join process 4
    int 2fh

    @deactivate_process main_process_id
    call restore_interrupts
    mov ax,4c00h
    int 21h
main endp

init_interrupts proc
    @change_vect 9 new_09h old_09h
    @change_vect 08h scheduler old_08h
    ret
init_interrupts endp

restore_interrupts proc
    @restore_vect 9 old_09h 
    @restore_vect 08h  old_08h 
    ret
restore_interrupts endp

init_title_process proc
    push bp
    mov bp, sp
    push ax
    lea ax, first_param_block_process
    push ax
    lea ax, first_child_process_name
    push ax
    lea ax, exec_title_keep_err
    push ax
    lea ax, exec_title_err
    push ax
    call init_process
    pop ax
    mov sp, bp
    pop bp
    ret
init_title_process endp

init_reader_writer_process proc
    push bp
    mov bp, sp
    push ax
    lea ax, second_param_block_process
    push ax
    lea ax, second_child_process_name
    push ax
    lea ax, exec_reader_writer_keep_err
    push ax
    lea ax, exec_reader_writer_err
    push ax
    call init_process
    pop ax
    mov sp, bp
    pop bp
    ret
init_reader_writer_process endp


init_clock_process proc
    push bp
    mov bp, sp
    push ax
    lea ax, third_param_block_process
    push ax
    lea ax, third_child_process_name
    push ax
    lea ax, exec_clock_object_keep_err
    push ax
    lea ax, exec_clock_object_err
    push ax
    call init_process
    pop ax
    mov sp, bp
    pop bp
    ret
init_clock_process endp

init_ball_process proc
    push bp
    mov bp, sp
    push ax
    lea ax, fourth_param_block_process
    push ax
    lea ax, fourth_child_process_name
    push ax
    lea ax, exec_moved_object_keep_err
    push ax
    lea ax, exec_moved_object_err
    push ax
    call init_process
    pop ax
    mov sp, bp
    pop bp
    ret
init_ball_process endp

; [pb+10 - param_block_process]
; [pb+8 - child_process_name]
; [pb+6 - exec_title_keep_err]
; [pb+4 - exec_title_err]
init_process proc
    push bp
    mov bp, sp
    push dx
    mov ax, data
    push ax
    mov ax,[bp+10]
    push ax
    mov ax, [bp+8]
    push ax
    call far ptr exec
    mov ah, 4dh
    int 21h
    cmp ah,3 
    je @exec_process_exit
    ;lea dx, exec_title_keep_err
    mov dx, [bp+6]
    mov ah,9
    int 21h
    jmp @exec_process_exit
    cmp al, 0
    je @exec_process_exit
   ; lea dx, exec_title_err
    mov ax, [bp+4]
    mov ah,9
    int 21h
    jmp @exec_process_exit
    cmp al, 0
@exec_process_exit:
    pop dx
    mov sp, bp
    pop bp
    ret 8
init_process endp

; Получим аргументы командной строки для запуска дочерних процессов
make_child_process_paramstr proc
    push bp
    mov bp,sp
    push ax bx di si
    call paramstr
    xor ax,ax
    mov al, byte ptr paramstr_array+1
    or ax,ax
    je @without_param
    cmp ax, 0
    je @without_param      ;Bad comman params
    ; First param
    lea bx, paramstr_array
    inc bx
    mov al, byte ptr [bx]
    cmp al,1
    jne @fail_paramstr
    mov al, byte ptr [bx]
    mov first_process_tail,al
    inc first_process_tail
    lea di, first_process_tail
    inc di
    mov byte ptr [di], ' '
    inc di
    inc bx
    mov al,  byte ptr [bx]
    mov byte ptr [di], al
    inc di
    mov byte ptr [di], 0dh  

    ; Second param
    lea si, paramstr_array
    add si, 3
    inc [si]
    inc [si]
    lea di, second_process_tail
    xor cx, cx
    mov cl, byte ptr [si]
    dec cl
    dec cl
    mov al,20H
    movsb
    stosb
    cld
rep movsb

; Therd param
    mov cx,2
    xor ax,ax
    lea bx, paramstr_array
    mov al, byte ptr [bx]
    cmp al, 3
    jne @fail_paramstr
    inc bx
@get_third_param:
    mov al, byte ptr [bx]
    or al,al
    je @without_param
    add bx,ax
    inc bx
    loop @get_third_param
    
    xor cx,cx
    dec bx
    dec bx
    mov cl, byte ptr [bx]
    lea di, third_process_tail
    mov si,bx
    inc si
    mov al,20H
    stosb
 rep   movsb

 
@fail_paramstr:
    print_str paramstr_err
    jmp @without_param

@without_param:
    pop si di bx ax
    mov sp, bp
    pop bp
    ret
make_child_process_paramstr endp

free_not_used_mem proc
    push bp
    mov bp, sp
    push dx es bx ax
    mov ax, zzz
    mov dx, es
    sub ax, dx
    mov bx, ax
    mov ah, 4ah
    int 21h
    pop ax bx es dx
    mov sp, bp
    pop bp
    ret
free_not_used_mem endp

draw_window_process proc
    push bp
    mov bp,sp
    call clrscr
    mov ax, seg wnd
    push ax 
    mov ax, offset wnd
    push ax
    call far ptr draw_window

    mov ax, offset wnd
    push ax 
    mov ax, text_color
    push ax
    mov ax, offset hello
    push ax
    mov ax, hello_length
    push ax
    call print
    mov sp,bp
    pop bp
    ret
draw_window_process endp

; bp+4 - str length
; bp+6 - str offset and str ended $
; bp+8 - attr
; bp+10 - TWind
print proc
    local
    push bp
    mov bp, sp
    push cx si ax bx dx di
    mov di,[bp+10]
    ; mov dx, [bp+8]
    mov dl, byte ptr [di].TWind.inner_left_top_col 
    mov dh, byte ptr [di].TWind.inner_left_top_row 
    push dx
    call gotoxy

    mov cx, [bp+4]
    ; dec cx
    ; push cx
    ; mov si, [bp+6]

@std_out_space:    
    mov bx,[bp+8]
    mov al, ' '
    mov ah,09h
    push cx 
    mov cx,1
    int 10h
    pop cx
    inc dx
    cmp dl, [di].TWind.right_bottom_col
    jnz @std_out_space_continue
    mov dl, [di].TWind.inner_left_top_col
    inc dh
@std_out_space_continue:    
    push dx
    call gotoxy
    loop @std_out_space

    mov dl, byte ptr [di].TWind.inner_left_top_col 
    mov dh, byte ptr [di].TWind.inner_left_top_row 
    push dx
    call gotoxy
    mov cx, [bp+4]
@std_out:

;******* getxy ******
    push cx dx bx
    mov bh,0
    mov ah,3h
    int 10h
    mov ax, dx
    pop bx dx cx
;******* getxy ******

    cmp al, [di].TWind.right_bottom_col
    jnz @std_out_str_continue
    mov al, [di].TWind.left_top_col
    inc al
    inc ah
    push ax
    call gotoxy

@std_out_str_continue:
    mov al, ds:[si]
    call write
    inc si
    loop @std_out

    pop di dx bx ax si cx
    mov sp, bp
    pop bp
    ret 10
print endp


; Процедура очистки окна
; Листать окно вверх (или очистить). Листать на 1 или более строк вверх.
; Вход:
; bp+10 (CH,CL) = строка,столбец верхнего левого угла окна (считая от 0)
; bp+8 (DH,DL) = строка,столбец нижнего правого угла окна (считая от 0)
; bp+6 (AL) = число пустых строк, вдвигаемых снизу (0=очистить все окно)
; bp+4 (BH) = видео атрибут, используемый для пустых строк
clrwind proc
        push bp
        mov bp, sp
        push ax bx cx dx
        mov cx, [bp+10]
        mov dx, [bp+8]
        mov al, [bp+7]
        mov bh, [bp+5]
        mov ah, 06h
        int 10h
        pop dx cx bx ax
        mov sp, bp
        pop bp
        ret 8
clrwind endp

init_process_table proc
    push bp
    mov bp, sp
    push es di ax
    xor ax,ax
    mov al, bios_process_linked_seg
    mov es, ax
    mov al, bios_process_linked_offs
    mov di, ax
    lea ax, thread_array
    mov es:[di], ax
    mov ax, seg thread_array
    mov es:[di+2], ax
    pop ax di es
    mov sp, bp
    pop bp
    ret
init_process_table endp

code ends

stack segment STACK
    dw 200h dup (0)
stack ends


zzz segment
zzz ends

end