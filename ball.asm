; ***************************************************************
; *     ПРОГРАММА ВЫВОДА ДВИЖУЩЕГОСЯ ОБЬЕКТА НА ЭКРАН           *
; * Использование: ball.exe                                     *
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
video equ 0b800h                                ; Сегмент памяти видеодаптера
text_color equ 02h                              ; Цвет движущегося обьекта

data segment PARA PUBLIC 'DATA'
    string db 1h, text_color                    ; Строка движущегося обьекта
    string_space db 20h, 0                      ; Константа пробела
    allredy_installed_err db 'Allredy installed!$' ; Ошибка, что резидент уже в памяти
    row_k db -1                                 ; Коэициент для строк
    col_k db -1                                 ; Коэфициент для колонок
    is_random_move db 1                         ; Флаг рандомное или ручное перемещение
    coord dw 0                                  ; Текущие кординаты обьекта
data ends

code segment PARA PUBLIC 'code'
    assume cs:code, ds:data, es:data, SS:stack
    jmp init
    
;     ball_process_id equ 3                     ; id процесса в таблице процессов
    installed equ 0ffh                          ; Константа для проверки уже запущен процеили нет, для избежания дубля резедента в памяти
    process_function_2f equ 0c0h + ball_process_id -1 ; Функция процесса, прерывания 2f

   ;------ window --------
    wnd TWind<1,40,15,79,2,41,14,78>            ; Структура для построения окна программы

    program_length dw 0                         ; Хранит длину программы в параграфах
    signal_stop_process dw 0                    ; Сигнал остановки процесса
    
    ;------ Vectors -------------
    old_2fh dd 0                                ; Хранит значение вектора 2f
    ;-----------------------------

; Обработчик прерывания 2f

new_2fh proc                                    ; Перепишем прерывание 2f, установим пользовательскую функцию, для
    local                                       ; проверки резидента в памяти и для обработки сигнала завершения программы
    cmp ah, process_function_2f                 ; Проверяем наша функция вызвана или нет
    jne @exit
    cmp al,0
    jne @title_2f_0
    mov al,0ffh
    jmp @exit
@title_2f_0:
    cmp al,0ffh
    jne @title_2f_ff
    mov cs:signal_stop_process,1                ; Вызвана функция остановки программы
    @restore_vect 2fh cs:old_2fh
    jmp @exit
@title_2f_ff:
@exit:    
    jmp cs:old_2fh
new_2fh endp

init: 
        mov ax, data
        mov ds,ax                                ; Вычисление размера программы в параграфах
        mov ax,zzzz                              ; для оставления программы в памяти резедентной TSR
        mov dx, es                               ; es = PSP
        sub ax,dx                                ; ax = длина программы в параграфах
        mov cs:program_length,ax
        xor ax,ax
        mov ah,process_function_2f
        int 2fh
        cmp al, installed                        ; Проверим что нет дубля программы в памяти
        jne @not_installed
        call do_exit

@not_installed:        
        @change_vect 2fh new_2fh cs:old_2fh
        mov ax, seg code
        mov ds, ax

@without_param:
        call init_window                          ; Отрисуем окно программы
        mov ax, data
        mov ds, ax  

        @fork ball_process_id, @start_process      ; Установим поток в таблице потоков
        call init_process_structure_table          ; Процедура записывает ссылку на переменные row_k, col_k

; @cycle1:      
;         call process
;          delay 1
; @queue_empty1:
;         jmp @cycle1

outprog:
        mov dx,cs:program_length                        ; Выходим из программы и оставляем резедентной
        mov ax, 3100h
        int 21h

@start_process:                                         ; Цикл программы   
@cycle:      
        mov ax,cs:signal_stop_process
        or ax,ax
        jne @stop_TSR         
        call process
        delay 1                                         ; Введем задержку при перемещении обьекта
@queue_empty:
        jmp @cycle

@stop_TSR:
        @deactivate_process ball_process_id             ; Остановим программу и освободим память, восстанавливаем вектора
@rrr:
        jmp @rrr        ; This is STUB 

do_exit proc                                            ; Выполняем выход из программы,
        mov ax,0d23h                                    ; с ошибкой, программа уже установлена в памяти
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

; Процедура выводит на пробел

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

; Процедура выводит на экран движущийся обьект

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

; Процедура выводит на экран символ

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

; Процедура записывает ссылку на переменные row_k, col_k
; в межпрограмную область биос

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

; Основной поток

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

; Процедура обрабатывает нажатие на стреки

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

; Процедура вычисляет координаты следующего шага перемещения

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

; Процедура инициализации окна программы

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

; Зарезервируем стек

stack segment para stack
        dw 200h dup(0)
stack ends

; Сегмент необходим для вычисления размера программы

zzzz segment
zzzz ends
end