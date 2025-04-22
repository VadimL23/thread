; ***************************************************************
; *     ПРОГРАММА ЧТЕНИЯ ДАННЫХ ИЗ ФАЙЛА И ВЫВОДА НА ЭКРАН      *
; * Использование: readwrite.exe <file_name>                    *
; * Программа читает данные из файла и выводит данный в рабочую *
; * область. Структура программы Производитель-потребитель.     *
; * Оба потока (нити) работают независимо. Для доступа к        * 
; * разделяемой памяти(кольцевому буферу), реализован алго-     *
; * ритм Петерсона.                                             *
; *                                                             * 
; * Примечание: Если файл, название которого передано как пара- *
; * метр при запуске программы не найден, то читается файл с    *                                                             * 
; * названием test.txt                                          * 
; *                                                             * 
; ***************************************************************

.MODEL SMALL
include prc_ids.inc
include struc.inc
include utils.inc
include int_macr.inc
include struc.inc

EXTRN gotoxy:PROC, draw_window:PROC, write:PROC, _getxy:PROC, open:PROC
EXTRN read:PROC, clrscr:PROC, paramstr_array:BYTE, paramstr:PROC, seek_start:PROC

data segment PARA PUBLIC 'DATA'
   ;---------------------- file --------------------------------
    file_name db 'text.txt',0                           ; Хранит имя файла
    db 100 dup(0)
    handle dw 0                                         ; Хранит хэндл файла
    file_not_found_err db 'File not found!$'            ; Ошибка что файл не найден
    allredy_installed_err db 'Allredy installed!$'      ; Ошибка, что резидент уже в памяти
   ;---------------------- file --------------------------------

    ;--------------------- queue ------------------------------
    head dw 0                                           ; указатель начала кольцевого буфера
    tail dw 0                                           ; указатель хвота кльцевого буфера
    buff_size equ 3                                     ; Размер очереди (количество строк)
    buff_string_length equ 23                           ; Размер строки (1 байт на длину + 22 байт на данные)
    buff_str db buff_size dup (buff_string_length dup(20h)) ; кольцевой буфер
    ;--------------------- queue ------------------------------

    ;-------- for G.L.Peterson algorithm since 1981 year -------
        true equ 1
        false equ 0
        thread_count equ 2      ; Количество нитей
        intrested db 2 dup(0)   
        turn db 0
    ;============================================================

data ends

code segment PARA PUBLIC 'code'
    assume cs:code, ds:data, es:data, SS:stack
    jmp init

    stdout equ 1
    cr     equ 10                 ; перевод каретки
 
;     readwrite_process_id equ 1                        ; id процесса чтения из файла
    process_write_id equ 2                              ; id процесса вывода на экран
    installed equ 0ffh                                  ; Константа для проверки уже запущен процеили нет, для избежания дубля резедента в памяти
    process_function_2f equ 0c0h + readwrite_process_id -1  ; Функция процесса, прерывания 2f
   ;------ screen --------
        
    wnd TWind<1,0,15,39,1,1,14,38>                      ; Структура для построения окна программы
    program_length dw 0                                 ; Хранит длину программы в параграфах
    signal_stop_process dw 0                            ; Сигнал остановки процесса
    ;------ Vectors -------------
    old_2fh dd 0                                        ; Хранит значение вектора 2f 
    ;-----------------------------
 
; Обработчик прерывания 2f

new_2fh proc                                            ; Перепишем прерывание 2f, установим пользовательскую функцию, для
    local                                               ; проверки резидента в памяти и для обработки сигнала завершения программы
    cmp ah, process_function_2f                         ; Проверяем наша функция вызвана или нет
    jne @exit
    cmp al,0
    jne @title_2f_0
    mov al,0ffh
    jmp @exit
@title_2f_0:
    cmp al,0ffh
    jne @title_2f_ff
    mov cs:signal_stop_process,1                        ; Вызвана функция остановки программы
    @restore_vect 2fh cs:old_2fh
    jmp @exit
@title_2f_ff:
@exit:    
    jmp cs:old_2fh
new_2fh endp

; Макрос для реализации алгоритма Петерсона

@enter_region macro process
   mov si,1
   sub si,process
   lea di,intrested
   add di,process
   mov byte ptr [di], true
   mov byte ptr turn, process
endm

init:
        mov ax, data
        mov ds,ax                                       ; Вычисление размера программы в параграфах
        mov ax,zzzz                                     ; для оставления программы в памяти резедентной TSR
        mov dx, es                                      ; es = PSP
        sub ax,dx                                       ; ax = длина программы в параграфах
        mov cs:program_length,ax                        
        xor ax,ax                                       
        mov ah,process_function_2f
        int 2fh
        cmp al, installed                               ; Проверим что нет дубля программы в памяти
        jne @not_installed
        call do_exit

@not_installed:                                         ; Устанавливаем вектор 2f
        @change_vect 2fh new_2fh cs:old_2fh
        mov ax, seg code
        mov ds, ax
        call get_file_name_from_paramstr                ; Получаем параметр командной строки

@without_param:
        ;call clrscr                                    ; commit it after test !!!
        mov ax, seg wnd
        push ax 
        mov ax, offset wnd
        push ax
        call far ptr draw_window                        ; инициализация окна
        mov ax,seg wnd
        mov ds,ax
        mov dh,byte ptr wnd.inner_right_bottom_row
        mov dl,1
        push dx
        call gotoxy     
        mov ax, data
        mov ds, ax  

        @fork readwrite_process_id, @start_process_read ; установим первый поток (читает данные из файла)
        @fork process_write_id, @start_process_write    ; установим второй поток (выводит  данные на экран)
        @get_ptr_to_thread_data 2                       ; т.к. процесс один, то разделим стек междудвумя потоками
        mov ax,word ptr es:[di].TThread.r_sp            ;
        sub ax,100h                                     ;
        mov word ptr es:[di].TThread.r_sp,ax            ;

outprog:                                                ; завершаем и оставляем резидентной
        mov dx,cs:program_length
        mov ax, 3100h
        int 21h


@start_process_read:                                    ; подпрограмма чтения из файла              
       call open_file
@cycle:  
        @enter_region 0
@wait_read:
        cmp byte ptr turn,0
        jne @read_turn
        cmp byte ptr intrested+1, true
        jmp @wait_read
@read_turn:                               
        mov ax,cs:signal_stop_process
        or ax,ax
        jne @stop_TSR         
        call process_read_from_file
        jmp @cycle

@start_process_write:                                   ; подпрограмма вывода на экран                  
        @enter_region 1
@wait_write:
        cmp byte ptr turn,1
        jne @write_turn
        cmp byte ptr intrested, true
        jmp @wait_write
@write_turn: 
        mov ax,cs:signal_stop_process
        or ax,ax
        jne @stop_TSR
        call process_stdout
        jmp @start_process_write

@stop_TSR:
        @deactivate_process readwrite_process_id        ; Остановим программу и освободим память, восстанавливаем вектора
@rrr:
        jmp @rrr                                        ; This is STUB 

do_exit proc                                            ; Выполняем выход из программы,
        mov ax,0d23h                                    ; с ошибкой, программа уже установлена в памяти
        push ax                                         ; 
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

; процедура открытия файла

open_file proc
        push bp
        mov bp,sp
        mov ax,data
        push ax
        mov ds, ax
        lea ax, file_name
        push ax
        mov ax, 0000h
        push ax        ; Файл откроем на чтение
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

; подпрограмма вывода на экран

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
        mov cl, [bx]                            ; количество символов в строке
        inc bx                                  ; указатель на начало строки
        call stdout_string  
        mov bx, [tail]
        inc bx
        cmp bx, buff_size
        jl @no_wrap_tail
        xor bx, bx                              ; Сброс tail, если достигнут конец буфера
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

; Выводит символы в цикле
; bp - increment
; cx - количество символов в строке

stdout_string proc
        push bp
        mov bp, sp
@stdout_buff_string:
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

; процедура чтения данных из файла

process_read_from_file proc       
        push bp
        mov bp, sp
        mov ax, [head]
        mov bx, [tail]
        cmp ax, buff_size - 1
        jl @process_read
        xor ax, ax                 ; Сброс head, если достигнут конец буфера
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
        call seek_start            ; установим указатель на началофайла
@not_eof:
        mov ax, [head]             ; Проверка на переполнение
        mov bx, [tail]
        inc ax
        cmp ax, buff_size 
        jl @no_wrap
        xor ax, ax                 ; Сброс head, если достигнут конец буфера
@no_wrap:
        mov head, ax
        cmp ax, bx
        je @queue_full             ; Если head == tail, очередь переполнена   
        jmp @process_read
@queue_full:
        mov sp, bp
        pop bp
        ret
process_read_from_file endp

; процедура прокрутки окна
;bp+4 - TWind

scroll_window_up proc
        local
        push bp
        mov bp, sp
        push bx cx dx ax
        mov bx,[bp+4]
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


; Скролинг вверх
; bp+10(сх) - координаты левого верхнего угла прямоугольной области экрана (ch - строка, cl - столбец), 
; bp+8(dx) - координаты правого нижнего угла (dh ? строка, dl ? столбец), 
; bp+6(al) - на сколько строк прокручивать заданное окно (при al = 0 все заданное окно очищается), 
; bp+4(bh) - атрибуты для заполнения освобождающихся строк (7 - белый по черному)

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

; Процедура читает имя файла из параметра запуска программы

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

; Зарезервируем стек

stack segment para stack
        dw 200h dup(0)
stack ends

; Сегмент необходим для вычисления размера программы

zzzz segment
zzzz ends
end