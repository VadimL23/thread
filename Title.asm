; ***************************************************************
; *     ПРОГРАММА ВЫВОДА ЗАГОЛОВКА РАБОТЫ НА ЭКРАН              *
; * Использование: title.exe <[0;1]>                            *
; *   0 - вывод заголовка вверху экрана                         *
; *   1 - вывод заголовка внизу экрана                          *
; *                                                             *
; ***************************************************************

.model SMALL
include prc_ids.inc
include struc.inc
include int_macr.inc
include utils.inc

EXTRN gotoxy:PROC, clrscr:PROC, _getxy:PROC, paramstr:PROC, paramstr_array, atoi:PROC

; title_process_id equ 2                                ; Номер процесса
is_test equ 0                                           ; Флаг тест режим
installed equ 0ffh                                      ; Константа для проверки уже запущен процеили нет, для избежания дубля резедента в памяти
function_2f equ 0c0h + title_process_id -1              ; Функция процесса, прерывания 2f

data segment PARA PUBLIC 'DATA'
    program_length dw 0                                 ; Хранит длину программы в параграфах
    title_str db 'Курсовой проект. Лачков Вадим Валерьевич. Группа: ПБ-21' ; Выводимая строка заголовка
    title_length equ $-title_str                        ; Хранит длину строки
    allredy_installed_err db 'Allredy installed!$'      ; Ошибка, что резидент уже в памяти
    location dw 0                                       ; Хранит координаты
data ends

code segment PARA PUBLIC 'CODE'
    ASSUME CS:code, DS:data, SS:stack
jmp init

;------------------- Vectors -----------------------
old_2fh dd 0                                            ; Хранит значение вектора 2f
;---------------------------------------------------
color db 1                                              ; Хранит текущий цвет строки
signal_stop_process dw 0                                ; Сигнал остановки процесса
maincs dw 0
;---------------------------------------------------

new_2fh proc                                            ; Перепишем прерывание 2f, установим пользовательскую функцию, для
    local                                               ; проверки резидента в памяти и для обработки сигнала завершения программы
    cmp ah, function_2f                                 ; Проверяем наша функция вызвана или нет
    jne @exit
    cmp al,0
    jne @title_2f_0
    mov al,0ffh
    jmp @exit
@title_2f_0:
    cmp al,0ffh
    jne @title_2f_ff                                    ; Вызвана функция остановки программы
    mov cs:signal_stop_process,1
    @restore_vect 2fh cs:old_2fh
    jmp @exit
@title_2f_ff:
@exit:    
    jmp cs:old_2fh
new_2fh endp
    
    init:
        mov ax, data
        mov ds, ax
        call paramstr
        
        xor ax,ax
        mov al, byte ptr paramstr_array
        or ax,ax
        je @without_param

        lea ax, [paramstr_array+1]
        push ax
        call atoi  
        cmp ax,1
        jne @without_param
        inc location

@without_param:                                         ; Вычисление размера программы в параграфах
        mov ax,zzz                                      ; для оставления программы в памяти резедентной TSR
        mov dx, es                                      ; es = PSP
        sub ax,dx                               
        mov program_length,ax                           ; ax = длина программы в параграфах
        xor ax,ax
        mov ah,function_2f
        int 2fh
        cmp al, installed                               ; Проверим, что нет дубля программы в памяти
        jne @not_installed
        call do_exit
@not_installed:  
        mov ax, is_test
        or ax,ax
        jne @it_is_test
        @change_vect 2fh new_2fh old_2fh
        jmp @main
@it_is_test:
        call clrscr
@test_cycle:
        call process
        loop @test_cycle
@main:
        mov ax, 0b800h
        mov es,ax
        
       @fork title_process_id, @cycle                   ; Установим поток в таблице потоков

        mov ax, cs                                      ; Выходим и оставляем TSR
        mov cs:maincs, ax 
        mov dx,program_length
        mov ax, 3100h
        int 21h

@cycle:                                                ; Выводим строку, пока не получим сигнал на завершение 
        mov ax,signal_stop_process
        or ax,ax
        jne @stop_TSR
        call process
        loop @cycle

@stop_TSR:                                              ; Останавливаем программу, восстанавливаем прерывание 2f  
        @deactivate_process title_process_id            
@rrr:
        jmp @rrr                                        ; This is STUB 
        
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

; Процедура вычисления координат вывода строки

get_offset proc
        mov di,79-title_length
        mov ax, 80*2*24
        mul word ptr ds:location
        add di,ax
        xor si,si
        ret
get_offset endp

; Основная подпрограмма

process proc
        push bp
        mov bp, sp
        mov ax, 0b800h
        mov es,ax
        mov cx,title_length
        call get_offset
        xor ax,ax
        mov al,cs:color
        or al,08h
        mov bl,0fh
        div bl
        mov cs:color,ah
        cld
@stdout_title:        
        mov al, byte ptr ds:title_str+[si]
        stosb
        mov al,cs:color
        stosb
        inc si
        loop @stdout_title
        inc cs:color
        delay 10
        mov sp, bp
        pop bp
        ret
process endp
code ends

; Зарезервируем стек

stack segment para stack
        dw 200h dup(0)
stack ends

; Сегмент необходим для вычисления размера программы

zzz segment
zzz ends

end