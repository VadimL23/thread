.model SMALL

public open
public read
public seek_start

code segment PARA PUBLIC 'code'
assume cs:code

; Вход: bp+4 - режим: 0 - чтение
;                     1 - запись
;                     2 - чтение/запись
;       bp+6 - lpzStr - имя файла
; Выход: ax - handle
open proc
        push bp
        mov bp, sp
        push dx ds
        mov ax,[bp+8]
        mov ds,ax
        mov ah, 3dh
        mov al,byte ptr [bp+4]
        mov dx, [bp+6]
        int 21h
        pop ds dx
        mov sp, bp
        pop bp
        ret 6
open endp

; вход: bp+4(cx) - количество байт котрые попытаемся прочитать.  
;       bp+6 - handle 
;       DS:DX - ADDR BUFFER
; выход: cx - количество прочитанных байт
read proc
        push bp
        mov bp, sp 
        push bx cx
        mov cx, [bp+4]
        mov ah, 3fh
        mov bx, [bp+6]
        xor al,al
        int 21h
        pop cx bx
        mov sp,bp
        pop bp
        ret 4
read endp 

; Процедура позиционирования в файле
; Вход: ah - 42h
;       al - 00h - абсолютное смещение от начала файла
;            01h - смещение от текущей позиции
;            02h - смещение от конца файла
;       cx - старший байт смещения
;       dx - младший байт смещения
;       bp+4 - handle
; Выход: AX - Код ошибки, если установлен флаг переноса CF
;             Младший байт текущей позиции, если флаг переноса CF сброшен
;        DX	Старший байт текущей позиции

seek_start proc
        push bp
        mov bp, sp

        push cx dx ax bx
        mov bx, [bp+4] 
        mov ah,42h
        mov cx,0
        mov dx,0
        mov al,00h
        int 21h
        pop bx ax dx cx
        
        mov sp,bp
        pop bp
        ret 2
seek_start endp
code ends
end