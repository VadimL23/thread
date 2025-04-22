; ***************************************************************
; *    ПОДПРОГРАММА ОТРИСОВКИ ОКНА ПРОГРАММЫ                    *
; *                                                             *
; ***************************************************************

.model SMALL

public draw_window
public TWind

include struc.inc

EXTRN write:PROC, gotoxy:PROC

data segment PARA PUBLIC 'data'

   ;------ screen symbols--------
   ; Символы для отрисовки рамки окна
    leftTop equ 201
    rightTop equ 187
    horizontal equ 205
    vertical equ 186
    leftBottom equ 200
    rightBottom equ 188
   ; ******  symbols ********** 
  
    color equ 01h                               ; Цвет рамки программы
    video equ 0b800h                            ; Видео сегмент

    coord dw 0                                  ; Адрес видео
          dw 0b800h
    wnd dd 0                                    ; Описатель окна
    height dw 0                                 ; Хранит высоту окна
    width dw 0                                  ; Хранит ширину окна
data ends


code segment PARA PUBLIC 'code'
assume cs:code, ds:data

; Процедура рисует рамку окна программы
; Вход   BP+8 - SEG STRUCT
;        bp+6 - offset struct

draw_window proc far
        push bp
        mov bp,sp
        push ax cx dx bx es di ds       
        mov ax, seg coord
        mov ds, ax
        mov ax, word ptr [bp+8]
        mov word ptr [wnd+2],ax
        mov ax,word ptr [bp+6]
        mov  word ptr [wnd],ax
        les bx, dword ptr wnd
        mov dh,es:[bx].TWind.left_top_row
        mov dl,es:[bx].TWind.left_top_col
        mov coord,dx
        les dx,dword ptr coord 
        xor ax,ax
        mov al,160
        mul dh
        xor dh,dh
        shl dl,1
        add ax,dx
        mov di,ax    
        mov al, leftTop
        mov ah, color
        stosw
        mov word ptr coord,di
        xor cx,cx
        les bx, dword ptr wnd
        mov cl, byte ptr es:[bx].TWind.right_bottom_col
        sub cl, byte ptr es:[bx].TWind.left_top_col
        mov width, cx
        dec cl
        les di, dword ptr coord
        mov al, horizontal
rep     stosw
        mov al, rightTop
        stosw
        mov word ptr coord,di
        xor cx,cx
        les bx, dword ptr wnd
        mov cl,byte ptr es:[bx].TWind.right_bottom_row
        sub cl, byte ptr es:[bx].TWind.left_top_row
        mov height, cx
        les di, dword ptr coord
        mov al, vertical
        dec cl
 @left_vertical:
        add di,158
        stosw
        loop @left_vertical
        add di,158
        mov al, rightBottom
        stosw
        mov cx,width
        sub di,width
        sub di,width
        dec cx        
        mov al,horizontal    
rep     stosw
        sub di,width
        sub di,width
        mov al,leftBottom
        stosw
        mov al,vertical
        mov cx, height
        dec cx
@right_vertical:
        sub di,162
        stosw
        loop @right_vertical
        pop ds
        pop di
        pop es
        pop bx
        pop dx
        pop cx
        pop ax    
        mov sp,bp
        pop bp
        retf 4
draw_window endp

code ends
end