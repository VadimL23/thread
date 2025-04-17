.model SMALL

public open
public read
public seek_start

code segment PARA PUBLIC 'code'
assume cs:code

; �室: bp+4 - ०��: 0 - �⥭��
;                     1 - ������
;                     2 - �⥭��/������
;       bp+6 - lpzStr - ��� 䠩��
; ��室: ax - handle
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

; �室: bp+4(cx) - ������⢮ ���� ����� ����⠥��� ������.  
;       bp+6 - handle 
;       DS:DX - ADDR BUFFER
; ��室: cx - ������⢮ ���⠭��� ����
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

; ��楤�� ����樮��஢���� � 䠩��
; �室: ah - 42h
;       al - 00h - ��᮫�⭮� ᬥ饭�� �� ��砫� 䠩��
;            01h - ᬥ饭�� �� ⥪�饩 ����樨
;            02h - ᬥ饭�� �� ���� 䠩��
;       cx - ���訩 ���� ᬥ饭��
;       dx - ����訩 ���� ᬥ饭��
;       bp+4 - handle
; ��室: AX - ��� �訡��, �᫨ ��⠭����� 䫠� ��७�� CF
;             ����訩 ���� ⥪�饩 ����樨, �᫨ 䫠� ��७�� CF ��襭
;        DX	���訩 ���� ⥪�饩 ����樨

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