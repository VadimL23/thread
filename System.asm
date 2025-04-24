
; ***************************************************************
; *    ��������������� ���������                                *
; *                                                             *
; ***************************************************************
.model SMALL

public write
public clrscr
public gotoxy
public _getxy
public put_char
public atoi
public dump

code segment PARA PUBLIC 'code'
assume cs:code

; ��楤�� �뢮�� �� �࠭
; [bp+10 - source segment]
; [bp+8 - source offset]
; [bp+6 - count of symbols]

dump proc far
        local
        push bp
        mov bp, sp     
        mov ds, [bp+10]
        mov ax, 0b800h
        mov es,ax
        mov di,0
        mov si, word ptr [bp+8] 
        mov cx, [bp+6]
        cld
rep     movsb
        mov bp, sp
        pop bp
        retf 6
dump endp

; ��楤�� ���⪨ �࠭�

clrscr proc
        push bp
        mov bp, sp
        push ax cx ds es si di
        mov ax, 0b800h
        mov ds, ax
        mov es,ax
        mov si,0
        mov di,0
        mov cx, 80*25
        cld
        lodsw
        mov al,0
        dec si
rep     stosw    
        pop di si es ds cx ax
        mov sp, bp
        pop bp
        ret 
clrscr endp


; ��楤�� �뢮�� ᨬ����
; � al - ASCII-�뢮���� ᨬ���

write proc
        push bp
        mov bp, sp
        push bx
        mov ah, 0eh
        mov bh, 0
        int 10h
        pop bx
        mov sp, bp
        pop bp
        ret
write endp

; ��楤�� ���室� �� ��न���� �࠭�
; bh - ����� ०��� 0..8
; 0 - B8000h..b8F400h
; 1 - B9000h..BFF400h
; ...
; 7 - BF000h..BF400h
; bp+5 (dh) - Y (��ப� 0..24)
; bp+4 (dl) - X (�⮫��� 0..78)

gotoxy proc
    local
    push bp
    mov bp, sp
    push dx bx ax
    mov dx, [bp+4]
    mov ah, 02h
    mov bh, 0
    int 10h
    pop ax bx dx
    mov sp, bp
    pop bp
    ret 2
gotoxy endp

;��楤�� ����祭�� ⥪��� ��न��� �࠭�
;��室:
;DH = ⥪��� ��ப� ����� (�. �㭪�� 02H)
;DL = ⥪�騩 �⮫��� ����� (�. �㭪�� 02H)
;CH = ⥪��� ��砫쭠� ��ப� �࠭�, ᮤ�ঠ�� ����� (�. �㭪�� 01H)
;CL = ⥪��� ����筠� ��ப� �࠭�, ᮤ�ঠ�� ����� (�. �㭪�� 01H)

_getxy proc
    push bp
    mov bp,sp
    push cx dx bx
    mov bh,0
    mov ah,3h
    int 10h
    mov ax, dx
    pop bx dx cx
    mov sp, bp
    pop bp
    ret
_getxy endp

; ����� ᨬ��� � ��ਡ�� � ⥪�饩 ����樨 �����
; �室: 
; bh - ����� ����� ��࠭��� 
; al - �����뢠��� ᨬ��� (ASCII ���) 
; cx - ���稪 (᪮�쪮 ������஢ ᨬ���� �������)
; bl - ����� ��ਡ�� (⥪�⮢� ०��) ��� 梥� (����᪨� ०��)

put_char proc
        push bp
        mov bp,sp
        push ax bx
        mov ah, 9
        mov bh, 0 
        ; mov bl, 70h
        int 10h
        pop bx ax
        mov sp,bp
        pop bp
        ret
put_char endp

; ��ॢ�� ���ப� � �᫮

atoi proc
    local
    push bp
    mov bp,sp
    push bx cx di dx 
    mov bx,[bp+4]
    xor cx,cx
    mov cl,[bx]
    inc bx
    mov di,bx
    xor ax,ax
    xor bx,bx
    xor dx,dx
    mov dl,10
@convert:
    mov bl,byte ptr [di]
    cmp bx, 30h
    jl @error
    cmp bx,39h
    jg @error
    sub bx,30h 
    mul dl
    add ax,bx
    inc di
loop @convert
    jmp @done
@error:
    mov ax,-1
@done:
    pop dx di cx bx
    mov sp,bp
    pop bp
    ret 2
atoi endp

; ��ॢ�� �᫠ � ��ப�

hex_to_ascii proc
    push bp
    mov bp, sp
    mov al, [bp+4]
    aam
    or ax, 3030h
    mov sp, bp
    pop bp
    ret 2
hex_to_ascii endp

; ��ॢ�� bcd �᫠ � bin

bcd_to_bin proc
    push bp
    mov sp, bp
    push cx      
    mov cl, al    
    and al, 0Fh    
    shr cl, 4      
    mov ah, 10   
    mul cl
    add al, ah     
    pop cx        
    mov sp, bp
    pop bp
    ret
bcd_to_bin endp
code ends 
end