
; ***************************************************************
; *    ВСПОМОГАТЕЛЬНЫЕ ПРОЦЕДУРЫ                                *
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

; Процедура вывода на экран
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

; Процедура очистки экрана

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


; Процедура вывода символа
; в al - ASCII-выводимый символ

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

; Процедура перехода на кординаты экрана
; bh - видео режимы 0..8
; 0 - B8000h..b8F400h
; 1 - B9000h..BFF400h
; ...
; 7 - BF000h..BF400h
; bp+5 (dh) - Y (строка 0..24)
; bp+4 (dl) - X (столбец 0..78)

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

;Процедура получения текущих кординат экрана
;Выход:
;DH = текущая строка курсора (см. функцию 02H)
;DL = текущий столбец курсора (см. функцию 02H)
;CH = текущая начальная строка экрана, содержащая курсор (см. функцию 01H)
;CL = текущая конечная строка экрана, содержащая курсор (см. функцию 01H)

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

; Писать символ и атрибут в текущей позиции курсора
; Вход: 
; bh - номер видео страницы 
; al - записываемый символ (ASCII код) 
; cx - счетчик (сколько экземпляров символа записать)
; bl - видео атрибут (текстовый режим) или цвет (графический режим)

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

; Перевод ыстроки в число

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

; Перевод числа в строку

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

; Перевод bcd числа в bin

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