    
; ***************************************************************
; *    СТРУКТУРЫ ПРОГРАММЫ                                      *
; *                                                             *
; ***************************************************************
    
    bios_process_linked_seg equ 40h
    bios_process_linked_offs equ 0f0h
    bios_process_linked_offs_second equ 0f4h
    
    ; Структура описатель окна, хранит внешние и внутренние кординаты

    TWind STRUC
          left_top_row db 0
          left_top_col db 0
          right_bottom_row db 0
          right_bottom_col db 0
          inner_left_top_row db 0
          inner_left_top_col db 0
          inner_right_bottom_row db 0
          inner_right_bottom_col db 0
    TWind ends

    ; Структура описатель потока, хранит контекст потока в таблице процессов

    TThread STRUC
      r_flags dw 0
      r_cs dw 0 
      r_ip dw 0 
      r_ax dw 0
      r_bx dw 0
      r_cx dw 0
      r_dx dw 0
      r_si dw 0
      r_di dw 0
      r_ds dw 0                        
      r_es dw 0
      r_bp dw 0
      r_sp dw 0
      r_ss dw 0
      status db          
    TThread ends

; 14 - words + 1 byte Длина структуры