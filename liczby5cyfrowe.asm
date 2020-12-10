; wyświetlanie 7 maksymalnie pięciocyfrowych liczb w systemie dziesiętnym podawanych albo liczba po liczbie albo w całości z zastosowaniem techniki "przekierowania danych wejściowych" z pliku tekstowego

dane SEGMENT
cyfrywe dw 5 
cyfrywy dw 5 
tab 	dw 64 dup (?) 
tabx 	dw 64 dup (?) 
ltb  	dw 64 
Buf_1 dw 5 dup(0) 
NL db 13, 10, '$'
NLx db '    ','$' 
dane 	ENDS

rozkazy SEGMENT 'CODE' use16 
	ASSUME cs:rozkazy, ds:dane

startuj:
	mov bx, SEG dane
	mov ds, bx	
	mov es, bx	


	mov ax, ltb 
	mov cx, ax	
	mov si, 0	
	mov di, 0 

p_gl:	
	mov ax, offset tab
	add ax, si	
	push ax		
	mov ax, cyfrywe  
	push ax			
	call WczyLiczbe10  
	pop ax		 
	pop ax			
	
	mov ax, offset tab
	add ax, si	
	push ax		
	mov ax, cyfrywy 
	push ax		
	call WyswLiczbe10 
	pop ax			
	pop ax			

	inc si		
	inc si		
	inc di		
	inc di

	call Spacje

	loop p_gl  
	
	call NowaLinia
	call NowaLinia
	call NowaLinia
	mov si, 0
	mov di, 0
	mov ax, ltb
	mov cx, ax
	wypisanie:   
	mov ax, offset tabx 
	add ax, si	
	push ax		
	mov ax, cyfrywy 
	push ax		
	call WyswLiczbe10
	pop ax			 
	pop ax			
	
	przed:
	inc si 
	inc si
	inc di
	inc di
	call Spacje
	loop wypisanie
	call NowaLinia
	call koniec



WczyLiczbe10 PROC near
	push bp
	mov bp, sp
	push cx
	push bx	
	push dx	
	push si	
	

	mov si, [bp]+6
	
	mov ax, 0       	  
	mov word PTR [si], ax 

	mov cx, [bp]+4 
czn: 
	mov ah, 07H 
	int 21H 	
	cmp al, 13
	je jest_enter 
	sub al, 30H 
	mov bl, al 
	mov bh, 0
	mov ax, 10
	mul word PTR [si] 	 
	add ax, bx 	       
	mov word PTR [si], ax	
	mov tabx [di], ax
	loop czn;
	
	jmp dalej	
jest_enter:
	mov ah, 06H 	
	mov dl, 255		 
	int 21H 		
dalej:
	
	cmp word PTR[si], 20000
	ja duza
	cmp word PTR[si], 2000
	jb mala
	;mov ax, 0
	jmp dalej1
	
	duza:
	mov ax, word PTR[si]
	mov dx, 0
	mov bx, 4
	div bx
	mov dx, 0
	sub ax, 500
	mov word PTR[si], ax
	jmp dalej1
	mala:
	mov ax, 4
	mov bx, word PTR [si]
	mul bx
	add ax, 500
	mov word PTR[si], ax
	jmp dalej1
	
	dalej1:
	pop si 	
	pop dx	
	pop bx	
	pop cx	

	pop bp
	ret
WczyLiczbe10 ENDP
WyswLiczbe10 PROC near
	push bp
	mov bp, sp
	push cx	
	push bx	
	push dx	
	push si	

		mov si, [bp]+6
		mov cx, 0
	
		mov ax, word PTR [si]
	
		mov bx, 10  
		ptl1: mov dx, 0 
		div bx 
		add dx, 30H 
		push dx 
		inc cx
		cmp ax, 0 
		jnz ptl1 
		ptl2: pop dx 
		mov ah, 2
		int 21H 
		loop ptl2
		
		
	pop si 
	pop dx	
	pop bx	
	pop cx	

	pop bp
	ret
WyswLiczbe10 ENDP
Spacje PROC near
	push dx
	mov dx, offset NLx
	mov ah, 9 
	int 21H
	pop dx
	ret
Spacje ENDP

NowaLinia PROC near
	push dx
	mov dx, offset NL
	mov ah, 9 
	int 21H
	pop dx
	ret
NowaLinia ENDP
koniec PROC near
	mov al, 0
	mov ah, 4CH
	int 21H
koniec ENDP
rozkazy ENDS


stosik SEGMENT stack
	dw 128 dup(?)
stosik ENDS
END startuj
