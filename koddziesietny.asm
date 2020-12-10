; wczytywanie i wy�wietlanie liczb w kodzie dziesi�tnym

dane SEGMENT
cyfrywe dw 5 ; maksymalna liczba cyfr wczytywanej liczby w kodzie dziesi�tnym
cyfrywy dw 5 ; maksymalna liczba cyfr wy�wietlanej liczby w kodzie dziesi�tnym
tab 	dw 7 dup (?) ; tablica do przechowywania wczytanych liczb (w kodzie binarnym)
tabx 	dw 7 dup (?) ;tablica do danych wejsciowych
tablicazapis dw 7 dup (?) ;tablica do danych wejsciowych
ltb  	dw 7 ; liczba wczytywanych element�w tablicy tab
Buf_1 db 5 dup(0) ;bufor do zapami�tania cyfr liczby wyj�ciowej 
NL db 13, 10, '$'
NLx db '    ','$' ;spacje
dane 	ENDS

rozkazy SEGMENT 'CODE' use16 ;segment rozkazu
	ASSUME cs:rozkazy, ds:dane

startuj:
	mov bx, SEG dane
	mov ds, bx	
	mov es, bx	; przypisanie segmentu danych do rejestru es 
			    ; z kt�rym wsp�pracuje rejestr di

	;wczytywanie ltb liczb w kodzie dziesi�tnym i zapisywanie ich wartosci
	;binarnych do element�w tablicy liczb 16 bitowych  tab  (podprogram WczyLiczbe10)
	;wraz z wy�wietlaniem wczytanych element�w tablicy w postaci liczb w kodzie 
	;dziesi�tnym (podprogram WyswLiczbe10)

	mov ax, ltb ;liczba wczytywanych element�w tablicy tab
	mov cx, ax	;liczba obieg�w p�tli wczytywania
	mov si, 0	;rejestr s�u��cy do wyznaczania offsetu kolejnych element�w tab
	mov di, 0 ;indeks do tabx

p_gl:	; P�tla g��wna, pocz�tek 
	;umieszcznie na stosie dw�ch parametr�w wywo�ania podprogramu  WczyLiczbe10
	mov ax, offset tab
	add ax, si	;zwiekszenie offsetu tablicy o 2*indeks elementu tablicy
	push ax		;przekazywanie na stos offsetu elementu tablicy
	mov ax, cyfrywe  ;liczba cyfr wczytywanej liczby w kodzie dziesi�tnym
	push ax			 ;przekazywanie na stos  cyfrywe
	call WczyLiczbe10  ;wczytanie liczby dziesi�tnej do wskazanego elementu tablicy
	pop ax			;zdj�cie ze stosu  
	pop ax			;dw�ch warto�ci
	
	
	;umieszcznie na stosie dw�ch parametr�w wywo�ania podprogramu  WyswLiczbe10
	mov ax, offset tab
	add ax, si	;zwiekszenie offsetu tablicy o 2*indeks elementu tablicy
	push ax		;przekazywanie na stos offsetu elementu tablicy
	mov ax, cyfrywy  ;liczba cyfr wy�wietlanej liczby w kodzie dziesi�tnym
	push ax		;przekazywanie na stos  cyfrywy
	call WyswLiczbe10 ;wy�wietlenie liczby w kodzie dziesi�tnym
	pop ax			;zdj�cie ze stosu  
	pop ax			;dw�ch warto�ci

	inc si		;przyrost si o 2,
	inc si		;gdy� tablica tab zawiera liczby 2 bajtowe
	inc di		;bajty w tabx
	inc di

	call NowaLinia

	loop p_gl  	; P�tla g��wna, koniec
	
	
	call NowaLinia
	call NowaLinia
	call NowaLinia
	mov si, 0
	mov di, 0
	mov ax, ltb
	mov cx, ax
	
	wypisanie:   ;petla wyswietlajaca dane wejsciowe
	cmp di, 15
	ja wyjs
	mov ax, offset tab ;pierwsze trzy dane wejsciowe
	add ax, si	;zwiekszenie offsetu tablicy o 2*indeks elementu tablicy
	push ax		;przekazywanie na stos offsetu elementu tablicy
	mov ax, cyfrywy  ;liczba cyfr wy�wietlanej liczby w kodzie dziesi�tnym
	push ax		;przekazywanie na stos  cyfrywy
	call WyswLiczbe10 ;wy�wietlenie liczby w kodzie dziesi�tnym
	pop ax			;zdj�cie ze stosu  
	pop ax			;dw�ch warto�ci
	jmp przed
	
	wyjs:
	mov ax, offset tabx ;pozostale dane wejsciowe
	add ax, si	;zwiekszenie offsetu tablicy o 2*indeks elementu tablicy
	push ax		;przekazywanie na stos offsetu elementu tablicy
	mov ax, cyfrywy  ;liczba cyfr wy�wietlanej liczby w kodzie dziesi�tnym
	push ax		;przekazywanie na stos  cyfrywy
	call WyswLiczbe10 ;wy�wietlenie liczby w kodzie dziesi�tnym
	pop ax			;zdj�cie ze stosu  
	pop ax			;dw�ch warto�ci
	
	
	
	
	przed:
	inc si 
	inc si
	inc di
	inc di
	call Spacje
	loop wypisanie
	call NowaLinia
	
	

	call koniec

;***********************************************************
;***********************************************************
;***********************************************************

WczyLiczbe10 PROC near
	push bp
	mov bp, sp
	push cx	;zapisanie na stosie wszystkich rejestr�w u�ywanych przez podprogram
	push bx	;
	push dx	;
	push si	;

	mov si, [bp]+6 ;wczytanie do rej. si offset'u zmiennej wyj�ciowej - pierwszy param. procedury
	
	mov ax, 0       	  ;wpisanie warto�ci poczatkowej 0
	mov word PTR [si], ax ;do zmiennej wyj�ciowej

	mov cx, [bp]+4 ;liczba wczytywanych cyfr dziesi�tnych - drugi param. procedury
czn: 
	mov ah, 07H ;wczytanie z klawiatury do rej. AL znaku w kodzie ASCII
	int 21H 	;bez wy�wietlania !!!
	cmp al, 13
	je jest_enter ;skok gdy nacisnieto klawisz Enter
	sub al, 30H ;zamaiana kodu ASCII na wartosc cyfry
	mov bl, al ;przechowanie kolejnej cyfry w rej. BL
	mov bh, 0  ;zerowanie rejestru BH
		   ;algorytm Hornera - wyznaczenie za pomoc� cyfr liczby wejsciowej 
		   ;(dziesi�tnej) jej warto�ci binarnej	
	mov ax, 10 ;mnoznik = 10 bo wczytujemy cyfry w kodzie dziesi�tnym
	mul word PTR [si] 	;mnozenie dotychczas uzyskanego wyniku przez 10, 
				        ;iloczyn zostaje wpisany do rejestr�w DX:AX
	add ax, bx 	        ;dodanie do wyniku mno�enia aktualnie wczytanej cyfry
	mov word PTR [si], ax	;przes�anie wyniku obliczenia do zmiennej wyj�ciowej
	loop czn;
	jmp dalej		;je�li wczytano wszystkie okre�lone przez drugi param. podprogramu
					;omijamy oczyszczenie bufora klawiatury ze znaku enter.

jest_enter:
	mov ah, 06H 	;Oczyszczenie bufora klawiatury ze znaku enter.  
	mov dl, 255		;Zabieg ten jest niezb�dny przy przetwarzaniu wsadowym, gdy  
	int 21H 		;przekierowujemy do programu strumie� danych w postaci pliku tekstowego  
					;z liczbami oddzielonymi enterem (komenda:  piaty.exe < dane.txt). 
					;- zabieg oczyszcenia bufora uzyskamy wstawiaj�c
					;do rej. DL warto�� 255 i wywo�uj�c funkcj� 06H przerwania 21H. 
dalej:	
mov bl,0
dalej4:
	cmp bl,1
	jmp dalej3
	
	cmp tab[di],20000
	ja dalej1
	cmp tab[di],2000
	jb dalej2
	jmp dalej3
	dalej1:
	mov ax,1
	mul tab[di]
	mov bx, 4
	mov dx,0
	div bx
	mov dx,0
	sub ax,500
	mov word PTR [si], ax
	jmp dalej3
	dalej2:
	mov ax,4
	mul tab[di]
	mov dx,0
	add ax,500
	mov word PTR [si], ax
	jmp dalej3
	dalej3:
	pop si 	; przywr�cenie warto�ci poczatkowych wszystkich rejestr�w 
	pop dx	; u�ywanych przez podprogram
	pop bx	; UWAGA! w odwrotnej kolejno�ci ni� by�o w to komendach push!
	pop cx	;
	
	pop bp
	cmp bl,1
	jmp dalej5
	mov bl,1
	jmp dalej4
	dalej5:
	
	ret
WczyLiczbe10 ENDP

;**************************

WyswLiczbe10 PROC near
	push bp
	mov bp, sp
	push cx	;zapisanie na stosie wszystkich rejestr�w u�ywanych przez podprogram
	push bx	;
	push dx	;
	push si	;
	mov si, [bp]+6
		mov cx, 0
	
		mov ax, word PTR [si]
	
		mov bx, 10  
		ptl1: mov dx, 0 ;zamiana binarnej na dziesietna i ASCII
		div bx ;do wyswietlenia
		add dx, 30H 
		mov tablicazapis[di],dx
		push dx 
		inc cx
		cmp ax, 0 
		jnz ptl1 
		ptl2: pop dx 
		mov ah, 2
		int 21H 
		loop ptl2
	pop si 	; przywr�cenie warto�ci poczatkowych wszystkich rejestr�w 
	pop dx	; u�ywanych przez podprogram
	pop bx	; UWAGA! w odwrotnej kolejno�ci!
	pop cx	;

	pop bp
	ret
WyswLiczbe10 ENDP

;**************************

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

;**************************

koniec PROC near
mov dx,offset tablicazapis
	mov ah, 09h
	int 21h
	mov al, 0
	mov ah, 4CH
	int 21H
koniec ENDP

;**************************

rozkazy ENDS


stosik SEGMENT stack
	dw 128 dup(?)
stosik ENDS
END startuj
