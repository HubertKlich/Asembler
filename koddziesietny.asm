; wczytywanie i wyœwietlanie liczb w kodzie dziesiêtnym

dane SEGMENT
cyfrywe dw 5 ; maksymalna liczba cyfr wczytywanej liczby w kodzie dziesiêtnym
cyfrywy dw 5 ; maksymalna liczba cyfr wyœwietlanej liczby w kodzie dziesiêtnym
tab 	dw 7 dup (?) ; tablica do przechowywania wczytanych liczb (w kodzie binarnym)
tabx 	dw 7 dup (?) ;tablica do danych wejsciowych
tablicazapis dw 7 dup (?) ;tablica do danych wejsciowych
ltb  	dw 7 ; liczba wczytywanych elementów tablicy tab
Buf_1 db 5 dup(0) ;bufor do zapamiêtania cyfr liczby wyjœciowej 
NL db 13, 10, '$'
NLx db '    ','$' ;spacje
dane 	ENDS

rozkazy SEGMENT 'CODE' use16 ;segment rozkazu
	ASSUME cs:rozkazy, ds:dane

startuj:
	mov bx, SEG dane
	mov ds, bx	
	mov es, bx	; przypisanie segmentu danych do rejestru es 
			    ; z którym wspó³pracuje rejestr di

	;wczytywanie ltb liczb w kodzie dziesiêtnym i zapisywanie ich wartosci
	;binarnych do elementów tablicy liczb 16 bitowych  tab  (podprogram WczyLiczbe10)
	;wraz z wyœwietlaniem wczytanych elementów tablicy w postaci liczb w kodzie 
	;dziesiêtnym (podprogram WyswLiczbe10)

	mov ax, ltb ;liczba wczytywanych elementów tablicy tab
	mov cx, ax	;liczba obiegów pêtli wczytywania
	mov si, 0	;rejestr s³u¿¹cy do wyznaczania offsetu kolejnych elementów tab
	mov di, 0 ;indeks do tabx

p_gl:	; Pêtla g³ówna, pocz¹tek 
	;umieszcznie na stosie dwóch parametrów wywo³ania podprogramu  WczyLiczbe10
	mov ax, offset tab
	add ax, si	;zwiekszenie offsetu tablicy o 2*indeks elementu tablicy
	push ax		;przekazywanie na stos offsetu elementu tablicy
	mov ax, cyfrywe  ;liczba cyfr wczytywanej liczby w kodzie dziesiêtnym
	push ax			 ;przekazywanie na stos  cyfrywe
	call WczyLiczbe10  ;wczytanie liczby dziesiêtnej do wskazanego elementu tablicy
	pop ax			;zdjêcie ze stosu  
	pop ax			;dwóch wartoœci
	
	
	;umieszcznie na stosie dwóch parametrów wywo³ania podprogramu  WyswLiczbe10
	mov ax, offset tab
	add ax, si	;zwiekszenie offsetu tablicy o 2*indeks elementu tablicy
	push ax		;przekazywanie na stos offsetu elementu tablicy
	mov ax, cyfrywy  ;liczba cyfr wyœwietlanej liczby w kodzie dziesiêtnym
	push ax		;przekazywanie na stos  cyfrywy
	call WyswLiczbe10 ;wyœwietlenie liczby w kodzie dziesiêtnym
	pop ax			;zdjêcie ze stosu  
	pop ax			;dwóch wartoœci

	inc si		;przyrost si o 2,
	inc si		;gdy¿ tablica tab zawiera liczby 2 bajtowe
	inc di		;bajty w tabx
	inc di

	call NowaLinia

	loop p_gl  	; Pêtla g³ówna, koniec
	
	
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
	mov ax, cyfrywy  ;liczba cyfr wyœwietlanej liczby w kodzie dziesiêtnym
	push ax		;przekazywanie na stos  cyfrywy
	call WyswLiczbe10 ;wyœwietlenie liczby w kodzie dziesiêtnym
	pop ax			;zdjêcie ze stosu  
	pop ax			;dwóch wartoœci
	jmp przed
	
	wyjs:
	mov ax, offset tabx ;pozostale dane wejsciowe
	add ax, si	;zwiekszenie offsetu tablicy o 2*indeks elementu tablicy
	push ax		;przekazywanie na stos offsetu elementu tablicy
	mov ax, cyfrywy  ;liczba cyfr wyœwietlanej liczby w kodzie dziesiêtnym
	push ax		;przekazywanie na stos  cyfrywy
	call WyswLiczbe10 ;wyœwietlenie liczby w kodzie dziesiêtnym
	pop ax			;zdjêcie ze stosu  
	pop ax			;dwóch wartoœci
	
	
	
	
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
	push cx	;zapisanie na stosie wszystkich rejestrów u¿ywanych przez podprogram
	push bx	;
	push dx	;
	push si	;

	mov si, [bp]+6 ;wczytanie do rej. si offset'u zmiennej wyjœciowej - pierwszy param. procedury
	
	mov ax, 0       	  ;wpisanie wartoœci poczatkowej 0
	mov word PTR [si], ax ;do zmiennej wyjœciowej

	mov cx, [bp]+4 ;liczba wczytywanych cyfr dziesiêtnych - drugi param. procedury
czn: 
	mov ah, 07H ;wczytanie z klawiatury do rej. AL znaku w kodzie ASCII
	int 21H 	;bez wyœwietlania !!!
	cmp al, 13
	je jest_enter ;skok gdy nacisnieto klawisz Enter
	sub al, 30H ;zamaiana kodu ASCII na wartosc cyfry
	mov bl, al ;przechowanie kolejnej cyfry w rej. BL
	mov bh, 0  ;zerowanie rejestru BH
		   ;algorytm Hornera - wyznaczenie za pomoc¹ cyfr liczby wejsciowej 
		   ;(dziesiêtnej) jej wartoœci binarnej	
	mov ax, 10 ;mnoznik = 10 bo wczytujemy cyfry w kodzie dziesiêtnym
	mul word PTR [si] 	;mnozenie dotychczas uzyskanego wyniku przez 10, 
				        ;iloczyn zostaje wpisany do rejestrów DX:AX
	add ax, bx 	        ;dodanie do wyniku mno¿enia aktualnie wczytanej cyfry
	mov word PTR [si], ax	;przes³anie wyniku obliczenia do zmiennej wyjœciowej
	loop czn;
	jmp dalej		;jeœli wczytano wszystkie okreœlone przez drugi param. podprogramu
					;omijamy oczyszczenie bufora klawiatury ze znaku enter.

jest_enter:
	mov ah, 06H 	;Oczyszczenie bufora klawiatury ze znaku enter.  
	mov dl, 255		;Zabieg ten jest niezbêdny przy przetwarzaniu wsadowym, gdy  
	int 21H 		;przekierowujemy do programu strumieñ danych w postaci pliku tekstowego  
					;z liczbami oddzielonymi enterem (komenda:  piaty.exe < dane.txt). 
					;- zabieg oczyszcenia bufora uzyskamy wstawiaj¹c
					;do rej. DL wartoœæ 255 i wywo³uj¹c funkcjê 06H przerwania 21H. 
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
	pop si 	; przywrócenie wartoœci poczatkowych wszystkich rejestrów 
	pop dx	; u¿ywanych przez podprogram
	pop bx	; UWAGA! w odwrotnej kolejnoœci ni¿ by³o w to komendach push!
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
	push cx	;zapisanie na stosie wszystkich rejestrów u¿ywanych przez podprogram
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
	pop si 	; przywrócenie wartoœci poczatkowych wszystkich rejestrów 
	pop dx	; u¿ywanych przez podprogram
	pop bx	; UWAGA! w odwrotnej kolejnoœci!
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
