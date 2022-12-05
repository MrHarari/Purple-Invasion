; -------------------------------------------------------------------------------------------------------------
; Read a BMP file 320x200 and print it to screen
; Author: Barak Gonen, 2014
; Credit: Diego Escala, www.ece.msstate.edu/~reese/EE3724/labs/lab9/bitmap.asm
; -------------------------------------------------------------------------------------------------------------
IDEAL
MODEL small
STACK 100h
DATASEG
	filename dw offset playerFilename
	playerFilename db 'BS.bmp', 0
	enemyFilename db 'BSE.bmp', 0
	linkFilename db 'BSL.bmp', 0
	backgroundFilename db 'BG1.bmp', 0
	blackFilename db 'BK.bmp', 0
	hpFilename db 'EH.bmp', 0
	slash db 'Slash.bmp', 0
	screen db 'SS.bmp', 0
	enemyExplosion db 'EE.bmp', 0
	openPassageFilename db 'OP.bmp', 0
	youWinFilename db 'YW.bmp', 0
	lastCharInput db 'B'
	filehandle dw ?
	Header db 54 dup (0)
	Palette db 256*4 dup (0)
	ScrLine db 320 dup (0)
	ErrorMsg db 'Error', 13, 10 ,'$'
	Height dw 200
	Wid dw 320
	imageXLocation dw 0
	imageYLocation dw 0
	temp dw 0
	hp dw 3
	bossHP dw 6
	invincibility db 0
	attackCooldown db 0
	bossCooldown dw 200
	numOfEnemies db 0
	enemies dw 0
CODESEG

proc PrintImage
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push [imageXLocation]
	push [imageYLocation]
	
	dec [imageYLocation]
	call OpenFile
    call ReadHeader
    call ReadPalette
    call CopyPal
    call CopyBitmap
	call CloseFile
	
	pop [imageYLocation]
	pop [imageXLocation]
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp PrintImage
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc CloseFile
  mov  ah, 3Eh
  mov  bx, [filehandle]
  int  21h
  ret
endp CloseFile
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc OpenFile
	
	; Open file
	
	mov ah, 3Dh
	xor al, al
	mov dx, [filename]
	int 21h
	
	jc openerror
	mov [filehandle], ax
	
	ret
openerror:

	;mov dx, offset ErrorMsg
	;mov ah, 9h
	;int	 21h
	
	ret
endp OpenFile
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc ReadHeader

    ; Read BMP file header, 54 bytes

    mov ah,3fh
    mov bx, [filehandle]
    mov cx,54
    mov dx,offset Header
    int 21h
    ret
endp ReadHeader
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc ReadPalette

    ; Read BMP file color palette, 256 colors * 4 bytes (400h)

    mov ah,3fh
    mov cx,400h
    mov dx,offset Palette
    int 21h
    ret
endp ReadPalette
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc CopyPal

    ; Copy the colors palette to the video memory
    ; The number of the first color should be sent to port 3C8h
    ; The palette is sent to port 3C9h

    mov si,offset Palette
    mov cx,256
    mov dx,3C8h
    mov al,0

    ; Copy starting color to port 3C8h

    out dx,al

    ; Copy palette itself to port 3C9h

    inc dx
    PalLoop:

    ; Note: Colors in a BMP file are saved as BGR values rather than RGB.

    mov al,[si+2] ; Get red value.
    shr al,2 ; Max. is 255, but video palette maximal

    ; value is 63. Therefore dividing by 4.

    out dx,al ; Send it.
    mov al,[si+1] ; Get green value.
    shr al,2
    out dx,al ; Send it.
    mov al,[si] ; Get blue value.
    shr al,2
    out dx,al ; Send it.
    add si,4 ; Point to next color.

    ; (There is a null chr. after every color.)

    loop PalLoop
    ret
endp CopyPal
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc CopyBitmap

    ; BMP graphics are saved upside-down.
    ; Read the graphic line by line (200 lines in VGA format),
    ; displaying the lines from bottom to top.

    mov ax, 0A000h
    mov es, ax
    mov cx,[Height]
PrintBMPLoop:
    push cx

    ; di = cx*320, point to the correct screen line

    mov di,cx
    shl cx,6
    shl di,8
    add di,cx
	
	add di,[imageXLocation]
	mov ax,320
	mul[imageYLocation]
	add di, ax
	
    ; Read one line

    mov ah,3fh
    mov cx,[Wid]
    mov dx,offset ScrLine
    int 21h

    ; Copy one line into video memory

    cld 

    ; Clear direction flag, for movsb

    mov cx,[Wid]
    mov si,offset ScrLine
    rep movsb 

    ; Copy line to the screen
    ;rep movsb is same as the following code:
    ;mov es:di, ds:si
    ;inc si
    ;inc di
    ;dec cx
    ;loop until cx=0

    pop cx
    loop PrintBMPLoop
    ret
endp CopyBitmap
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc Input
	
	mov ah,1
	int 16h
	jz notEnterd
	
	mov ah,0
	int 16h
	
	cmp al, 20h
	je slasha
	
	push bx
	mov bx, offset slash
	mov [byte ptr bx], 'A'
	pop bx
	
	
	cmp ah,48h
	je up
	
	cmp ah,4Bh	
	je left
	
	cmp ah,4Dh
	je right
	
	cmp ah,50h
	je down
	
	cmp ah,01h
	je Backout

	mov [byte ptr bx], ' '
	call WaitTime
	ret
	
slasha:
	
	push bx
	mov bx, offset slash
	mov [byte ptr bx], 'S'
	pop bx
	
notEnterd:

	mov [byte ptr bx], ' '
	call WaitTime
	ret
	
up:
	mov [byte ptr bx], 'B'
	mov [lastCharInput], 'B'
	ret

down:
	mov [byte ptr bx], 'F'
	mov [lastCharInput], 'F'
	ret
	
left:
	mov [byte ptr bx], 'L'
	mov [lastCharInput], 'L'
	ret

right:
	mov [byte ptr bx], 'R'
	mov [lastCharInput], 'R'
	ret
Backout:
	mov [numOfEnemies], 0
	mov sp, 0FEh
	call StartGame
endp Input	
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc LeftLeg
	
	mov [byte ptr bx + 1], 'L'
	add [imageYLocation], si
	add [imageXLocation], di
	call PrintImage
	
	ret
endp LeftLeg
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc Stand
	
	mov [byte ptr bx + 1], 'S'
	add [imageYLocation], si
	add [imageXLocation], di
	call PrintImage
	
	ret
endp Stand
;===========================================================================================================================================================
;
;===========================================================================================================================================================	
proc RightLeg
	
	mov [byte ptr bx + 1], 'R'
	add [imageYLocation], si
	add [imageXLocation], di
	call PrintImage
	ret
endp RightLeg
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc PrintAnimation
	cmp [slash], 'S'
	jne SlashNotPressed
	call SlashAppear
	ret
	
SlashNotPressed:
	
	cmp [byte ptr bx], 'B'
	jne NotUp
	mov si, -1 ; y
	mov di, 0 ; x
	mov dx, -1
	mov cx, 0
	
	jmp DoneCheckingRotation
NotUp:
	cmp [byte ptr bx], 'F'
	jne NotDown
	mov si, 1
	mov di, 0
	mov dx, 17
	mov cx, 0
	
	jmp DoneCheckingRotation
NotDown:
	cmp [byte ptr bx], 'L'
	jne NotLeft
	mov si, 0
	mov di, -1
	mov dx, 0
	mov cx, -1
	
	jmp DoneCheckingRotation
NotLeft:
	cmp [byte ptr bx], 'R'
	jne NotRight
	mov si, 0
	mov di, 1
	mov dx, 0
	mov cx, 16
	
	jmp DoneCheckingRotation
NotRight:
	
	ret
DoneCheckingRotation:
	push bp
	mov bp, sp
	
	
	call CheckLine
	
	call LeftLeg
	call WaitTime
	
	call CheckLine
	
	call Stand
	
	call CheckLine
	
	call RightLeg
	call WaitTime
	
	call CheckLine
	
	call Stand
	
	call ClearKeyBoardBuffer
	pop bp
	
	ret
endp PrintAnimation
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc SlashAppear
	cmp [slash], 'S'
	je SlashPressed
	ret
SlashPressed:
	push [imageXLocation]
	push [imageYLocation]
	
	; push ax
	; mov dx, ax
	; add dx, '0'
	; mov ah,2
	; int 21h
	; pop ax
	
	call CheckBox
	
	
	;mov ax, dx
	cmp [temp], 0
	jne NotSlRight
	
	cmp [lastCharInput], 'B'
	jne NotSlUp
	sub [imageYLocation], 17
	
	jmp DoneCheckingSlashPlacement
NotSlUp:
	cmp [lastCharInput], 'F'
	jne NotSlDown
	add [imageYLocation], 17
	
	jmp DoneCheckingSlashPlacement
NotSlDown:
	cmp [lastCharInput], 'L'
	jne NotSlLeft
	sub [imageXLocation], 16
	
	jmp DoneCheckingSlashPlacement
NotSlLeft:
	cmp [lastCharInput], 'R'
	jne NotSlRight
	add [imageXLocation], 16
	
	jmp DoneCheckingSlashPlacement
NotSlRight:
	call CheckHit
	mov [slash], 'A'
	pop [imageYLocation]
	pop [imageXLocation]
	ret
DoneCheckingSlashPlacement:
	
	cmp [attackCooldown], 0
	jnz DoneExplosion
	mov [filename], offset slash
	call PrintImage
	mov [filename], offset blackFilename
	mov dx, [imageYLocation]
	mov cx, [imageXLocation]
	pop [imageYLocation]
	pop [imageXLocation]
	call WaitTime
	call WaitTime
	call WaitTime
	call WaitTime
	push [imageXLocation]
	push [imageYLocation]
	mov [imageXLocation], cx
	mov [imageYLocation], dx
	call PrintImage
	mov [filename], bx
	mov [attackCooldown], 45
DoneExplosion:
	mov [slash], 'A'
	call ClearKeyBoardBuffer
	pop [imageYLocation]
	pop [imageXLocation]
	ret
endp SlashAppear
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc CheckHit
	push dx
	push bx
	push cx
	
	cmp [lastCharInput], 'B'
	jne NotCheckUp
	sub [imageYLocation], 17
	
	jmp DoneCheckWhereToCheck
NotCheckUp:
	cmp [lastCharInput], 'F'
	jne NotCheckDown
	add [imageYLocation], 17
	
	jmp DoneCheckWhereToCheck
NotCheckDown:
	cmp [lastCharInput], 'L'
	jne NotCheckLeft
	sub [imageXLocation], 16
	
	jmp DoneCheckWhereToCheck
NotCheckLeft:
	add [imageXLocation], 16
DoneCheckWhereToCheck:

	call CheckHitBoss
	
	xor cx, cx
	mov cl, [numOfEnemies]
	cmp cl, 0
	je DoneCheckHitLoop
	
	mov bx, offset enemies
	
CheckHitLoop:


	mov ax, [imageXLocation]
	sub ax, [bx]
	cmp ax, 17
	jge DoneCheckingEnemy
	
	cmp ax, -17
	jle DoneCheckingEnemy
	
	mov ax, [imageYLocation]
	sub ax, [bx + 2]
	
	cmp ax, 17
	jge DoneCheckingEnemy
	
	cmp ax, -17
	jle DoneCheckingEnemy
	
	call DeleteEnemy
	jmp DoneCheckHitLoop

DoneCheckingEnemy:

	add bx, 4
	loop CheckHitLoop
	
DoneCheckHitLoop:

	pop cx
	pop bx
	pop dx
	ret
endp CheckHit
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc CheckHitBoss
	
	mov [temp], 0
	
	cmp [attackCooldown], 0
	jne DidntHitBoss
	
	cmp [backgroundFilename + 2], '2'
	jne DidntHitBoss
	
	cmp [imageXLocation], 187
	jae DidntHitBoss
	
	cmp [imageXLocation], 100
	jbe DidntHitBoss
	
	cmp [imageYLocation], 97
	jae DidntHitBoss
	
	cmp [imageYLocation], 55
	jbe DidntHitBoss
	
	call DecBossHP
	mov [temp], 1
	mov [attackCooldown], 17
	
DidntHitBoss:
	ret
endp CheckHitBoss
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc DeleteEnemy
	push [imageXLocation]
	push [imageYLocation]
	push [filename]
	push ax
	push bx
	push si
	
	
	cmp [attackCooldown], 0
	jne DoneDeletingEnemy
	
	mov [attackCooldown], 17
	
	dec [numOfEnemies]
	
	push [bx]
	pop [imageXLocation]
	push [bx + 2]
	pop [imageYLocation]
	
	mov [filename], offset enemyExplosion
	call PrintImage
	
	mov cx, 0    ;HIGH WORD.
	mov dx, 05000h ;LOW WORD.
	mov ah, 86h    ;WAIT.
	int 15h
	
	xor ax, ax
	mov al, [numOfEnemies]
	mov si, 4
	mul si
	add ax, offset enemies
	
	mov si, ax
	
	mov [filename], offset blackFilename
	call PrintImage
	
	push [si]
	pop [bx]
	
	push [si + 2]
	pop [bx + 2]
	
DoneDeletingEnemy:
	
	
	pop si
	pop bx
	pop ax
	pop [filename]
	pop [imageYLocation]
	pop [imageXLocation]
	ret
endp DeleteEnemy
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc WaitTime
	push cx
	push dx
	push ax
	
	mov cx, 0    ;HIGH WORD.
	mov dx, 03000h ;LOW WORD.
	mov ah, 86h    ;WAIT.
	int 15h
	
	call EnemyTurn
	
	pop ax
	pop dx
	pop cx

	ret 
endp WaitTime
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc DecHP
	push [filename]
	push [Height]
	push [Wid]
	push [imageXLocation]
	push [imageYLocation]
	
	dec [hp]
	mov [Height], 17
	mov [Wid], 16
	mov ax, 16
	mul [hp]
	mov [imageXLocation], ax
	mov [imageYLocation], 0
	mov [filename], offset hpFilename
	call PrintImage
	
	cmp [hp], 0 
	jne NotLost
	
	pop [imageYLocation]
	pop [imageXLocation]
	pop [Wid]
	pop [Height]
	pop [filename]
	mov sp, 0FEh
	call StartGame
NotLost:

	pop [imageYLocation]
	pop [imageXLocation]
	pop [Wid]
	pop [Height]
	pop [filename]
	ret
endp DecHP
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc DecBossHP
	push [filename]
	push [Height]
	push [Wid]
	push [imageXLocation]
	push [imageYLocation]
	
	mov [Height], 17
	mov [Wid], 16
	mov ax, 16
	mul [bossHP]
	mov bx, 319
	sub bx, ax
	mov [imageXLocation], bx
	mov [imageYLocation], 0
	mov [filename], offset hpFilename
	call PrintImage
	dec [bossHP]
	
	cmp [bossHP], 0 
	jne NotWon
	
	pop [imageYLocation]
	pop [imageXLocation]
	pop [Wid]
	pop [Height]
	pop [filename]
	call YouWin
NotWon:

	pop [imageYLocation]
	pop [imageXLocation]
	pop [Wid]
	pop [Height]
	pop [filename]
	ret
endp DecBossHP
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc YouWin
	mov [filename], offset youWinFilename
	mov [imageXLocation], 0
	mov [imageYLocation], 0
	mov [Height], 200
	mov [Wid], 320
	call PrintImage
	mov ah, 0
	int 16h 
	mov sp, 0FEh
	call StartGame
	ret
endp YouWin
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc StartGame
	mov [temp], 0
	mov [lastCharInput], 'B'
	mov [playerFilename], 'B'
	mov [invincibility], 0
	mov [attackCooldown], 0
	mov [bossCooldown], 200
	mov [bossHP], 6
	mov [backgroundFilename + 2], '1'
	mov [imageXLocation], 0
	mov [imageYLocation], 0
	mov [Height], 200
	mov [Wid], 320
	mov [filename], offset screen
	mov [numOfEnemies], 0
	mov [hp], 3
	
StartGameLoop:
	call PrintImage
	
	mov ah, 0
	int 16h 
	cmp ah, 1
	jne NotEnded
	
	ret
	
NotEnded:
	cmp al, 20h
	jne StartGameLoop
	
	call PrintBackground
	mov [Wid], 16
	mov [Height], 17
	mov [imageXLocation], 300
	mov [imageYLocation], 80
	call AddEnemy
	mov [imageXLocation], 70
	mov [imageYLocation], 30
	call AddEnemy
	mov [imageXLocation], 30
	mov [imageYLocation], 100
	call AddEnemy
	mov [imageXLocation], 140
	mov [imageYLocation], 150
	mov bx, offset playerFilename
	mov [filename], offset playerFilename
	mov [byte ptr bx], 'B'
	mov [byte ptr bx + 1], 'S'
	call PrintImage
	call Play
	ret
endp StartGame
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc EnemyTurn
	push [imageXLocation]
	push [imageYLocation]
	push [filename]
	push si
	push di
	push dx
	push cx
	push bx
	push ax
	
	xor cx, cx
	mov bx, offset enemies
	mov cl, [numOfEnemies]
	cmp cx, 0
	je NoEnemies
	
EnemyTurns:
	
	call EnemyMovement
	add bx, 4
	
	loop EnemyTurns
NoEnemies:

	pop ax
	pop bx
	pop cx
	pop dx
	pop di
	pop si
	pop [filename]
	pop [imageYLocation]
	pop [imageXLocation]
	ret
endp EnemyTurn
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc AddEnemy
	push ax
	push bx
	push [filename]
	
	mov ax, 4
	mul [numOfEnemies]
	add ax, offset enemies
	
	mov bx, ax
	
	inc [numOfEnemies]
	
	push [imageXLocation]
	pop [bx]
	push [imageYLocation]
	pop [bx + 2]
	mov [filename], offset enemyFilename

	
	call PrintImage
	
	pop [filename]
	pop bx
	pop ax
	ret
endp AddEnemy
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc EnemyMovement
	push si
	push di
	push dx
	push cx
	push bx
	push ax
	push [imageXLocation]
	push [imageYLocation]
	
	xor si, si
	xor di, di

	mov ax, [imageXLocation]
	cmp ax, [bx]
	jae NotMoveLeft
	mov si, 0
	mov di, -1
	mov dx, 0
	mov cx, -2
	
	jmp DoneHorizontal
NotMoveLeft:
	cmp ax, [bx]
	je NotMove1
	mov si, 0
	mov di, 1
	mov dx, 0
	mov cx, 17
	
	jmp DoneHorizontal
NotMove1:
	mov si, 0
	mov di, 0
	mov dx, 0
	mov cx, 0
	
DoneHorizontal:
	
	
	
	
	mov ax, [imageYLocation]
	
	push [bx]
	pop [imageXLocation]
	push [bx + 2]
	pop [imageYLocation]
	
	call CheckLine
	cmp si, 0
	jne MoveEnemy
	
	cmp di, 0
	jne MoveEnemy
	
	
	cmp ax, [bx + 2]
	jae NotMoveUp
	mov si, -1
	mov di, 0
	mov dx, -2
	mov cx, 0
	
	jmp DoneVertical
NotMoveUp:
	cmp ax, [bx + 2]
	je DoneVertical
	mov si, 1
	mov di, 0
	mov dx, 18
	mov cx, 0
	
DoneVertical:
	
	push [bx]
	pop [imageXLocation]
	push [bx + 2]
	pop [imageYLocation]
	
	call CheckLine
	cmp si, 0
	jne MoveEnemy
	
	cmp di, 0
	jne MoveEnemy
	
	pop [imageYLocation]
	pop [imageXLocation]
	
	call EnemyAttack
	
	pop ax
	pop bx
	pop cx
	pop dx
	pop di
	pop si
	ret
MoveEnemy:
	
	
	add [bx], di
	add [bx + 2], si
	
	push [bx]
	pop [imageXLocation]
	push [bx + 2]
	pop [imageYLocation]
	
	mov [filename], offset enemyFilename
	
	call PrintImage
	
	pop [imageYLocation]
	pop [imageXLocation]
	pop ax
	pop bx
	pop cx
	pop dx
	pop di
	pop si
	ret
endp EnemyMovement
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc EnemyAttack
	
	cmp [invincibility], 0
	jnz DoneCheckingPlayer
	
	mov ax, [imageXLocation]
	sub ax, [bx]
	cmp ax, 18
	jge DoneCheckingPlayer
	
	cmp ax, -17
	jle DoneCheckingPlayer
	
	mov ax, [imageYLocation]
	sub ax, [bx + 2]
	
	cmp ax, 18
	jge DoneCheckingPlayer
	
	cmp ax, -17
	jle DoneCheckingPlayer
	
	call DecHP
	mov [invincibility], 35
	
DoneCheckingPlayer:
	
	
	ret
endp EnemyAttack
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc BossTurn
	push [imageXLocation]
	push [imageYLocation]
	push si
	push di
	push dx
	push cx
	
	mov si, 1
	mov di, 0
	mov dx, 0
	mov cx, 0
	
	cmp [bossCooldown], 0
	jne SkipEnemy3
	
	mov si, 1
	mov [imageXLocation], 270
	mov [imageYLocation], 62
	call CheckEnemyNear
	
	cmp [temp], 0
	jne SkipEnemy1
	
	mov [bossCooldown], 300
	call AddEnemy
SkipEnemy1:

	mov si, 1
	mov [imageXLocation], 150
	mov [imageYLocation], 100
	call CheckEnemyNear
	
	
	cmp [temp], 0
	jne SkipEnemy2
	
	mov [bossCooldown], 300
	call AddEnemy
SkipEnemy2:

	mov si, 1
	mov [imageXLocation], 50
	mov [imageYLocation], 62
	call CheckEnemyNear
	
	cmp [temp], 0
	jne SkipEnemy3
	
	mov [bossCooldown], 300
	call AddEnemy
SkipEnemy3:
	
	pop cx
	pop dx
	pop di
	pop si
	pop [imageYLocation]
	pop [imageXLocation]
	ret
endp BossTurn
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc CheckEnemyNear
	mov [temp], 17
	
	cmp si, 0
	jne CheckEnemyNearLoop
	inc [temp]

CheckEnemyNearLoop:
	cmp [temp], 0
	je DoneCheckEnemyNearLoop
	push [temp]
	
	call CheckLine
	
	pop [temp]
	cmp si, 0
	jne NotDoneCheckEnemyNearLoop
	cmp di, 0
	jne NotDoneCheckEnemyNearLoop
	
	mov [temp], 1
	jmp DoneCheckEnemyNearLoop
	
NotDoneCheckEnemyNearLoop:
	
	add dx, si
	add cx, di
	
	dec [temp]
	jmp CheckEnemyNearLoop
DoneCheckEnemyNearLoop:
	ret
endp CheckEnemyNear
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc CheckLine
	
	push bx
	push cx	
	push dx
	push ax
	
	add dx, [imageYLocation]
	add cx, [imageXLocation]
	xor bx, bx
	
	cmp si, 0
	jne CheckLineLength
	mov ax, 17
	
	jmp EndCheckLineLength
CheckLineLength:
	mov ax, 16
	
EndCheckLineLength:
	
CheckLineLoop:
	cmp ax, 0
	je DoneCheckLine
	push ax
	
	
    mov ah,0dh
    int 10h
	
	call CheckLineHelp
	
	cmp al, 0
	je fine
	xor si, si
	xor di, di
fine:
	
	pop ax
	dec ax
	jmp CheckLineLoop
DoneCheckLine:
	
	pop ax
	pop dx
	pop cx
	pop bx
	ret
endp CheckLine
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc CheckBox
	push [imageXLocation]
	push [imageYLocation]
	
	cmp [lastCharInput], 'B'
	jne NotCheckBoxUp
	mov si, -1 ; y
	mov di, 0 ; x
	mov dx, -3
	mov cx, 0
	
	jmp DoneCheckBoxRotation
NotCheckBoxUp:
	cmp [lastCharInput], 'F'
	jne NotCheckBoxDown
	mov si, 1
	mov di, 0
	mov dx, 19
	mov cx, 0
	
	jmp DoneCheckBoxRotation
NotCheckBoxDown:
	cmp [lastCharInput], 'L'
	jne NotCheckBoxLeft
	mov si, 0
	mov di, -1
	mov dx, 0
	mov cx, -3
	
	jmp DoneCheckBoxRotation
NotCheckBoxLeft:
	mov si, 0
	mov di, 1
	mov dx, 0
	mov cx, 18
	
DoneCheckBoxRotation:

	mov [temp], 17
	
	cmp si, 0
	jne CheckBoxLoop
	inc [temp]

CheckBoxLoop:
	cmp [temp], 0
	je DoneCheckBoxLoop
	push [temp]
	
	call CheckLine
	
	pop [temp]
	cmp si, 0
	jne NotDoneCheckBoxLoop
	cmp di, 0
	jne NotDoneCheckBoxLoop
	
	mov [temp], 1
	jmp DoneCheckBoxLoop
	
NotDoneCheckBoxLoop:
	
	add dx, si
	add cx, di
	
	dec [temp]
	jmp CheckBoxLoop
DoneCheckBoxLoop:
	
	
	
	
	pop [imageYLocation]
	pop [imageXLocation]
	ret
endp CheckBox
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc CheckLineHelp
	
	cmp si, 0
	jne hey1
	add dx, 1
	
	ret
hey1:
	add cx, 1
	
	ret
endp CheckLineHelp
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc Play
LoopFirstStage:
	mov bx, offset playerFilename
	
	call Input
	call PrintAnimation
	
	call CheckInvincility

	call CheckAttackCooldown

	cmp [numOfEnemies], 0
	jne DidntKillAll
	
	call OpenPassage
	
	cmp [imageYLocation], 18
	jnb  DidntKillAll

	jmp SecondStage

DidntKillAll:

	jmp LoopFirstStage
	
SecondStage:
	
	mov [backgroundFilename + 2], '2'
	call PrintBackground
	mov [imageYLocation], 183
	mov [filename], offset playerFilename
	call PrintImage
	mov [hp], 3
	
LoopSecondStage:
	mov bx, offset playerFilename
	
	call Input
	call BossTurn
	call PrintAnimation
	
	call CheckInvincility

	call CheckAttackCooldown
	
	call CheckBossCooldown
	
	jmp LoopSecondStage
	ret
endp Play
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc CheckAttackCooldown

	cmp [attackCooldown], 0
	je AttackCooldownZero
	
	dec [attackCooldown]
	
AttackCooldownZero:

	ret
endp CheckAttackCooldown
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc CheckBossCooldown

	cmp [bossCooldown], 0
	je BossCooldownZero
	
	dec [bossCooldown]
	
BossCooldownZero:

	ret
endp CheckBossCooldown
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc CheckInvincility

	cmp [invincibility], 0
	je InvincilityZero
	
	dec [invincibility]
	
InvincilityZero:

	ret
endp CheckInvincility
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc PrintBackground
	push [filename]
	push [Height]
	push [Wid]
	push [imageXLocation]
	push [imageYLocation]
	
	mov [filename], offset backgroundFilename
	mov [Height], 200
	mov [Wid], 320
	mov [imageXLocation], 0
	mov [imageYLocation], 0
	
	call PrintImage
	
	pop [imageYLocation]
	pop [imageXLocation]
	pop [Wid]
	pop [Height]
	pop [filename]
	ret
endp PrintBackground
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc OpenPassage
	push [filename]
	push [Height]
	push [Wid]
	push [imageXLocation]
	push [imageYLocation]
	
	mov [filename], offset blackFilename
	mov [Height], 18
	mov [Wid], 43
	mov [imageXLocation], 117
	mov [imageYLocation], 0
	
	call PrintImage
	
	pop [imageYLocation]
	pop [imageXLocation]
	pop [Wid]
	pop [Height]
	pop [filename]
	ret
endp OpenPassage
;===========================================================================================================================================================
;
;===========================================================================================================================================================
proc ClearKeyBoardBuffer
	push		ax
	push		es
	mov		ax, 0000h
	mov		es, ax
	mov		[word ptr es:041ah], 041eh
	mov		[word ptr es:041ch], 041eh				; Clears keyboard buffer
	pop		es
	pop		ax
	ret
endp ClearKeyBoardBuffer
;===========================================================================================================================================================
;
;===========================================================================================================================================================
start:
	mov ax, @data
	mov ds, ax
; Graphic mode
	mov ax, 13h
	int 10h
	
	call StartGame

; Back to text mode
	mov ah, 0
	mov al, 2
	int 10h
exit :
	mov ax, 4c00h
	int 21h
END start