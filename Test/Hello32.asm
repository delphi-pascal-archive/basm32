.APPTYPE GUI ; win32 gui

; include files, windows constants and structures
include 'inc\winconst.inc'

include 'inc\winstruct.inc'


; import functions from dll libraries
IMPORT ExitProcess, lib:"kernel32.dll", Name:"ExitProcess"
IMPORT MessageBoxA, lib:"user32.dll", Name:"MessageBoxA"

; Set up NIL constant
NIL	= 0

.DATA  ; Select data segment
 Message db 'Hello!',0
 Title   db 'BASM',0


.CODE  ; select code segment

START:
 ; MessageBoxA(NIL, Message, Title, MB_OK+MB_ICONINFORMATION);
	PUSH	MB_OK+MB_ICONINFORMATION
	PUSH	Title   ; same as :  push  offset Title
	PUSH	Message
	PUSH	NIL
	CALL	MessageBoxA
 ; ExitProcess(0);
	PUSH	0
	CALL	ExitProcess
;END
