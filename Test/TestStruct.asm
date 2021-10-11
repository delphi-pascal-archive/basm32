.APPTYPE GUI

include 'inc\winconst.inc'
include 'inc\winstruct.inc'

IMPORT ExitProcess, lib:"kernel32.dll", Name:"ExitProcess"
IMPORT MessageBoxA, lib:"user32.dll", Name:"MessageBoxA"

StructMsgbox STRUCT
 Style   dd 40H
 Message db 'Hello!',0
 Title   db 'Title',0
ends StructMsgbox 

.CODE
	Jmp	Start

Msgb StructMsgbox

START:	
; MessageBoxA
	mov eax, Msgb.Title
	PUSH	[Msgb.Style]
	PUSH	Msgb.Title
	PUSH	Msgb.Message	; ADDR or OFFSET
	PUSH	0
	CALL	MessageBoxA
; ExitProcess
	PUSH	0
	CALL	ExitProcess



END
