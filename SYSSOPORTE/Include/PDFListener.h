#include reportlisteners.h
#include foxpro_reporting.h


#define FALSE .F.
#define TRUE .T.
#define CR CHR(13)
#define LF CHR(10)
#define chr0	Chr(0)
#define CONV_FACTOR	0.075

*percents of FONTMETRIC(1) used to adjust line height , at least some aproximate value
#define LINE_ADJUST_FIELDS 0.845
#define LINE_ADJUST_LABELS 0.90

*registry constants
#define HKEY_CLASSES_ROOT	-2147483648
#define HKEY_CURRENT_USER	-2147483647
#define HKEY_LOCAL_MACHINE	-2147483646
#define HKEY_USERS			-2147483645

#define REG_SZ 				1	&& Data string
#define REG_BINARY 			3	&& Binary data in any form.
#define REG_DWORD 			4	&& A 32-bit number.

#define CLSID_JPG '{557CF401-1A04-11D3-9A73-0000F81EF32E}'

