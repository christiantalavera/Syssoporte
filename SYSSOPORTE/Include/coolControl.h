
#DEFINE CRLF            CHR(13) + CHR(10)

*-- Registro de windows
#DEFINE REG_VAL         "REG_NONE"
#DEFINE REG_STR         "REG_SZ"
#DEFINE REG_HEX         "REG_DWORD"
#DEFINE REG_BIN         "REG_BINARY"

*-- TYPE() tags
#DEFINE T_CHARACTER     "C"
#DEFINE T_NUMERIC       "N"
#DEFINE T_DOUBLE       	"B"
#DEFINE T_DATE         	"D"
#DEFINE T_DATETIME      "T"
#DEFINE T_MEMO          "M"
#DEFINE T_GENERAL       "G"
#DEFINE T_OBJECT        "O"
#DEFINE T_SCREEN        "S"
#DEFINE T_LOGICAL       "L"
#DEFINE T_CURRENCY      "Y"
#DEFINE T_UNDEFINED     "U"

*-- QueryUnload
#DEFINE FORM_CONTROLMENU        0
#DEFINE FORM_CODE               1
#DEFINE APP_WINDOWS             2
#DEFINE APP_TASKMANAGER         3
#DEFINE FORM_MDIFORM            4

*-- WindowState
#DEFINE WINDOWSTATE_NORMAL          0       && 0 - Normal
#DEFINE WINDOWSTATE_MINIMIZED       1       && 1 - Minimized
#DEFINE WINDOWSTATE_MAXIMIZED       2       && 2 - Maximized

*-- Window Borders
#DEFINE BORDER_NONE     0
#DEFINE BORDER_SINGLE   1
#DEFINE BORDER_DOUBLE   2
#DEFINE BORDER_SYSTEM   3

*-- Toolbar Positions
#DEFINE TOOL_NOTDOCKED  -1
#DEFINE TOOL_TOP         0
#DEFINE TOOL_LEFT        1
#DEFINE TOOL_RIGHT       2
#DEFINE TOOL_BOTTOM      3

*-- Button parameter masks
#DEFINE BUTTON_LEFT     1
#DEFINE BUTTON_RIGHT    2
#DEFINE BUTTON_MIDDLE   4

*-- Function Parameters
*-- MessageBox parameters
#DEFINE MB_OK                   0       && OK button only
#DEFINE MB_OKCANCEL             1       && OK and Cancel buttons
#DEFINE MB_ABORTRETRYIGNORE     2       && Abort, Retry, and Ignore buttons
#DEFINE MB_YESNOCANCEL          3       && Yes, No, and Cancel buttons
#DEFINE MB_YESNO                4       && Yes and No buttons
#DEFINE MB_RETRYCANCEL          5       && Retry and Cancel buttons

#DEFINE MB_ICONSTOP             16      && Critical message
#DEFINE MB_ICONQUESTION         32      && Warning query
#DEFINE MB_ICONEXCLAMATION      48      && Warning message
#DEFINE MB_ICONINFORMATION      64      && Information message

#DEFINE MB_APPLMODAL            0       && Application modal message box
#DEFINE MB_DEFBUTTON1           0       && First button is default
#DEFINE MB_DEFBUTTON2           256     && Second button is default
#DEFINE MB_DEFBUTTON3           512     && Third button is default
#DEFINE MB_SYSTEMMODAL          4096    && System Modal

*-- MsgBox return values
#DEFINE IDOK            1       && OK button pressed
#DEFINE IDCANCEL        2       && Cancel button pressed
#DEFINE IDABORT         3       && Abort button pressed
#DEFINE IDRETRY         4       && Retry button pressed
#DEFINE IDIGNORE        5       && Ignore button pressed
#DEFINE IDYES           6       && Yes button pressed
#DEFINE IDNO            7       && No button pressed

*-- PRTINFO() Constants
#DEFINE PRT_ORIENTATION         1
#DEFINE PRT_PAPERSIZE           2
#DEFINE PRT_PAPERLENGTH         3
#DEFINE PRT_PAPERWIDTH          4
#DEFINE PRT_SCALE               5
#DEFINE PRT_COPIES              6
#DEFINE PRT_DEFASOURCE          7
#DEFINE PRT_PRINTQUAL           8
#DEFINE PRT_COLOR               9
#DEFINE PRT_DUPLEX              10
#DEFINE PRT_YRESOLUTION         11
#DEFINE PRT_TTOPTION            12

*--PRTINFO() Return types
*-- Paper sizes
#define PRTPAPER_LETTER      1       && Letter 8 1/2 x 11 in               
#define PRTPAPER_LETTERSMALL 2       && Letter Small 8 1/2 x 11 in         
#define PRTPAPER_TABLOID     3       && Tabloid 11 x 17 in                 
#define PRTPAPER_LEDGER      4       && Ledger 17 x 11 in                  
#define PRTPAPER_LEGAL       5       && Legal 8 1/2 x 14 in                
#define PRTPAPER_STATEMENT   6       && Statement 5 1/2 x 8 1/2 in         
#define PRTPAPER_EXECUTIVE   7       && Executive 7 1/4 x 10 1/2 in      
#define PRTPAPER_A3          8       && A3 297 x 420 mm                    
#define PRTPAPER_A4          9       && A4 210 x 297 mm                    
#define PRTPAPER_A4SMALL     10      && A4 Small 210 x 297 mm              
#define PRTPAPER_A5          11      && A5 148 x 210 mm                    
#define PRTPAPER_B4          12      && B4 250 x 354                       
#define PRTPAPER_B5          13      && B5 182 x 257 mm                    
#define PRTPAPER_FOLIO       14      && Folio 8 1/2 x 13 in                
#define PRTPAPER_QUARTO      15      && Quarto 215 x 275 mm                
#define PRTPAPER_10X14       16      && 10x14 in                           
#define PRTPAPER_11X17       17      && 11x17 in                           
#define PRTPAPER_NOTE        18      && Note 8 1/2 x 11 in                 
#define PRTPAPER_ENV_9       19      && Envelope #9 3 7/8 x 8 7/8          
#define PRTPAPER_ENV_10      20      && Envelope #10 4 1/8 x 9 1/2         
#define PRTPAPER_ENV_11      21      && Envelope #11 4 1/2 x 10 3/8        
#define PRTPAPER_ENV_12      22      && Envelope #12 4 \276 x 11           
#define PRTPAPER_ENV_14      23      && Envelope #14 5 x 11 1/2            
#define PRTPAPER_CSHEET      24      && C size sheet                       
#define PRTPAPER_DSHEET      25      && D size sheet                       
#define PRTPAPER_ESHEET      26      && E size sheet                       
#define PRTPAPER_ENV_DL      27      && Envelope DL 110 x 220mm            
#define PRTPAPER_ENV_C5      28      && Envelope C5 162 x 229 mm           
#define PRTPAPER_ENV_C3      29      && Envelope C3  324 x 458 mm          
#define PRTPAPER_ENV_C4      30      && Envelope C4  229 x 324 mm          
#define PRTPAPER_ENV_C6      31      && Envelope C6  114 x 162 mm          
#define PRTPAPER_ENV_C65     32      && Envelope C65 114 x 229 mm          
#define PRTPAPER_ENV_B4      33      && Envelope B4  250 x 353 mm          
#define PRTPAPER_ENV_B5      34      && Envelope B5  176 x 250 mm          
#define PRTPAPER_ENV_B6      35      && Envelope B6  176 x 125 mm          
#define PRTPAPER_ENV_ITALY   36      && Envelope 110 x 230 mm              
#define PRTPAPER_ENV_MONARCH 37      && Envelope Monarch 3.875 x 7.5 in    
#define PRTPAPER_ENV_PERSONAL 38     && 6 3/4 Envelope 3 5/8 x 6 1/2 in    
#define PRTPAPER_FANFOLD_US  39      && US Std Fanfold 14 7/8 x 11 in      
#define PRTPAPER_FANFOLD_STD_GERMAN  40 && German Std Fanfold 8 1/2 x 12 in   
#define PRTPAPER_FANFOLD_LGL_GERMAN  41 && German Legal Fanfold 8 1/2 x 13 in 

*-- Paper bins
#define PRTBIN_UPPER            1
#define PRTBIN_ONLYONE          1
#define PRTBIN_LOWER            2
#define PRTBIN_MIDDLE           3
#define PRTBIN_MANUAL           4
#define PRTBIN_ENVELOPE         5
#define PRTBIN_ENVMANUAL        6
#define PRTBIN_AUTO             7
#define PRTBIN_TRACTOR          8
#define PRTBIN_SMALLFMT         9
#define PRTBIN_LARGEFMT         10
#define PRTBIN_LARGECAPACITY    11
#define PRTBIN_CASSETTE         14

*-- Print qualities
#define PRTRES_DRAFT        -1
#define PRTRES_LOW          -2
#define PRTRES_MEDIUM       -3
#define PRTRES_HIGH         -4

*-- FontMetric()
#DEFINE TM_HEIGHT           1
#DEFINE TM_ASCENT           2
#DEFINE TM_DESCENT          3
#DEFINE TM_INTERNALLEADING  4
#DEFINE TM_EXTERNALLEADING  5
#DEFINE TM_AVECHARWIDTH     6
#DEFINE TM_MAXCHARWIDTH     7
#DEFINE TM_WEIGHT           8
#DEFINE TM_ITALIC           9
#DEFINE TM_UNDERLINED      10
#DEFINE TM_STRUCKOUT       11
#DEFINE TM_FIRSTCHAR       12
#DEFINE TM_LASTCHAR        13
#DEFINE TM_DEFAULTCHAR     14
#DEFINE TM_BREAKCHAR       15
#DEFINE TM_PITCHANDFAMILY  16
#DEFINE TM_CHARSET         17
#DEFINE TM_OVERHANG        18
#DEFINE TM_ASPECTX         19
#DEFINE TM_ASPECTY         20

*-- SysMetric()
#DEFINE SM_CXSCREEN             0
#DEFINE SM_CYSCREEN             1
#DEFINE SM_CXVSCROLL            2
#DEFINE SM_CYHSCROLL            3
#DEFINE SM_CYCAPTION            4
#DEFINE SM_CXBORDER             5
#DEFINE SM_CYBORDER             6
#DEFINE SM_CXDLGFRAME           7
#DEFINE SM_CYDLGFRAME           8
#DEFINE SM_CYVTHUMB             9
#DEFINE SM_CXHTHUMB             10
#DEFINE SM_CXICON               11
#DEFINE SM_CYICON               12
#DEFINE SM_CXCURSOR             13
#DEFINE SM_CYCURSOR             14
#DEFINE SM_CYMENU               15
#DEFINE SM_CXFULLSCREEN         16
#DEFINE SM_CYFULLSCREEN         17
#DEFINE SM_CYKANJIWINDOW        18
#DEFINE SM_MOUSEPRESENT         19
#DEFINE SM_CYVSCROLL            20
#DEFINE SM_CXHSCROLL            21
#DEFINE SM_DEBUG                22
#DEFINE SM_SWAPBUTTON           23
#DEFINE SM_RESERVED1            24
#DEFINE SM_RESERVED2            25
#DEFINE SM_RESERVED3            26
#DEFINE SM_RESERVED4            27
#DEFINE SM_CXMIN                28
#DEFINE SM_CYMIN                29
#DEFINE SM_CXSIZE               30
#DEFINE SM_CYSIZE               31
#DEFINE SM_CXFRAME              32
#DEFINE SM_CYFRAME              33
#DEFINE SM_CXMINTRACK           34
#DEFINE SM_CYMINTRACK           35
#DEFINE SM_CMETRICS             36

#DEFINE COLOR_SCROLLBAR            0
#DEFINE COLOR_BACKGROUND           1
#DEFINE COLOR_ACTIVECAPTION        2
#DEFINE COLOR_INACTIVECAPTION      3
#DEFINE COLOR_MENU                 4
#DEFINE COLOR_WINDOW               5
#DEFINE COLOR_WINDOWFRAME          6
#DEFINE COLOR_MENUTEXT             7
#DEFINE COLOR_WINDOWTEXT           8
#DEFINE COLOR_CAPTIONTEXT          9
#DEFINE COLOR_ACTIVEBORDER        10
#DEFINE COLOR_INACTIVEBORDER      11
#DEFINE COLOR_APPWORKSPACE        12
#DEFINE COLOR_HIGHLIGHT           13
#DEFINE COLOR_HIGHLIGHTTEXT       14
#DEFINE COLOR_BTNFACE             15
#DEFINE COLOR_BTNSHADOW           16
#DEFINE COLOR_GRAYTEXT            17
#DEFINE COLOR_BTNTEXT             18
#DEFINE COLOR_INACTIVECAPTIONTEXT 19
#DEFINE COLOR_BTNHIGHLIGHT        20

#DEFINE COLOR_3DDKSHADOW          21
#DEFINE COLOR_3DLIGHT             22
#DEFINE COLOR_INFOTEXT            23
#DEFINE COLOR_INFOBK              24

#Define EM_GETLINE              196
#Define EM_GETLINECOUNT         186
#Define EM_GETSEL               176
#Define EM_LINEFROMCHAR         201
#Define EM_LINEINDEX            187
#Define EM_LINELENGTH           193
#Define EM_LINESCROLL           182
