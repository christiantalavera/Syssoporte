*Modified in VFP format : 2006-10-10
*Dorin Vasilescu

*/*
 * << Haru Free PDF Library 2.0.3 >> -- hpdf_types.h
 *
 * URL http://libharu.sourceforge.net/
 *
 * Copyright (c) 1999-2006 Takeshi Kanno
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.
 * It is provided "as is" without express or implied warranty.
 *

*/*----------------------------------------------------------------------------*/
*/*----- type definition ------------------------------------------------------*/


*/*  native OS integer types */
#define HPDF_INT	integer		&&typedef  signed int          HPDF_INT
#define HPDF_UINT	integer		&&typedef  unsigned int        

#define HPDF_INT32	integer		&&typedef  signed int          HPDF_INT32
#define HPDF_UINT32	integer		&&typedef  unsigned int        HPDF_UINT32

#define HPDF_INT16	integer
#define HPDF_UINT16 integer

*/*  8bit integer types
#define HPDF_INT8 	integer		&&typedef  signed char  HPDF_INT8
#define HPDF_UINT8	integer		&&typedef  unsigned char       HPDF_UINT8

*/*  8bit binary types
#define HPDF_BYTE	string		&&typedef  unsigned char   HPDF_BYTE

*/*  float type (32bit IEEE754)
#define HPDF_REAL	single		&&typedef  float               HPDF_REAL

*/*  double type (64bit IEEE754)
#define HPDF_DOUBLE	double		&&typedef  double              HPDF_DOUBLE

*/*  boolean type (0: False, !0: True)
#define HPDF_BOOL	integer		&&typedef  signed int          HPDF_BOOL

*/*  error-no type (32bit unsigned integer)
#define HPDF_STATUS	integer		&&typedef  unsigned long       HPDF_STATUS

#define  HPDF_CID			HPDF_UINT16         
#define  HPDF_UNICODE		HPDF_UINT16        

*typedef enum _HPDF_InfoType {
*    /* date-time type parameters */
#define    HPDF_INFO_CREATION_DATE  0
#define    HPDF_INFO_MOD_DATE		1

*/* string type parameters */
#define    HPDF_INFO_AUTHOR			3
#define    HPDF_INFO_CREATOR		4
#define    HPDF_INFO_PRODUCER		5
#define    HPDF_INFO_TITLE			6
#define    HPDF_INFO_SUBJECT		7
#define	   HPDF_INFO_KEYWORDS		8
#define	   HPDF_INFO_EOF			9
#define	   HPDF_InfoType		integer


*typedef enum  _HPDF_EncryptMode {
#define    HPDF_ENCRYPT_R2     2
#define    HPDF_ENCRYPT_R3     3
#define	   HPDF_EncryptMode		integer


*typedef void
*(HPDF_STDCALL *HPDF_Error_Handler)  (HPDF_STATUS   error_no,
*                                     HPDF_STATUS   detail_no,
*                                     void         *user_data);

*typedef void*
*(HPDF_STDCALL *HPDF_Alloc_Func)  (HPDF_UINT  size);


*typedef void
*(HPDF_STDCALL *HPDF_Free_Func)  (void  *aptr);


*/*---------------------------------------------------------------------------*/
*/*------ text width struct --------------------------------------------------*/

*typedef struct _HPDF_TextWidth {
*    HPDF_UINT numchars;
*
*    /* don't use this value (it may be change in the feature).
*       use numspace as alternated. */
*    HPDF_UINT numwords;
*
*    HPDF_UINT width;
*    HPDF_UINT numspace;
*} HPDF_TextWidth;


*/*---------------------------------------------------------------------------*/
*/*------ dash mode ----------------------------------------------------------*/

*typedef struct _HPDF_DashMode {
*    HPDF_UINT16  ptn[8];
*    HPDF_UINT    num_ptn;
*    HPDF_UINT    phase;
*} HPDF_DashMode;


*/*---------------------------------------------------------------------------*/
*/*----- HPDF_TransMatrix struct ---------------------------------------------*/

*typedef struct _HPDF_TransMatrix {
*    HPDF_REAL   a;
*    HPDF_REAL   b;
*    HPDF_REAL   c;
*    HPDF_REAL   d;
*    HPDF_REAL   x;
*    HPDF_REAL   y;
*} HPDF_TransMatrix;


*/*---------------------------------------------------------------------------*/

*typedef enum _HPDF_ColorSpace {

#define    HPDF_CS_DEVICE_GRAY  0
#define    HPDF_CS_DEVICE_RGB	1
#define    HPDF_CS_DEVICE_CMYK	2
#define    HPDF_CS_CAL_GRAY		3
#define    HPDF_CS_CAL_RGB		4
#define    HPDF_CS_LAB			5
#define    HPDF_CS_ICC_BASED	6
#define    HPDF_CS_SEPARATION	7
#define    HPDF_CS_DEVICE_N		8
#define    HPDF_CS_INDEXED		9
#define    HPDF_CS_PATTERN		10
#define    HPDF_CS_EOF			11
#define	   HPDF_ColorSpace		integer

*/*---------------------------------------------------------------------------*/
*/*----- HPDF_RGBColor struct ------------------------------------------------*/


*typedef struct _HPDF_RGBColor {
*    HPDF_REAL   r;
    HPDF_REAL   g;
    HPDF_REAL   b;

#define HPDF_RGBColor	integer 


*/*---------------------------------------------------------------------------*/
*/*----- HPDF_CMYKColor struct -----------------------------------------------*/

*typedef struct _HPDF_CMYKColor {
*    HPDF_REAL   c;
    HPDF_REAL   m;
    HPDF_REAL   y;
    HPDF_REAL   k;
} HPDF_CMYKColor;

*/*---------------------------------------------------------------------------*/
*/*------ The line cap style -------------------------------------------------*/

*typedef enum _HPDF_LineCap {
#define    HPDF_BUTT_END				0
#define    HPDF_ROUND_END				1
#define    HPDF_PROJECTING_SCUARE_END	2
#define    HPDF_LINECAP_EOF				3
#define	   HPDF_LineCap					integer

*/*----------------------------------------------------------------------------*/
*/*------ The line join style -------------------------------------------------*/

*typedef enum _HPDF_LineJoin {
#define    HPDF_MITER_JOIN		0
#define    HPDF_ROUND_JOIN		1
#define    HPDF_BEVEL_JOIN		2
#define    HPDF_LINEJOIN_EOF	3
#define	   HPDF_LineJoin		integer

*/*----------------------------------------------------------------------------*/
*/*------ The text rendering mode ---------------------------------------------*/

*typedef enum _HPDF_TextRenderingMode {
#define    HPDF_FILL					0
#define    HPDF_STROKE					1
#define    HPDF_FILL_THEN_STROKE		2
#define    HPDF_INVISIBLE				3
#define    HPDF_FILL_CLIPPING			4
#define    HPDF_STROKE_CLIPPING			5
#define    HPDF_FILL_STROKE_CLIPPING	6
#define    HPDF_CLIPPING				7
#define    HPDF_RENDERING_MODE_EOF		8
#define	   HPDF_TextRenderingMode		integer


*typedef enum _HPDF_WritingMode {
#define    HPDF_WMODE_HORIZONTAL 	0
#define    HPDF_WMODE_VERTICAL		1
#define    HPDF_WMODE_EOF			2
#define	   HPDF_WritingMode			integer


*typedef enum _HPDF_PageLayout {
#define    HPDF_PAGE_LAYOUT_SINGLE				0
#define    HPDF_PAGE_LAYOUT_ONE_COLUMN			1
#define    HPDF_PAGE_LAYOUT_TWO_COLUMN_LEFT		2
#define    HPDF_PAGE_LAYOUT_TWO_COLUMN_RIGHT	3
#define    HPDF_PAGE_LAYOUT_EOF					4
#define	   HPDF_PageLayout						integer


*typedef enum _HPDF_PageMode {
#define    HPDF_PAGE_MODE_USE_NONE			0
#define    HPDF_PAGE_MODE_USE_OUTLINE		1
#define    HPDF_PAGE_MODE_USE_THUMBS		2
#define    HPDF_PAGE_MODE_FULL_SCREEN		3
*/*  HPDF_PAGE_MODE_USE_OC,
#define    HPDF_PAGE_MODE_USE_ATTACHMENTS	4
 */
#define    HPDF_PAGE_MODE_EOF				5
#define	   HPDF_PageMode					integer


*typedef enum _HPDF_PageNumStyle {
#define    HPDF_PAGE_NUM_STYLE_DECIMAL			0
#define    HPDF_PAGE_NUM_STYLE_UPPER_ROMAN		1
#define    HPDF_PAGE_NUM_STYLE_LOWER_ROMAN		2
#define    HPDF_PAGE_NUM_STYLE_UPPER_LETTERS	3
#define    HPDF_PAGE_NUM_STYLE_LOWER_LETTERS	4
#define    HPDF_PAGE_NUM_STYLE_EOF				5
#define	   HPDF_PageNumStyle					integer


*typedef enum _HPDF_DestinationType {
#define    HPDF_XYZ		0
#define    HPDF_FIT		1
#define    HPDF_FIT_H	3
#define    HPDF_FIT_V	4
#define    HPDF_FIT_R	5
#define    HPDF_FIT_B	6
#define    HPDF_FIT_BH	7
#define    HPDF_FIT_BV	8
#define    HPDF_DST_EOF	9
#define	   HPDF_DestinationType	integer


*typedef enum _HPDF_AnnotType {
#define    HPDF_ANNOT_TEXT_NOTES		0
#define    HPDF_ANNOT_LINK				1
#define    HPDF_ANNOT_SOUND				2
#define    HPDF_ANNOT_FREE_TEXT			3
#define    HPDF_ANNOT_STAMP				4
#define    HPDF_ANNOT_SQUARE			5
#define    HPDF_ANNOT_CIRCLE			6
#define    HPDF_ANNOT_STRIKE_OUT		7
#define    HPDF_ANNOT_HIGHTLIGHT		8
#define    HPDF_ANNOT_UNDERLINE			9
#define    HPDF_ANNOT_INK				10
#define    HPDF_ANNOT_FILE_ATTACHMENT	11
#define    HPDF_ANNOT_POPUP				12
#define	   HPDF_AnnotType				integer


*typedef enum _HPDF_AnnotFlgs {
#define    HPDF_ANNOT_INVISIBLE		0
#define    HPDF_ANNOT_HIDDEN		1
#define    HPDF_ANNOT_PRINT			2
#define    HPDF_ANNOT_NOZOOM		3
#define    HPDF_ANNOT_NOROTATE		4
#define    HPDF_ANNOT_NOVIEW		5
#define    HPDF_ANNOT_READONLY		6
#define	   HPDF_AnnotFlgs			integer


*typedef enum _HPDF_AnnotHighlightMode {
#define    HPDF_ANNOT_NO_HIGHTLIGHT			0
#define    HPDF_ANNOT_INVERT_BOX			1
#define    HPDF_ANNOT_INVERT_BORDER			2
#define    HPDF_ANNOT_DOWN_APPEARANCE		3
#define    HPDF_ANNOT_HIGHTLIGHT_MODE_EOF	4
#define	   HPDF_AnnotHighlightMode			integer


*typedef enum _HPDF_AnnotIcon {
#define    HPDF_ANNOT_ICON_COMMENT			0
#define    HPDF_ANNOT_ICON_KEY				1
#define    HPDF_ANNOT_ICON_NOTE				2
#define    HPDF_ANNOT_ICON_HELP				3
#define    HPDF_ANNOT_ICON_NEW_PARAGRAPH	4
#define    HPDF_ANNOT_ICON_PARAGRAPH		5
#define    HPDF_ANNOT_ICON_INSERT			6
#define    HPDF_ANNOT_ICON_EOF				7
#define	   HPDF_AnnotIcon					integer

*/*----------------------------------------------------------------------------*/
*/*------ border stype --------------------------------------------------------*/

*typedef enum _HPDF_BSSubtype {
#define    HPDF_BS_SOLID		0
#define    HPDF_BS_DASHED		1
#define    HPDF_BS_BEVELED		2
#define    HPDF_BS_INSET		3
#define    HPDF_BS_UNDERLINED	4
#define	   HPDF_BSSubtype		integer


*typedef enum _HPDF_PageSizes {
#define    HPDF_PAGE_SIZE_LETTER 	0
#define    HPDF_PAGE_SIZE_LEGAL		1
#define    HPDF_PAGE_SIZE_A3		2
#define    HPDF_PAGE_SIZE_A4		3
#define    HPDF_PAGE_SIZE_A5		4
#define    HPDF_PAGE_SIZE_B4		5
#define    HPDF_PAGE_SIZE_B5		6
#define    HPDF_PAGE_SIZE_EXECUTIVE	7
#define    HPDF_PAGE_SIZE_US4x6		8
#define    HPDF_PAGE_SIZE_US4x8		9
#define    HPDF_PAGE_SIZE_US5x7		10
#define    HPDF_PAGE_SIZE_COMM10	11
#define    HPDF_PAGE_SIZE_EOF		12
#define    HPDF_PageSizes	integer


*typedef enum _HPDF_PageDirection {
#define    HPDF_PAGE_PORTRAIT  	0
#define    HPDF_PAGE_LANDSCAPE	1
#define	   HPDF_PageDirection	integer


*typedef enum  _HPDF_EncoderType {
#define    HPDF_ENCODER_TYPE_SINGLE_BYTE	0
#define    HPDF_ENCODER_TYPE_DOUBLE_BYTE	1
#define    HPDF_ENCODER_TYPE_UNINITIALIZED	2
#define    HPDF_ENCODER_UNKNOWN				3
#define	   HPDF_EncoderType					integer


*typedef enum _HPDF_ByteType {
#define    HPDF_BYTE_TYPE_SINGLE	0
#define    HPDF_BYTE_TYPE_LEAD		1
#define    HPDF_BYTE_TYPE_TRIAL		2
#define    HPDF_BYTE_TYPE_UNKNOWN	3
#define	   HPDF_ByteType			integer


*typedef enum _HPDF_TextAlignment {
#define    HPDF_TALIGN_LEFT		0
#define    HPDF_TALIGN_RIGHT	1
#define    HPDF_TALIGN_CENTER	2
#define    HPDF_TALIGN_JUSTIFY	3
#define	   HPDF_TextAlignment	integer


*/*  HPDF_Point struct
*/
*typedef  struct  _HPDF_Point {
*    HPDF_REAL  x;
*    HPDF_REAL  y;

#define HPDF_Point String

*typedef  struct _HPDF_Rect {
*    HPDF_REAL  left;
    HPDF_REAL  bottom;
    HPDF_REAL  right;
    HPDF_REAL  top;
    
#define HPDF_Rect	string
#define HPDF_Box	HPDF_Rect

*/* HPDF_Date struct
 */
*typedef  struct  _HPDF_Date {
*    HPDF_INT    year;
    HPDF_INT    month;
    HPDF_INT    day;
    HPDF_INT    hour;
    HPDF_INT    minutes;
    HPDF_INT    seconds;
    char        ind;
    HPDF_INT    off_hour;
    HPDF_INT    off_minutes;

#define HPDF_Date string
