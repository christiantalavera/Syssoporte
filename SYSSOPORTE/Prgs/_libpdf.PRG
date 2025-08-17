*!* Program: PDF Generator library
*!* Author: Dorin Vasilescu
*!* Date:
*!* Copyright: 2007 Dorin Vasilescu
*!* Description: PDF generator library, free libharu API wrapper
*!*
*!* Revision Information:
*!* 2007.05 - first beta release


*if commented, status_Assign will not be included, slightly faster code
*but no access to error messages from API  
#define WITH_STATUS_ASSIGN	1

#include hpdf_consts.h
#include hpdf_types.h

#define FALSE .F.
#define TRUE .T.
#define CR CHR(13)
#define LF CHR(10)
#define chr0	Chr(0)

define class pdfGen as Custom olepublic 

	doc = 0					&& pdf document handle, returned by New()
	name = "_pdf"
	initialized = FALSE
	pdfLibrary = "libhpdf.dll"
	status = 0				&&last status 0 = HPDF_OK
	errorMsg = ''			&&error message
	errors = .null.			&&error messages

	*info attributes
	info_author = 'PDF'		&&pdf info
	info_creator = 'VFP9'
	info_producer = 'PDF library'
	info_title = ' '
	info_subject = ' '
	info_keywords = ' '

	*encryption
	owner_passwd = ''
	user_passwd = ''
	encrypt_mode = 'R3'		&& R2 or R3, R3 has 5 to 16 key length
	key_length = 8

	*permissions
	enable_print = TRUE
	enable_copy = TRUE

	**************
	procedure Init
		with this

			.Addproperty('aDeclaredDLLs[1,3]','')
			.errors = NewObject('Collection')
			
			if not this.initialized
				.LoadDLLS()
				.Declare_Dlls()
				.InitializeErrCodes()
				store TRUE to this.initialized
			endif
			.doc = .New()
		endwith
	endproc

	procedure loadDLLS
		local array aTemp[1,3]
		aTemp = ''
		if aDlls(aTemp) > 0
			dimension this.aDeclaredDLLs[Alen(aTemp,1),3]
			Acopy(aTemp,this.aDeclaredDLLs)
		endif
	endproc

	procedure SetPDF
		with this
			.status =  .SetInfoAttr( HPDF_INFO_AUTHOR , .info_author )
			.status = .SetInfoAttr( HPDF_INFO_CREATOR , .info_creator )
			.status = .SetInfoAttr( HPDF_INFO_PRODUCER , .info_producer )
			.status = .SetInfoAttr( HPDF_INFO_TITLE , .info_title )
			.status = .SetInfoAttr( HPDF_INFO_SUBJECT , .info_subject )

			.SetPermission()

			if not Empty(.owner_passwd) or not Empty(.user_passwd)
				.SetPassword( .owner_passwd, .user_passwd )
			endif

		endwith
	endproc

#IFDEF WITH_STATUS_ASSIGN
	***********************
	procedure status_assign
		lparameters nErrorCode
		if m.nErrorCode = this.status 
			return 
		endif 
		this.status =  m.nErrorCode
		if nErrorCode <> 0
			this.errormsg = 'Unknown error.'
			try 
				this.errormsg = this.errors.Item(Transform( m.nErrorCode))
			catch
			endtry 
		endif 
	endproc 
#ENDIF 

	*!*************************************************************************
	*!* main PDF document handling functions
	*!*************************************************************************
	procedure New
		if this.HasDoc()
			this.FreeDoc()
		endif
		*this.SetPDF()
		return HPDF_New(0,0)
	endproc

	procedure Free( )
		this.status = HPDF_Free(this.doc)
		return this.status = HPDF_OK
	endproc

	procedure NewDoc()
		local pdf
		pdf = this.doc
		this.status = HPDF_NewDoc(this.doc)
		return this.status = HPDF_OK
	endproc

	*revoke document but keeps allocated resources (images, encodings, fonts) for reuse
	*
	procedure FreeDoc()
		this.status = HPDF_FreeDoc(this.doc)
		return this.status = HPDF_OK
	endproc

	*revoke document and all allocated resources (images, encodings, fonts)
	*
	procedure FreeDocAll()
		this.status = HPDF_FreeDocAll(this.doc)
		return this.status = HPDF_OK
	endproc

	procedure HasDoc(pdf)
		return not HPDF_HasDoc(pdf) = 0
	endproc

	procedure SaveToStream()
		this.status = HPDF_SaveToStream(this.doc)
		return this.status = HPDF_OK
	endproc

	procedure GetStreamSize()
		return HPDF_GetStreamSize(this.doc )
	endproc

	procedure ReadFromStream
		Parameters buf,size
		return HPDF_ReadFromStream(this.doc, @m.buf, m.size )
	endproc

	procedure ResetStream()
		this.status = HPDF_ResetStream(this.doc)
		return this.status = HPDF_OK
	endproc

	procedure SaveToFile( filename ) as HPDF_Status
		this.status = HPDF_SaveToFile(this.doc,m.filename)
		return this.status 
	endproc

	*returns last error code of pdf object
	procedure GetError()
		return HPDF_GetError(this.doc)
	endproc

	procedure ResetError()
		this.status = HPDF_ResetError(this.doc)
		return this.status = HPDF_OK
	endproc

	procedure GetPageByIndex(index)
		this.status = this.GetError()
		return HPDF_GetPageByIndex(this.doc,m.index)
	endproc

	procedure GetPageLayout()
		return HPDF_GetPageLayout(this.doc)
	endproc

	procedure SetPageLayout(layout)
		this.status = HPDF_SetPageLayout(this.doc, m.layout)
		return this.status = HPDF_OK
	endproc

	procedure GetPageMode()
		return HPDF_GetPageMode(this.doc)
	endproc

	procedure SetPageMode(mode)
		this.status = HPDF_SetPageMode(this.doc,m.mode)
		return this.status = HPDF_OK
	endproc

	procedure GetViewerPreference()
		return HPDF_GetViewerPreference(this.doc)
	endproc

	procedure SetViewerPreference(prefv)
		this.status = HPDF_SetViewerPreference(this.doc,m.prefv)
		return this.status = HPDF_OK
	endproc

	procedure SetOpenAction(open_action)
		this.status = HPDF_SetOpenAction(this.doc,m.open_action)
		return this.status = HPDF_OK
	endproc


	*!*************************************************************************
	*!* PDF document pages handling functions
	*!*************************************************************************

	procedure GetCurrentPage()
		return 	HPDF_GetCurrentPage(this.doc)
	endproc

	procedure AddPage()
		return HPDF_AddPage(this.doc)
	endproc

	procedure InsertPage(page)
		return HPDF_InsertPage(this.doc,page)
	endproc

	procedure Page_SetWidth(page,value)
		this.status = HPDF_Page_SetWidth(m.page,m.value)
		return this.status = HPDF_OK
	endproc

	procedure Page_SetHeight(page,value)
		this.status = HPDF_Page_SetHeight(m.page,m.value)
		return this.status = HPDF_OK
	endproc

	procedure Page_SetHeight(page,value)
		this.status = HPDF_Page_SetHeight(m.page,m.value)
		return this.status = HPDF_OK
	endproc

	procedure Page_SetSize(page,size,direction)
		this.status = HPDF_Page_SetSize(m.page,m.size,m.direction)
		return this.status = HPDF_OK
	endproc

	procedure Page_SetRotate(page,angle)
		this.status = HPDF_Page_SetRotate(m.page,m.angle)
		return this.status = HPDF_OK
	endproc

	procedure Page_GetCurrentPos(page)
		local point as Empty
		local cPoint as String 
		cPoint = Replicate(Chr(0),8)
		point = Newobject('Empty')
		this.status = HPDF_Page_GetCurrentPos2(m.page,@m.cPoint)
		Addproperty(point,'x',Ctobin(Left(cPoint,4),'N'))
		Addproperty(point,'y',Ctobin(Right(cPoint,4),'N'))
		return point
	endproc 

	procedure Page_GetCurrentPos2(page)
		local point as Empty
		local cPoint as String 
		cPoint = Replicate(Chr(0),8)
		point = Newobject('Empty')
		this.status = HPDF_Page_GetCurrentPos2(m.page,@m.cPoint)
		Addproperty(point,'x',Ctobin(Left(m.cPoint,4),'N'))
		Addproperty(point,'y',Ctobin(Right(m.cPoint,4),'N'))
		return point
	endproc 

	procedure Page_GetCurrentTextPos(page)
		local point as Empty
		local cPoint as String 
		cPoint = Replicate(Chr(0),8)
		point = Newobject('Empty')
		this.status = HPDF_Page_GetCurrentTextPos2(m.page, @m.cPoint)
		Addproperty(point,'x',Ctobin(Left(m.cPoint,4),'N'))
		Addproperty(point,'y',Ctobin(Right(m.cPoint,4),'N'))
		return point
	endproc 
	

	*!*************************************************************************
	*!* PDF document font handling functions
	*!*************************************************************************

	procedure GetFont(font_name,encoding_name)
		return HPDF_GetFont(this.doc,m.font_name,m.encoding_name)
	endproc

	procedure LoadType1FontFromFile(afm_file_name, data_file_name)
		return HPDF_LoadType1FontFromFile(this.doc,m.afm_file_name,m.data_file_name)
	endproc

	procedure LoadTTFontFromFile(file_name, embedding)
		return HPDF_LoadTTFontFromFile(this.doc, m.file_name, m.embedding)
	endproc

	procedure LoadTTFontFromFile2(file_name, index, embedding)
		return HPDF_LoadTTFontFromFile2(this.doc, m.file_name, m.index, m.embedding)
	endproc

	procedure AddPageLabel(page_num,style,first_page,prefix)
		return HPDF_AddPageLabel(this.doc,m.page_num,m.style,m.first_page,m.prefix)
	endproc

	*!*************************************************************************
	*!* PDF document outline handling functions
	*!*************************************************************************

	procedure CreateOutline(parent,title,encoder)
		return HPDF_CreateOutline(this.doc,m.parent,m.title,m.encoder)
	endproc

	procedure Outline_SetOpened(outline as HPDF_Outline, opened as HPDF_BOOL) as HPDF_STATUS
		return HPDF_Outline_SetOpened(m.outline,m.opened)
	endproc

	procedure Outline_SetDestination(outline as HPDF_Outline, dst as HPDF_Outline) as HPDF_STATUS
		return HPDF_Outline_SetDestination(m.outline,m.dst)
	endproc

	*!*************************************************************************
	*!* PDF document destinations handling functions
	*!*************************************************************************

	procedure Page_CreateDestination(page as HPDF_Page) as HPDF_Destination
		pagedst = HPDF_Page_CreateDestination(m.page)
		if pagedst = 0
			this.status = this.GetError()
		endif 
		return pagedst
	endproc

	procedure Destination_SetXYZ(dst as HPDF_Destination ;
			,left as HPDF_Destination ;
			,top as HPDF_Destination  ;
			,zoom as HPDF_Destination ) as HPDF_STATUS

		this.status = HPDF_Destination_SetXYZ(m.dst,m.left,m.top,m.zoom)
		return this.status = HPDF_OK
	endproc


	procedure Destination_SetFit(dst as HPDF_Destination) as HPDF_STATUS
		this.status = HPDF_Destination_SetFit(m.dst)
		return this.status = HPDF_OK
	endproc


	procedure Destination_SetFitH(dst as HPDF_Destination ;
			,top as HPDF_Destination) ;
			as HPDF_STATUS
		this.status = HPDF_Destination_SetFitH(m.dst , m.top )
		return this.status = HPDF_OK
	endproc

	procedure Destination_SetFitV( dst as HPDF_Destination ;
			,left as HPDF_REAL ) as HPDF_STATUS
		this.status = HPDF_Destination_SetFitV(m.dst,m.left)
		return this.status = HPDF_OK
	endproc

	procedure Destination_SetFitR(dst as HPDF_Destination ;
			,left as HPDF_REAL 			;
			,bottom as HPDF_REAL        ;
			,right as HPDF_REAL         ;
			,top as HPDF_REAL ) as HPDF_STATUS
			
		this.status = HPDF_Destination_SetFitR(m.dst,m.left,m.bottom,m.right)
		return this.status = HPDF_OK
	endproc

	procedure Destination_SetFitB(dst as HPDF_Destination) as HPDF_STATUS
		this.status = HPDF_Destination_SetFitB(m.dst)
		return this.status = HPDF_OK
	endproc

	procedure Destination_SetFitBH(dst as HPDF_Destination  ;
			,top as HPDF_REAL) as HPDF_STATUS
		this.status = HPDF_Destination_SetFitBH(m.dst,m.top)
		return this.status = HPDF_OK
	endproc

	procedure Destination_SetFitBV(dst as HPDF_Destination  ;
			,left as HPDF_REAL ) as HPDF_STATUS
		this.status = HPDF_Destination_SetFitBV(m.dst,m.left)
		return this.status = HPDF_OK
	endproc

	*!*************************************************************************
	*!* PDF document encoder functions
	*!*************************************************************************

	procedure GetEncoder(encoding_name as String) as HPDF_Encoder
		return HPDF_GetEncoder(this.doc,m.encoding_name)
	endproc

	procedure GetCurrentEncoder() as HPDF_Encoder
		return HPDF_GetCurrentEncoder  (this.doc)
	endproc

	procedure SetCurrentEncoder(encoding_name) as HPDF_STATUS
		this.status = HPDF_SetCurrentEncoder(this.doc,m.encoding_name)
		return this.status = HPDF_OK
	endproc

	procedure Encoder_GetType(encoder as HPDF_Encoder) as HPDF_EncoderType
		return HPDF_Encoder_GetType(m.encoder)
	endproc

	procedure Encoder_GetByteType(encoder as HPDF_Encoder ;
			,text as String ;
			,index as HPDF_UINT ) as HPDF_ByteType
		return HPDF_Encoder_GetByteType  (m.encoder,m.text,m.index)
	endproc

	procedure Encoder_GetUnicode(encoder as HPDF_Encoder   ;
			,code as HPDF_UINT16) as HPDF_UNICODE
		return HPDF_Encoder_GetUnicode(m.encoder,m.code)
	endproc

	procedure Encoder_GetWritingMode (encoder as HPDF_Encoder) as HPDF_WritingMode
		return HPDF_Encoder_GetWritingMode(m.encoder)
	endproc

	procedure UseJPEncodings() as HPDF_STATUS
		this.status = HPDF_UseJPEncodings(this.doc)
		return this.status = HPDF_OK
	endproc

	procedure UseKREncodings() as HPDF_STATUS
		this.status = HPDF_UseKREncodings(this.doc)
		return this.status = HPDF_OK
	endproc

	procedure UseCNSEncodings() as HPDF_STATUS
		this.status = HPDF_UseCNSEncodings(this.doc)
		return this.status = HPDF_OK
	endproc

	procedure UseCNTEncodings() as HPDF_STATUS
		this.status = HPDF_UseCNTEncodings(this.doc)
		return this.status = HPDF_OK
	endproc

	*!*************************************************************************
	*!* PDF document annotations functions
	*!*************************************************************************

	procedure Page_CreateTextAnnot( page as HPDF_Page ;
			,rect as HPDF_Rect ;
			,text as String ;
			,encoder as HPDF_Encoder ) as HPDF_Annotation
		retval = HPDF_Page_CreateTextAnnot(m.page,m.rect,m.text,m.encoder)
		if m.retval = 0
			this.status = this.GetError()
		endif 
		return m.retval
	endproc

	procedure Page_CreateLinkAnnot(page as HPDF_Page ;
			,rect as HPDF_Rect ;
			,dst as HPDF_Destination ) as  HPDF_Annotation
		m.retval = HPDF_Page_CreateLinkAnnot  (m.page,m.rect,m.dst)
		if m.retval = 0
			this.status = this.GetError()
		endif 
		return retval
	endproc

	procedure Page_CreateURILinkAnnot(page as HPDF_Page	;
			,rect as HPDF_Rect	;
			,url as String) as HPDF_Annotation
		retval = HPDF_Page_CreateURILinkAnnot(m.page,m.rect,m.url)
		if m.retval = 0
			this.status = this.GetError()
		endif 
		return retval
	endproc

	procedure LinkAnnot_SetHighlightMode(annot as HPDF_Annotation ;
			,mode as HPDF_AnnotHighlightMode) as HPDF_STATUS
		this.status = HPDF_LinkAnnot_SetHighlightMode(m.annot,m.mode)
		return this.status = HPDF_OK
	endproc

	procedure LinkAnnot_SetBorderStyle(annot as HPDF_Annotation	;
			,width as HPDF_REAL ;
			,dash_on as HPDF_UINT16 ;
			,dash_off as HPDF_UINT16) as HPDF_STATUS
		this.status = HPDF_LinkAnnot_SetBorderStyle(m.annot,m.width,m.dash_on,m.dash_off)
		return this.status = HPDF_OK
	endproc

	procedure TextAnnot_SetIcon( annot as HPDF_Annotation ;
			,icon as HPDF_Annotation) as HPDF_STATUS
		this.status = HPDF_TextAnnot_SetIcon(m.annot,m.icon)
		return this.status = HPDF_OK
	endproc

	procedure extAnnot_SetOpened(annot as HPDF_Annotation ;
			,opened as HPDF_BOOL ) as HPDF_STATUS
		this.status = HPDF_TextAnnot_SetOpened(m.annot,m.opened)
		return this.status = HPDF_OK
	endproc

	*!*************************************************************************
	*!* PDF document image handling functions
	*!*************************************************************************
	procedure LoadPngImageFromFile(filename as string) as HPDF_Image
		retval = HPDF_LoadPngImageFromFile(this.doc,m.filename)
		if m.retval = 0
			this.status = this.GetError()
		endif 
		return m.retval	
	endproc

	procedure LoadPngImageFromFile2(filename as string) as HPDF_Image
		m.retval = HPDF_LoadPngImageFromFile2(this.doc, m.filename)
		if m.retval = 0
			this.status = this.GetError()
		endif 
		return m.retval	
	endproc

	procedure LoadJpegImageFromFile(filename AS String ) as HPDF_Image
		retval = HPDF_LoadJpegImageFromFile(this.doc, m.filename)
		if m.retval = 0
			this.status = this.GetError()
		endif 
		return m.retval	
	endproc

	procedure LoadRawImageFromFile(filename as String ;
			,width as HPDF_UINT ;
			,height as HPDF_UINT ;
			,color_space as HPDF_ColorSpace)  as HPDF_Image
		retval = HPDF_LoadRawImageFromFile(this.doc,m.filename,m.width,m.height,m.color_space)
		if m.retval = 0
			this.status = this.GetError()
		endif 
		return m.retval	
	endproc

	procedure LoadRawImageFromMem(buf as HPDF_BYTE ;
			,width as HPDF_UINT ;
			,height as HPDF_UINT ;
			,color_space as HPDF_ColorSpace ;
			,bits_per_component as HPDF_UINT) as HPDF_Image
		retval = HPDF_LoadRawImageFromMem(this.doc,m.buf,m.width,m.height,m.color_space,m.bits_per_component)
		if m.retval = 0
			this.status = this.GetError()
		endif 
		return m.retval	
	endproc

	procedure Image_GetWidth(image as HPDF_Image) as HPDF_UINT
		return HPDF_Image_GetWidth(m.image)
	endproc

	procedure Image_GetHeight(image as HPDF_Image) as HPDF_UINT
		return HPDF_Image_GetHeight(m.image)
	endproc

	procedure Image_GetBitsPerComponent(image as HPDF_Image  )	as HPDF_UINT
		return HPDF_Image_GetBitsPerComponent(m.image)
	endproc

	procedure Image_GetColorSpace(image as HPDF_Image) as string
		return HPDF_Image_GetColorSpace (m.image)
	endproc

	procedure Image_SetColorMask(image as HPDF_Image ;
			,rmin as HPDF_UINT ;
			,rmax as HPDF_UINT ;
			,gmin as HPDF_UINT ;
			,gmax as HPDF_UINT ;
			,bmin as HPDF_UINT ;
			,bmax as HPDF_UINT	) as HPDF_STATUS
		this.status = HPDF_Image_SetColorMask(m.image,m.rmin,m.rmax,m.gmin,m.gmax,m.bmin,m.bmax)
		return this.status
	endproc

	procedure Image_SetMaskImage(image as HPDF_Image ;
			,mask_image as HPDF_Image) as HPDF_STATUS
		this.status = HPDF_Image_SetMaskImage(m.image, m.mask_image)
		return this.status
	endproc

	*!*************************************************************************
	*!* PDF document info attributes handling functions
	*!*************************************************************************

	procedure SetInfoAttr(type as HPDF_InfoType ;
			,value as String) as HPDF_STATUS
		this.status = HPDF_SetInfoAttr(this.doc,m.type,m.value)
		return this.status
	endproc

	procedure info_author_assign(cValue)
		if Vartype(cValue) = 'C'
			this.status = this.SetInfoAttr(HPDF_INFO_AUTHOR , m.cValue)
			this.info_author = m.cValue
		endif
	endproc

	procedure info_creator_assign(cValue)
		if Vartype(cValue) = 'C'
			this.status = this.SetInfoAttr(HPDF_INFO_CREATOR , m.cValue)
			this.info_creator = m.cValue
		endif
	endproc

	procedure info_producer_assign(cValue)
		if Vartype(cValue) = 'C'
			this.status = this.SetInfoAttr( HPDF_INFO_PRODUCER , m.cValue)
			this.info_producer = m.cValue
		endif
	endproc

	procedure info_title_assign(cValue)
		if Vartype(cValue) = 'C'
			this.status = this.SetInfoAttr( HPDF_INFO_TITLE, m.cValue)
			this.info_title = m.cValue
		endif
	endproc

	procedure info_subject_assign(cValue)
		if Vartype(cValue) = 'C'
			this.status = this.SetInfoAttr( HPDF_INFO_SUBJECT, m.cValue)
			this.info_title = m.cValue
		endif
	endproc

	*doesn't work :(
	*procedure info_keywords_assign(cValue)
	*	if Vartype(cValue) = 'C'
	*		this.status = this.SetInfoAttr( HPDF_INFO_KEYWORDS, m.cValue )
	*		this.info_keywords = m.cValue
	*	endif
	*endproc

	procedure GetInfoAttr(type as HPDF_InfoType ) as String
		return HPDF_GetInfoAttr(this.doc,type)
	endproc

	procedure SetInfoDateAttr(type as HPDF_InfoTypeas ;
			,value as Datetime) as HPDF_STATUS
		local year, month, day, hour, minutes, seconds, cStruct
		year = Year(m.value)
		month = Month(m.value)
		day = Day(m.value)
		hour = Hour(m.value)
		minutes = Minute(m.value)
		seconds = Sec(m.value)

		cStruct = BinToC(m.year,'4rs') ;
			+ BinToC(m.month ,'4rs') ;
			+ BinToC(m.day ,'4rs') ;
			+ BinToC(m.hour ,'4rs') ;
			+ BinToC(m.minutes ,'4rs') ;
			+ BinToC(m.seconds ,'4rs') + ' ' + Replicate(Chr(0),8)

		this.status = HPDF_SetInfoDateAttr(this.doc,m.type,m.cStruct)
		return this.status
	endproc

	*!*************************************************************************
	*!* PDF document encryption/permission handling functions
	*!*************************************************************************
	procedure SetPassword(owner_passwd as String ;
			,user_passwd as String ) as HPDF_STATUS
		this.status = HPDF_SetPassword(this.doc,m.owner_passwd,m.user_passwd)
		return this.status
	endproc

	procedure owner_passwd_assign(cPassword)
		if Vartype(cPassword) = 'C'
			this.status = this.SetPassword(this.doc,m.cPassword,this.user_passwd)
			this.owner_passwd = m.cValue
		endif
	endproc
	procedure user_passwd_assign(cPassword)
		if Vartype(cPassword) = 'C'
			this.status = this.SetPassword(this.doc,this.owner_passwd, m.cPassword)
			this.user_passwd = m.cValue
		endif
	endproc

	procedure SetPermission() as HPDF_STATUS
		local permission
		permission = Iif(this.enable_print,4,0) + Iif(this.enable_copy,16,0)
		this.status = HPDF_SetPermission(this.doc,m.permission)
		return this.status
	endproc

	procedure enable_print_assign(lValue)
		if Vartype(lValue) = 'L'
			this.enable_print = m.lValue
			this.SetPermission()
		endif
	endproc

	procedure enable_copy_assign(lValue)
		if Vartype(lValue) = 'L'
			this.enable_copy = m.lValue
			this.SetPermission()
		endif
	endproc

	procedure SetEncryptionMode() as HPDF_STATUS
		local mode,key_len
		mode = this.encrypt_mode
		key_len = this.key_length

		if this.encrypt_mode = 'R2'
			mode = HPDF_ENCRYPT_R2
			key_len = 5
		endif
		if this.encrypt_mode = 'R3'
			mode = HPDF_ENCRYPT_R3
			key_len = Max(this.key_length,16)
		endif
		this.status = HPDF_SetEncryptionMode(this.doc,m.mode,m.key_len)
		return this.status
	endproc

	*!*************************************************************************
	*!* PDF document compression setting function
	*!*************************************************************************
	procedure SetCompressionMode(mode as HPDF_UINT) as HPDF_STATUS
		this.status = HPDF_SetCompressionMode(this.doc, m.mode)
		return this.status
	endproc

	*!*************************************************************************
	*!* PDF document fonts / text functions
	*!*************************************************************************
	*
	*real_width must be passed by ref if needed
	procedure Font_MeasureText(font as HPDF_Font;
			,text as String ;
			,len as HPDF_UINT ;
			,width as HPDF_REAL;
			,font_size as HPDF_REAL;
			,char_space as HPDF_REAL ;
			,word_space as HPDF_REAL ;
			,wordwrap as HPDF_BOOL ;
			,real_width as HPDF_REAL ) as HPDF_UINT
		HPDF_Font_MeasureText(m.font,m.text,m.len,m.width,m.font_size,m.char_space,m.word_space,m.wordwrap,@m.real_width)
		return m.real_width
	endproc

	procedure Page_TextWidth(page as HPDF_Page,text as String) as HPDF_REAL
		return HPDF_Page_TextWidth(m.page,m.text)
	endproc

	procedure Page_GetHeight(page as HPDF_Page) as HPDF_REAL
		return HPDF_Page_GetHeight(m.page)
	endproc
	procedure Page_GetWidth(page as HPDF_Page) as HPDF_REAL
		return HPDF_Page_GetWidth(m.page)
	endproc

	*
	*real_width must be passed by ref if needed
	procedure Page_MeasureText(page as HPDF_Page ;
			,text as String ;
			,width as HPDF_REAL ;
			,wordwrap as HPDF_BOOL ;
			,real_width as HPDF_REAL ) as HPDF_UINT
		HPDF_Page_MeasureText(m.page,m.text,m.width,m.wordwrap,@m.real_width)
		return m.real_width
	endproc

	procedure Page_GetCurrentFont( page as HPDF_Page ) as HPDF_Font
		return HPDF_Page_GetCurrentFont(m.page)
	endproc 
	  
	procedure Page_GetCurrentFontSize(page as HPDF_Page) as HPDF_REAL
		return HPDF_Page_GetCurrentFontSize(m.page)
	endproc 
	
	
	*!*************************************************************************
	*!* PDF document graphics functions
	*!*************************************************************************
	procedure Page_SetLineWidth(page as HPDF_Page ;
			,line_width as HPDF_REAL  ) as HPDF_STATUS
		this.status = HPDF_Page_SetLineWidth(m.page,m.line_width)
		return this.status
	endproc
	
	procedure Page_GetLineWidth(page as HPDF_Page)
		return HPDF_Page_GetLineWidth(m.page)
	endproc 
		
	procedure Page_SetLineCap(page as HPDF_Page ;
			,line_cap as HPDF_LineCap) as HPDF_STATUS
		this.status =	HPDF_Page_SetLineCap(m.page,m.line_cap)
		return this.status
	endproc

	procedure Page_SetLineJoin(page as HPDF_Page ;
			,line_join as HPDF_LineJoin) as HPDF_STATUS
		this.status = HPDF_Page_SetLineJoin(m.page,m.line_join)
		return this.status
	endproc

	procedure Page_SetMiterLimit(page as HPDF_Page ;
			,miter_limit as HPDF_REAL) as HPDF_STATUS
		this.status =	HPDF_Page_SetMiterLimit(m.page,m.miter_limit)
		return this.status
	endproc

	procedure Page_SetDash(	page as HPDF_Page ;
			,dash_ptn as string ;				&&string of UINT16 points
			,num_param as HPDF_UINT ;
			,phase as HPDF_UINT) as HPDF_STATUS
		this.status = HPDF_Page_SetDash(m.page,m.dash_ptn,m.num_param,m.phase)
		return this.status
	endproc

	procedure Page_SetFlat(page as HPDF_Page ;
			,flatness as HPDF_REAL) as HPDF_STATUS
		this.status =	HPDF_Page_SetFlat(m.page,m.flatness)
		return this.status
	endproc

	procedure Page_MoveTo(page as HPDF_Page ;
			,x as HPDF_REAL ;
			,y as HPDF_REAL) as HPDF_STATUS
		this.status =	HPDF_Page_MoveTo(m.page,m.x,m.y)
		return this.status
	endproc


	procedure Page_LineTo(page as HPDF_Page ;
			,x as HPDF_REAL ;
			,y as HPDF_REAL ) as HPDF_STATUS
		this.status = HPDF_Page_LineTo(m.page,m.x,m.y)
		return this.status
	endproc


	procedure Page_CurveTo(  page as HPDF_Page ;
			,x1 as HPDF_REAL ;
			,y1 as HPDF_REAL ;
			,x2 as HPDF_REAL ;
			,y2 as HPDF_REAL ;
			,x3 as HPDF_REAL ;
			,y3 as HPDF_REAL ) as HPDF_STATUS
		this.status = HPDF_Page_CurveTo(m.page,m.x1,m.y1,m.x2,m.y2,m.x3,m.y3)
		return this.status
	endproc

	procedure Page_CurveTo2(page as HPDF_Page ;
			,x2 as HPDF_REAL	;
			,y2 as HPDF_REAL	;
			,x3 as HPDF_REAL	;
			,y3 as HPDF_REAL	) as HPDF_STATUS
		this.status = HPDF_Page_CurveTo2(m.page,m.x2,m.y2,m.x3,m.y3)
		return this.status
	endproc

	procedure Page_CurveTo3(page as HPDF_Page ;
			,x1 as HPDF_REAL  ;
			,y1 as HPDF_REAL  ;
			,x3 as HPDF_REAL  ;
			,y3 as HPDF_REAL  ) as HPDF_STATUS
		this.status =	HPDF_Page_CurveTo3(m.page,m.x1,m.y1,m.x3,m.y3)
		return this.status
	endproc

	procedure Page_ClosePath(page as HPDF_Page)as HPDF_STATUS
		this.status =	HPDF_Page_ClosePath(m.page)
		return this.status
	endproc

	procedure Page_Rectangle(page as HPDF_Page ;
			,x as HPDF_REAL ;
			,y as HPDF_REAL  ;
			,width as HPDF_REAL ;
			,height as HPDF_REAL ) 	as HPDF_STATUS
		this.status =	HPDF_Page_Rectangle(m.page,m.x,m.y,m.width,m.height)
		return this.status
	endproc

	*/*--- Special graphic state operator --------------------------------------*/
	procedure Page_GSave(page as HPDF_Page) as HPDF_STATUS
		this.status = HPDF_Page_GSave(m.page)
		return this.status
	endproc

	procedure Page_GRestore(page as HPDF_Page) as HPDF_STATUS
		this.status = HPDF_Page_GRestore(m.page)
		return this.status
	endproc

	procedure Page_Concat(page as HPDF_Page ;
			,a as HPDF_REAL ;
			,b as HPDF_REAL ;
			,c as HPDF_REAL	;
			,d as HPDF_REAL	;
			,x as HPDF_REAL	;
			,y as HPDF_REAL	) as HPDF_STATUS
		this.status = HPDF_Page_Concat(m.page,m.a,m.b,m.c,m.d,m.x,m.y)
		return this.status
	endproc

	procedure Page_SetCharSpace(page as HPDF_Page, value as HPDF_REAL ) as HPDF_STATUS
		this.status =	HPDF_Page_SetCharSpace(m.page,m.value)
		return this.status
	endproc

	procedure Page_SetWordSpace(page as HPDF_Page, value as HPDF_REAL ) as HPDF_STATUS
		this.status =	HPDF_Page_SetWordSpace(m.page,m.value)
		return this.status
	endproc

	procedure Page_SetHorizontalScalling(page as HPDF_Page, value as HPDF_REAL ) as HPDF_STATUS
		this.status =	HPDF_Page_SetHorizontalScalling(m.page,m.value)
		return this.status
	endproc
	procedure Page_SetTextLeading(page as HPDF_Page, value as HPDF_REAL ) as HPDF_STATUS
		this.status =	HPDF_Page_SetTextLeading(m.page,m.value)
		return this.status
	endproc
	procedure Page_SetTextRise(page as HPDF_Page, value as HPDF_REAL ) as HPDF_STATUS
		this.status =	HPDF_Page_SetTextRise(m.page,m.value)
		return this.status
	endproc

	*/*--- Path painting operator ---------------------------------------------*/
	procedure Page_Stroke(page as HPDF_Page)
		this.status =  HPDF_Page_Stroke(m.page)
		return this.status
	endproc

	procedure Page_ClosePathStroke(page as HPDF_Page)
		this.status =  HPDF_Page_ClosePathStroke(m.page)
		return this.status
	endproc

	procedure Page_Fill(page as HPDF_Page)
		this.status =  HPDF_Page_Fill(m.page)
		return this.status
	endproc

	procedure Page_Eofill(page as HPDF_Page)
		this.status =  HPDF_Page_Eofill(m.page)
		return this.status
	endproc

	procedure Page_FillStroke(page as HPDF_Page)
		this.status =  HPDF_Page_FillStroke (m.page)
		return this.status
	endproc

	procedure Page_EofillStroke(page as HPDF_Page)
		this.status =  HPDF_Page_EofillStroke(m.page)
		return this.status
	endproc

	procedure Page_ClosePathFillStroke(page as HPDF_Page)
		this.status =  HPDF_Page_ClosePathFillStroke(m.page)
		return this.status
	endproc

	procedure Page_ClosePathEofillStroke(page as HPDF_Page)
		this.status =  HPDF_Page_ClosePathEofillStroke(m.page)
		return this.status
	endproc

	procedure Page_EndPath(page as HPDF_Page)
		this.status =  HPDF_Page_EndPath(m.page)
		return this.status
	endproc

	procedure Page_BeginText(page as HPDF_Page  ) as HPDF_STATUS
		this.status = HPDF_Page_BeginText(m.page)
		return this.status
	endproc


	procedure Page_EndText(page as HPDF_Page) as HPDF_STATUS
		this.status =	HPDF_Page_EndText(m.page)
		return this.status
	endproc

	procedure Page_Clip(page as HPDF_Page) as HPDF_STATUS  
		this.status = HPDF_Page_Clip(m.page)
		return this.status 
	endproc

	procedure Page_Eoclip(page as HPDF_Page) as HPDF_STATUS  
		this.status = HPDF_Page_Eoclip(m.page)
		return this.status 
	endproc
	
	procedure Page_SetFontAndSize(page as HPDF_Page, font as HPDF_Font,size as HPDF_REAL ) as HPDF_STATUS
		this.status =	HPDF_Page_SetFontAndSize(m.page,m.font,m.size)
		return this.status
	endproc

	procedure Page_SetTextRenderingMode(page as HPDF_Page,mode as HPDF_TextRenderingMode) as HPDF_STATUS
		this.status =	HPDF_Page_SetTextRenderingMode(m.page,m.mode)
		return this.status
	endproc

	procedure Page_MoveTextPos(page as HPDF_Page,x as HPDF_REAL,y as HPDF_REAL) as HPDF_STATUS
		this.status =	HPDF_Page_MoveTextPos(m.page,m.x,m.y)
		return this.status
	endproc

	procedure Page_MoveTextPos2(page as HPDF_Page,x as HPDF_REAL,y as HPDF_REAL) as HPDF_STATUS
		this.status =	HPDF_Page_MoveTextPos2(m.page,m.x,m.y)
		return this.status
	endproc

	procedure Page_SetTextMatrix( page as HPDF_Page ;
								,a as HPDF_REAL ;
								,b as HPDF_REAL ;
								,c as HPDF_REAL ;
								,d as HPDF_REAL ;
								,x as HPDF_REAL ;
								,y as HPDF_REAL ) as HPDF_STATUS
		this.status = HPDF_Page_SetTextMatrix(m.page,m.a,m.b,m.c,m.d,m.x,m.y)
	endproc 

	procedure Page_MoveToNextLine(page as HPDF_Page) as HPDF_STATUS
		this.status =	HPDF_Page_MoveToNextLine(m.page)
		return this.status
	endproc

	procedure Page_ShowText(page as HPDF_Page, text as String ) as HPDF_STATUS
		this.status = HPDF_Page_ShowText(m.page,m.text)
		return this.status
	endproc

	procedure Page_ShowTextNextLine(page as HPDF_Page, text as String ) as HPDF_STATUS
		this.status = HPDF_Page_ShowTextNextLine(m.page,m.text)
		return this.status
	endproc

	procedure Page_SetGrayFill(page as HPDF_Page, gray as HPDF_REAL) as HPDF_STATUS
		this.status =	HPDF_Page_SetGrayFill(m.page,m.gray)
		return this.status
	endproc

	procedure Page_SetGrayStroke(page as HPDF_Page, gray as HPDF_REAL) as HPDF_STATUS
		this.status =	HPDF_Page_SetGrayStroke(m.page,m.gray)
		return this.status
	endproc

	procedure Page_SetRGBFill(page as HPDF_Page,r as HPDF_REAL,g as HPDF_REAL,b as HPDF_REAL) as HPDF_STATUS
		this.status =	HPDF_Page_SetRGBFill(m.page,m.r,m.g,m.b)
		return this.status
	endproc

	procedure Page_SetRGBStroke(page as HPDF_Page,r as HPDF_REAL,g as HPDF_REAL,b as HPDF_REAL) as HPDF_STATUS
		this.status =	HPDF_Page_SetRGBStroke(m.page,m.r,m.g,m.b)
		return this.status
	endproc

	procedure Page_SetCMYKFill(page as HPDF_Page ;
			,c as HPDF_REAL  ;
			,m as HPDF_REAL  ;
			,y as HPDF_REAL  ;
			,k as HPDF_REAL ) as HPDF_STATUS
		this.status =	HPDF_Page_SetCMYKFill(m.page,m.c,m.m,m.y,m.k)
		return this.status
	endproc

	procedure Page_SetCMYKStroke(page as HPDF_Page ;
			,c as HPDF_REAL  ;
			,m as HPDF_REAL  ;
			,y as HPDF_REAL  ;
			,k as HPDF_REAL ) as HPDF_STATUS
		this.status =	HPDF_Page_SetCMYKStroke(m.page,m.c,m.m,m.y,m.k)
		return this.status
	endproc

	procedure Page_DrawImage(page as HPDF_Page ;
			,image as HPDF_Image ;
			,x as HPDF_REAL ;
			,y as HPDF_REAL ;
			,width as HPDF_REAL	;
			,height as HPDF_REAL ) as HPDF_STATUS
		this.status =	HPDF_Page_DrawImage(m.page,m.image,m.x,m.y,m.width,m.height)
		return this.status
	endproc

	procedure Page_Circle(page as HPDF_Page ;
			,x as HPDF_REAL ;
			,y as HPDF_REAL	;
			,ray as HPDF_REAL ) as HPDF_STATUS
		this.status =	HPDF_Page_Circle(m.page,m.x,m.y,m.ray)
		return this.status
	endproc

	procedure Page_Ellipse( page as HPDF_Page ;
			,x as HPDF_REAL ;
			,y as HPDF_REAL ;
			,xray as HPDF_REAL ;
			,yray as HPDF_REAL ) as HPDF_STATUS
		this.status = HPDF_Page_Ellipse ( m.page, m.x, m.y, m.xray, m.yray )
		return this.status
	endproc  

	procedure Page_Arc(page as HPDF_Page ;
			,x as HPDF_REAL ;
			,y as HPDF_REAL	;
			,ray as HPDF_REAL ;
			,ang1 as HPDF_REAL ;
			,ang2 as HPDF_REAL ) as HPDF_STATUS
		this.status =	HPDF_Page_Arc( m.page, m.x, m.y, m.ray, m.ang1, m.ang2)
		return this.status
	endproc

	procedure Page_TextOut(page as HPDF_Page ;
			,xpos as HPDF_REAL ;
			,ypos as HPDF_REAL ;
			,text as String ) as HPDF_STATUS
		this.status =	HPDF_Page_TextOut( m.page, m.xpos, m.ypos, m.text)
		return this.status
	endproc

	procedure Page_TextRect(page as HPDF_Page ;
							,left as HPDF_REAL ;
							,top as HPDF_REAL ;
							,right as HPDF_REAL ;
							,bottom as HPDF_REAL ;
							,text as string ;
							,align as HPDF_TextAlignment ;
							,len as integer, style as string  ) as HPDF_STATUS
		this.status = HPDF_Page_TextRect( m.page, m.left, m.top, m.right, m.bottom, m.text, m.align, m.len )
		return this.status
	endproc

	procedure Page_ExecuteXObject(page as HPDF_Page, obj as HPDF_XObject) as HPDF_STATUS  
		this.status = HPDF_Page_ExecuteXObject( m.page, m.obj)
		return this.status
	endproc

	*this doesn't work, return page handle as integer and empty string 
	*
	*procedure Page_GetRGBFill(page as HPDF_Page) as HPDF_RGBColor 
	*	colorDWAddress = HPDF_Page_GetRGBFill(page)
	*	colorStruct = Sys(2600, colorDWAddress , 12)
	*	color = NewObject('Empty')
	*	AddProperty(color,'r',CToBin(Left(colorStruct ,4),'N'))
	*	AddProperty(color,'g',CToBin(Substr(colorStruct ,5,4),'N'))
	*	AddProperty(color,'b',CToBin(Substr(colorStruct ,9,4),'N'))
	*	return color 		

	procedure Declare_dlls
		*/*---------------------------------------------------------------------------*/
		*/*----- document handling ---------------------------------------------------*/

		declare HPDF_Doc HPDF_New in ((this.pdfLibrary)) integer, integer
		declare integer HPDF_Free in ((this.pdfLibrary)) HPDF_Doc pdf
		declare HPDF_STATUS HPDF_NewDoc in ((this.pdfLibrary)) HPDF_Doc pdf
		declare integer HPDF_FreeDoc in ((this.pdfLibrary)) HPDF_Doc pdf
		declare integer HPDF_FreeDocAll in (this.pdfLibrary) HPDF_Doc pdf
		declare HPDF_BOOL HPDF_HasDoc  in (this.pdfLibrary) HPDF_Doc pdf
		declare HPDF_STATUS HPDF_SaveToStream  in (this.pdfLibrary) HPDF_Doc pdf
		declare integer HPDF_GetStreamSize  in (this.pdfLibrary) HPDF_Doc pdf
		declare HPDF_STATUS HPDF_ReadFromStream  in (this.pdfLibrary) HPDF_Doc pdf, @ string buf , integer size
		declare HPDF_STATUS HPDF_ResetStream  in (this.pdfLibrary) HPDF_Doc pdf
		declare HPDF_STATUS HPDF_SaveToFile  in (this.pdfLibrary) HPDF_Doc pdf, string filename
		declare HPDF_STATUS HPDF_GetError  in (this.pdfLibrary) HPDF_Doc pdf
		declare integer HPDF_ResetError  in (this.pdfLibrary) HPDF_Doc pdf
		declare HPDF_Page HPDF_GetPageByIndex  in (this.pdfLibrary) HPDF_Doc pdf, integer index
		declare HPDF_PageLayout HPDF_GetPageLayout in (this.pdfLibrary) HPDF_Doc pdf
		declare HPDF_STATUS HPDF_SetPageLayout in (this.pdfLibrary) HPDF_Doc pdf ,integer layout
		declare HPDF_PageMode HPDF_GetPageMode in (this.pdfLibrary) HPDF_Doc pdf
		declare HPDF_STATUS HPDF_SetPageMode in (this.pdfLibrary) HPDF_Doc pdf, integer mode
		declare integer HPDF_GetViewerPreference in (this.pdfLibrary) HPDF_Doc pdf
		declare HPDF_STATUS HPDF_SetViewerPreference in (this.pdfLibrary) HPDF_Doc pdf, integer prefv
		declare HPDF_STATUS HPDF_SetOpenAction in (this.pdfLibrary) HPDF_Doc pdf, integer open_action

		*/*---------------------------------------------------------------------------*/
		*/*----- page handling -------------------------------------------------------*/

		declare HPDF_Page HPDF_GetCurrentPage in (this.pdfLibrary) HPDF_Doc  pdf
		declare HPDF_Page HPDF_AddPage in (this.pdfLibrary) HPDF_Doc  pdf
		declare HPDF_Page HPDF_InsertPage in (this.pdfLibrary) HPDF_Doc pdf, HPDF_Page pageBefore
		declare HPDF_STATUS HPDF_Page_SetWidth in (this.pdfLibrary) HPDF_Page   page,	HPDF_REAL   value
		declare HPDF_STATUS HPDF_Page_SetHeight in (this.pdfLibrary) HPDF_Page   page,	HPDF_REAL   value
		declare HPDF_STATUS HPDF_Page_SetHeight in (this.pdfLibrary) HPDF_Page   page,	HPDF_REAL   value
		declare HPDF_STATUS HPDF_Page_SetSize in (this.pdfLibrary) HPDF_Page  page,	HPDF_PageSizes  size ;
			,HPDF_PageDirection direction
		declare HPDF_STATUS HPDF_Page_SetRotate in (this.pdfLibrary) HPDF_Page	page,	HPDF_UINT16   angle
		
		*declare HPDF_Point HPDF_Page_GetCurrentPos in (this.pdfLibrary) HPDF_Page page &&doesn't work
		declare HPDF_STATUS HPDF_Page_GetCurrentPos2 in (this.pdfLibrary) HPDF_Page page, HPDF_Point  @ pos 
		declare HPDF_STATUS HPDF_Page_GetCurrentTextPos2 in (this.pdfLibrary) HPDF_Page page, HPDF_Point  @ pos 

		*/*---------------------------------------------------------------------------*/
		*/*----- font handling -------------------------------------------------------*/

		declare HPDF_Font HPDF_GetFont in (this.pdfLibrary) HPDF_Doc  pdf, string font_name, string  encoding_name
		declare string HPDF_LoadType1FontFromFile in (this.pdfLibrary) HPDF_Doc	pdf, string   afm_file_name ;
			,string	data_file_name
		declare string HPDF_LoadTTFontFromFile in (this.pdfLibrary) HPDF_Doc	pdf, string  file_name ;
			,HPDF_BOOL    embedding
		declare string HPDF_LoadTTFontFromFile2 in (this.pdfLibrary) HPDF_Doc pdf, string file_name ;
			,HPDF_UINT    index, HPDF_BOOL    embedding
		declare integer HPDF_AddPageLabel in (this.pdfLibrary) HPDF_Doc pdf,HPDF_UINT page_num,HPDF_PageNumStyle style ;
			,HPDF_UINT first_page, string prefix

		*/*--------------------------------------------------------------------------*/
		*/*----- outline ------------------------------------------------------------*/

		declare HPDF_STATUS HPDF_CreateOutline in (this.pdfLibrary) HPDF_Doc pdf,HPDF_Outline   parent ;
			,string title,	HPDF_Encoder encoder
		declare HPDF_STATUS HPDF_Outline_SetOpened in (this.pdfLibrary) HPDF_Outline outline,HPDF_BOOL opened
		declare HPDF_STATUS HPDF_Outline_SetDestination in (this.pdfLibrary) HPDF_Outline outline,HPDF_Destination  dst

		*/*--------------------------------------------------------------------------*/
		*/*----- destination --------------------------------------------------------*/

		declare HPDF_Destination HPDF_Page_CreateDestination in (this.pdfLibrary) HPDF_Page page
		declare HPDF_STATUS HPDF_Destination_SetXYZ in (this.pdfLibrary) HPDF_Destination  dst,HPDF_REAL left,HPDF_REAL top ;
			,HPDF_REAL zoom
		declare HPDF_STATUS HPDF_Destination_SetFit in (this.pdfLibrary) HPDF_Destination dst
		declare HPDF_STATUS HPDF_Destination_SetFitH in (this.pdfLibrary) HPDF_Destination  dst,HPDF_REAL top
		declare HPDF_STATUS HPDF_Destination_SetFitV in (this.pdfLibrary) HPDF_Destination  dst,HPDF_REAL left
		declare HPDF_STATUS HPDF_Destination_SetFitR in (this.pdfLibrary) HPDF_Destination  dst,HPDF_REAL left ;
			,HPDF_REAL bottom, HPDF_REAL right,HPDF_REAL top
		declare HPDF_STATUS HPDF_Destination_SetFitB in (this.pdfLibrary) HPDF_Destination  dst
		declare HPDF_STATUS HPDF_Destination_SetFitBH in (this.pdfLibrary) HPDF_Destination  dst ;
			,HPDF_REAL top
		declare HPDF_STATUS HPDF_Destination_SetFitBV in (this.pdfLibrary) HPDF_Destination  dst ;
			,HPDF_REAL left

		*/*--------------------------------------------------------------------------*/
		*/*----- encoder ------------------------------------------------------------*/

		declare HPDF_Encoder HPDF_GetEncoder in (this.pdfLibrary) HPDF_Doc pdf, string encoding_name
		declare HPDF_Encoder HPDF_GetCurrentEncoder in (this.pdfLibrary) HPDF_Doc   pdf
		declare HPDF_STATUS HPDF_SetCurrentEncoder in (this.pdfLibrary) HPDF_Doc pdf, string encoding_name
		declare HPDF_EncoderType HPDF_Encoder_GetType in (this.pdfLibrary) HPDF_Encoder   encoder

		declare HPDF_ByteType HPDF_Encoder_GetByteType in (this.pdfLibrary) HPDF_Encoder encoder ;
			,string text,HPDF_UINT index
		declare string HPDF_Encoder_GetUnicode in (this.pdfLibrary) HPDF_Encoder   encoder ;
			,string code
		declare HPDF_WritingMode HPDF_Encoder_GetWritingMode in (this.pdfLibrary) HPDF_Encoder   encoder
		declare HPDF_STATUS HPDF_UseJPEncodings in (this.pdfLibrary) HPDF_Doc   pdf
		declare HPDF_STATUS HPDF_UseKREncodings in (this.pdfLibrary) HPDF_Doc pdf
		declare HPDF_STATUS HPDF_UseCNSEncodings in (this.pdfLibrary) HPDF_Doc   pdf
		declare HPDF_STATUS HPDF_UseCNTEncodings  in (this.pdfLibrary) HPDF_Doc   pdf

		*/*--------------------------------------------------------------------------*/
		*/*----- annotation ---------------------------------------------------------*/
		declare HPDF_Annotation HPDF_Page_CreateTextAnnot in (this.pdfLibrary) HPDF_Page page ;
			,HPDF_Rect  rect, string text, HPDF_Encoder encoder
		declare HPDF_Annotation HPDF_Page_CreateLinkAnnot in (this.pdfLibrary) HPDF_Page page ;
			,HPDF_Rect          rect ;
			,HPDF_Destination   dst
		declare HPDF_Annotation HPDF_Page_CreateURILinkAnnot in (this.pdfLibrary) HPDF_Page     page ;
			,HPDF_Rect  rect ;
			,string 	url
		declare HPDF_STATUS HPDF_LinkAnnot_SetHighlightMode in (this.pdfLibrary) ;
			HPDF_Annotation  annot, HPDF_AnnotHighlightMode   mode
		declare HPDF_STATUS HPDF_LinkAnnot_SetBorderStyle in (this.pdfLibrary) HPDF_Annotation  annot ;
			,HPDF_REAL        width ;
			,HPDF_UINT16      dash_on ;
			,HPDF_UINT16      dash_off
		declare HPDF_STATUS HPDF_TextAnnot_SetIcon in (this.pdfLibrary) HPDF_Annotation   annot ;
			, HPDF_AnnotIcon    icon
		declare HPDF_STATUS HPDF_TextAnnot_SetOpened in (this.pdfLibrary) HPDF_Annotation   annot ;
			,HPDF_BOOL   opened

		*/*--------------------------------------------------------------------------*/
		*/*----- image data ---------------------------------------------------------*/

		declare HPDF_Image HPDF_LoadPngImageFromFile in (this.pdfLibrary) HPDF_Doc pdf, string filename
		declare HPDF_Image HPDF_LoadPngImageFromFile2 in (this.pdfLibrary) HPDF_Doc pdf, string filename
		declare HPDF_Image HPDF_LoadJpegImageFromFile in (this.pdfLibrary) HPDF_Doc pdf, string filename
		declare HPDF_Image HPDF_LoadRawImageFromFile in (this.pdfLibrary) HPDF_Doc pdf ;
			,string 			filename ;
			,HPDF_UINT          width ;
			,HPDF_UINT          height;
			,HPDF_ColorSpace    color_space
		declare HPDF_Image HPDF_LoadRawImageFromMem in (this.pdfLibrary) HPDF_Doc pdf ;
			,HPDF_BYTE buf ,HPDF_UINT width, HPDF_UINT height,HPDF_ColorSpace	color_space ;
			,HPDF_UINT	bits_per_component
		declare integer HPDF_Image_GetWidth in (this.pdfLibrary) HPDF_Image image
		declare integer HPDF_Image_GetHeight  in (this.pdfLibrary) HPDF_Image image
		declare integer HPDF_Image_GetBitsPerComponent in (this.pdfLibrary) HPDF_Image  image
		declare string HPDF_Image_GetColorSpace in (this.pdfLibrary) HPDF_Image  image
		declare HPDF_STATUS HPDF_Image_SetColorMask in (this.pdfLibrary) HPDF_Image image,HPDF_UINT	rmin ;
			,HPDF_UINT	rmax,HPDF_UINT	gmin,HPDF_UINT	gmax,HPDF_UINT	bmin,HPDF_UINT	bmax
		declare HPDF_STATUS HPDF_Image_SetMaskImage in (this.pdfLibrary) HPDF_Image image,HPDF_Image mask_image

		*/*--------------------------------------------------------------------------*/
		*/*----- info dictionary ----------------------------------------------------*/

		declare HPDF_STATUS HPDF_SetInfoAttr in (this.pdfLibrary) HPDF_Doc pdf,HPDF_InfoType type,string value
		declare string HPDF_GetInfoAttr in (this.pdfLibrary) HPDF_Doc pdf,HPDF_InfoType   type
		declare HPDF_STATUS HPDF_SetInfoDateAttr in (this.pdfLibrary) HPDF_Doc pdf ;
			,HPDF_InfoType type,HPDF_Date value

		*/*--------------------------------------------------------------------------*/
		*/*----- encryption ---------------------------------------------------------*/
		declare HPDF_STATUS HPDF_SetPassword in (this.pdfLibrary) HPDF_Doc pdf ;
			,string owner_passwd, string user_passwd
		declare HPDF_STATUS HPDF_SetPermission in (this.pdfLibrary) HPDF_Doc pdf, integer permission
		declare HPDF_STATUS HPDF_SetEncryptionMode in (this.pdfLibrary) HPDF_Doc pdf ;
			,HPDF_EncryptMode   mode,HPDF_UINT key_len

		*/*--------------------------------------------------------------------------*/
		*/*----- compression --------------------------------------------------------*/
		declare HPDF_STATUS HPDF_SetCompressionMode in (this.pdfLibrary) HPDF_Doc pdf, HPDF_UINT mode

		*/*--------------------------------------------------------------------------*/
		*/*----- fonts / text -------------------------------------------------------*/
		declare HPDF_UINT HPDF_Font_MeasureText in (this.pdfLibrary) HPDF_Font font ;
			,string text ;
			,HPDF_UINT len ;
			,HPDF_REAL width ;
			,HPDF_REAL font_size ;
			,HPDF_REAL char_space ;
			,HPDF_REAL word_space ;
			,HPDF_BOOL wordwrap ;
			,HPDF_REAL @ real_width 
			
		declare HPDF_REAL HPDF_Page_GetWidth in (this.pdfLibrary) HPDF_Page page
		declare HPDF_REAL HPDF_Page_GetHeight in (this.pdfLibrary) HPDF_Page page
		declare HPDF_REAL HPDF_Page_TextWidth in (this.pdfLibrary) HPDF_Page page, string text
		declare HPDF_Font HPDF_Page_GetCurrentFont in (this.pdfLibrary)	HPDF_Page page
		declare HPDF_UINT HPDF_Page_MeasureText in (this.pdfLibrary) HPDF_Page page ;
				,string text ;
				,HPDF_REAL width ;
				,HPDF_BOOL wordwrap ;
				,HPDF_REAL @ real_width
				
		declare HPDF_RGBColor HPDF_Page_GetRGBFill in (this.pdfLibrary) HPDF_Page page
		declare HPDF_Font HPDF_Page_GetCurrentFont in (this.pdfLibrary) HPDF_Page page
		declare HPDF_REAL HPDF_Page_GetCurrentFontSize  in (this.pdfLibrary) HPDF_Page page

		*/*----- GRAPHICS OPERATORS -------------------------------------------------*/

		*/*--- General graphics state ---------------------------------------------*/
		*/* w */
		declare HPDF_STATUS HPDF_Page_SetLineWidth in (this.pdfLibrary) HPDF_Page  page,HPDF_REAL  line_width
		declare HPDF_REAL HPDF_Page_GetLineWidth in (this.pdfLibrary) HPDF_Page   page
		*/* J */
		declare HPDF_STATUS HPDF_Page_SetLineCap in (this.pdfLibrary) HPDF_Page	 page,HPDF_LineCap  line_cap
		*/* j */
		declare HPDF_STATUS HPDF_Page_SetLineJoin in (this.pdfLibrary) HPDF_Page	  page,HPDF_LineJoin  line_join
		*/* M */
		declare HPDF_STATUS HPDF_Page_SetMiterLimit in (this.pdfLibrary) HPDF_Page  page,HPDF_REAL  miter_limit

		*/* d */
		declare HPDF_STATUS HPDF_Page_SetDash in (this.pdfLibrary) HPDF_Page  page, string  dash_ptn ;
			,HPDF_UINT	num_param, HPDF_UINT phase
		*/* ri --not implemented yet */
		*/* i */
		declare HPDF_STATUS HPDF_Page_SetFlat in (this.pdfLibrary) HPDF_Page page,HPDF_REAL	flatness
		*/*--- Path construction operator ------------------------------------------*/
		*/* m */
		declare HPDF_STATUS HPDF_Page_MoveTo in (this.pdfLibrary) HPDF_Page	page,HPDF_REAL	x,HPDF_REAL	y
		*/* l */
		declare HPDF_STATUS HPDF_Page_LineTo in (this.pdfLibrary) HPDF_Page	page ,HPDF_REAL x ;
			,HPDF_REAL y
		*/* c */
		declare HPDF_STATUS HPDF_Page_CurveTo in (this.pdfLibrary) HPDF_Page page,HPDF_REAL	x1 ;
			,HPDF_REAL	y1,	HPDF_REAL	x2 ,HPDF_REAL	y2, HPDF_REAL	x3 ,HPDF_REAL	y3
		*/* v */
		declare HPDF_STATUS HPDF_Page_CurveTo2 in (this.pdfLibrary) HPDF_Page page ;
			,HPDF_REAL	x2 ;
			,HPDF_REAL	y2 ;
			,HPDF_REAL	x3 ;
			,HPDF_REAL	y3
		*/* y */
		declare HPDF_STATUS HPDF_Page_CurveTo3 in (this.pdfLibrary) HPDF_Page  page ;
			,HPDF_REAL  x1 ;
			,HPDF_REAL  y1 ;
			,HPDF_REAL  x3 ;
			,HPDF_REAL  y3
		*/* h */
		declare HPDF_STATUS HPDF_Page_ClosePath in (this.pdfLibrary) HPDF_Page  page
		*/* re */
		declare HPDF_STATUS HPDF_Page_Rectangle in (this.pdfLibrary) HPDF_Page  page ;
			,HPDF_REAL  x;
			,HPDF_REAL  y;
			,HPDF_REAL  width;
			,HPDF_REAL  height

		*/*--- Special graphic state operator --------------------------------------*/
		*/* q */
		declare HPDF_STATUS HPDF_Page_GSave in (this.pdfLibrary) HPDF_Page	page
		*/* Q */
		declare HPDF_STATUS HPDF_Page_GRestore  in (this.pdfLibrary) HPDF_Page	page
		*/* cm */
		declare HPDF_STATUS HPDF_Page_Concat in (this.pdfLibrary) HPDF_Page	page ;
			,HPDF_REAL	a;
			,HPDF_REAL	b;
			,HPDF_REAL	c;
			,HPDF_REAL	d;
			,HPDF_REAL	x;
			,HPDF_REAL	y

		*/* Tc */
		declare HPDF_STATUS HPDF_Page_SetCharSpace  in (this.pdfLibrary) HPDF_Page  page, HPDF_REAL  value

		*/* Tw */
		declare HPDF_STATUS HPDF_Page_SetWordSpace in (this.pdfLibrary) HPDF_Page  page, HPDF_REAL  value
		*/* Tz */
		declare HPDF_STATUS HPDF_Page_SetHorizontalScalling  in (this.pdfLibrary) HPDF_Page  page, HPDF_REAL  value
		*/* TL */
		declare HPDF_STATUS HPDF_Page_SetTextLeading  in (this.pdfLibrary) HPDF_Page  page, HPDF_REAL  value
		*/* Ts */
		declare HPDF_STATUS HPDF_Page_SetTextRise in (this.pdfLibrary) HPDF_Page  page, HPDF_REAL  value
		*/*--- Path painting operator ---------------------------------------------*/
		*/* S */
		declare HPDF_STATUS HPDF_Page_Stroke in (this.pdfLibrary) HPDF_Page  page
		*/* s */
		declare HPDF_STATUS HPDF_Page_ClosePathStroke in (this.pdfLibrary) HPDF_Page  page
		*/* f */
		declare HPDF_STATUS HPDF_Page_Fill in (this.pdfLibrary) HPDF_Page  page
		*/* f* */
		declare HPDF_STATUS HPDF_Page_Eofill in (this.pdfLibrary) HPDF_Page  page
		*/* B */
		declare HPDF_STATUS HPDF_Page_FillStroke in (this.pdfLibrary) HPDF_Page  page
		*/* B* */
		declare HPDF_STATUS HPDF_Page_EofillStroke  in (this.pdfLibrary) HPDF_Page  page
		*/* b */
		declare HPDF_STATUS HPDF_Page_ClosePathFillStroke  in (this.pdfLibrary) HPDF_Page  page
		*/* b* */
		declare HPDF_STATUS HPDF_Page_ClosePathEofillStroke in (this.pdfLibrary) HPDF_Page  page
		*/* n */
		declare HPDF_STATUS HPDF_Page_EndPath in (this.pdfLibrary) HPDF_Page  page
		*/*--- Text object operator -----------------------------------------------*/
		*/* BT */
		declare HPDF_STATUS HPDF_Page_BeginText in (this.pdfLibrary) HPDF_Page  page
		*/* ET */
		declare HPDF_STATUS HPDF_Page_EndText in (this.pdfLibrary) HPDF_Page  page
		*/* W */
		declare HPDF_STATUS HPDF_Page_Clip in (this.pdfLibrary) HPDF_Page  page
		*/* W* */
		declare HPDF_STATUS HPDF_Page_Eoclip in (this.pdfLibrary) HPDF_Page  page

		*/*--- Text state ---------------------------------------------------------*/
		*/* Tf */
		declare HPDF_STATUS HPDF_Page_SetFontAndSize in (this.pdfLibrary) HPDF_Page  page;
			,HPDF_Font  font;
			,HPDF_REAL  size
		*/* Tr */
		declare HPDF_STATUS HPDF_Page_SetTextRenderingMode in (this.pdfLibrary) HPDF_Page page ;
			,HPDF_TextRenderingMode  mode
		*/*--- Text positioning ---------------------------------------------------*/
		*/* Td */
		declare HPDF_STATUS HPDF_Page_MoveTextPos in (this.pdfLibrary) HPDF_Page page ;
			,HPDF_REAL   x ;
			,HPDF_REAL   y
		*/* TD */
		declare HPDF_STATUS HPDF_Page_MoveTextPos2 in (this.pdfLibrary) HPDF_Page page ;
			,HPDF_REAL   x ;
			,HPDF_REAL   y

		*/* TM */
		declare HPDF_STATUS HPDF_Page_SetTextMatrix in (this.pdfLibrary) HPDF_Page page ;
			,HPDF_REAL a,HPDF_REAL    b,HPDF_REAL    c,HPDF_REAL    d,HPDF_REAL    x,HPDF_REAL    y

		*/* T* */
		declare HPDF_STATUS HPDF_Page_MoveToNextLine in (this.pdfLibrary) HPDF_Page  page
		*/*--- Text showing -------------------------------------------------------*/
		*/* Tj */
		declare HPDF_STATUS HPDF_Page_ShowText in (this.pdfLibrary) HPDF_Page page,string text
		*/* ' */
		declare HPDF_STATUS HPDF_Page_ShowTextNextLine in (this.pdfLibrary) HPDF_Page page,string text
		*/* g */
		declare HPDF_STATUS HPDF_Page_SetGrayFill in (this.pdfLibrary) HPDF_Page page,HPDF_REAL gray
		*/* G */
		declare HPDF_STATUS HPDF_Page_SetGrayStroke in (this.pdfLibrary) HPDF_Page page,HPDF_REAL gray
		*/* rg */
		declare HPDF_STATUS HPDF_Page_SetRGBFill in (this.pdfLibrary) HPDF_Page  page ;
			,HPDF_REAL  r ;
			,HPDF_REAL  g ;
			,HPDF_REAL  b
		*/* RG */
		declare HPDF_STATUS HPDF_Page_SetRGBStroke in (this.pdfLibrary) HPDF_Page  page ;
			,HPDF_REAL  r ;
			,HPDF_REAL  g ;
			,HPDF_REAL  b
		*/* k */
		declare HPDF_STATUS HPDF_Page_SetCMYKFill in (this.pdfLibrary) HPDF_Page page ;
			,HPDF_REAL  c ;
			,HPDF_REAL  m ;
			,HPDF_REAL  y ;
			,HPDF_REAL  k
		*/* K */
		declare HPDF_STATUS HPDF_Page_SetCMYKStroke  in (this.pdfLibrary) HPDF_Page page ;
			,HPDF_REAL  c ;
			,HPDF_REAL  m ;
			,HPDF_REAL  y ;
			,HPDF_REAL  k
		declare HPDF_STATUS HPDF_Page_Circle in (this.pdfLibrary) HPDF_Page	page ;
			,HPDF_REAL	 x ;
			,HPDF_REAL	 y ;
			,HPDF_REAL	 ray
	
		declare HPDF_STATUS HPDF_Page_Ellipse in (this.pdfLibrary) HPDF_Page page ;
			,HPDF_REAL   x ;
			,HPDF_REAL   y ;
			,HPDF_REAL  xray ;
			,HPDF_REAL  yray 
	
		declare HPDF_STATUS HPDF_Page_Arc in (this.pdfLibrary) HPDF_Page page ;
			,HPDF_REAL	x ;
			,HPDF_REAL	y ;
			,HPDF_REAL	ray ;
			,HPDF_REAL	ang1 ;
			,HPDF_REAL	ang2
		declare HPDF_STATUS HPDF_Page_DrawImage in (this.pdfLibrary) HPDF_Page	page ;
			,HPDF_Image image;
			,HPDF_REAL	x;
			,HPDF_REAL	y;
			,HPDF_REAL	width;
			,HPDF_REAL	height
			
		declare HPDF_STATUS HPDF_Page_TextRect in (this.pdfLibrary) HPDF_Page page ;
			,HPDF_REAL				left ;
			,HPDF_REAL				top ;
			,HPDF_REAL				right ;
			,HPDF_REAL				bottom ;
			,string					text ;
			,HPDF_TextAlignment   	align ;
			,integer len

		declare HPDF_STATUS HPDF_Page_TextOut in (this.pdfLibrary) HPDF_Page	page ;
			,HPDF_REAL	xpos ;
			,HPDF_REAL	ypos ;
			,string  text

		*/*--- XObjects -----------------------------------------------------------*//* 
		declare HPDF_STATUS HPDF_Page_ExecuteXObject in (this.pdfLibrary) HPDF_Page page ;
			,HPDF_XObject  obj 

	endproc

	procedure InitializeErrCodes

		with this.errors
			.Add([Internal error. The consistency of the data was lost.], "4097")
			.Add([Internal error. The consistency of the data was lost.], "4098")
			.Add([Internal error. The consistency of the data was lost.], "4099")
			.Add([The length of the data exceeds HPDF_LIMIT_MAX_STRING_LEN.], "4100")
			.Add([Cannot get a pallet data from PNG image.], "4101")
			.Add([], "4102")
			.Add([The count of elements of a dictionary exceeds HPDF_LIMIT_MAX_DICT_ELEMENT], "4103")
			.Add([Internal error. The consistency of the data was lost.], "4104")
			.Add([Internal error. The consistency of the data was lost.], "4105")
			.Add([Internal error. The consistency of the data was lost.], "4106")
			.Add([HPDF_SetPermission() OR HPDF_SetEncryptMode() was called before a password is set.], "4107")
			.Add([Internal error. The consistency of the data was lost.], "4108")
			.Add([], "4109")
			.Add([Tried to register a font that has been registered.], "4110")
			.Add([Cannot register a character to the japanese word wrap characters list.], "4111")
			.Add([], "4112")
			.Add([Tried to set the owner password to NULL.The owner password and user password is the same.], "4113")
			.Add([Internal error. The consistency of the data was lost.], "4115")
			.Add([The depth of the stack exceeded HPDF_LIMIT_MAX_GSTATE.], "4116")
			.Add([Memory allocation failed.], "4117")
			.Add([File processing failed. (A detailed code is set.)], "4118")
			.Add([Cannot open a file. (A detailed code is set.)], "4119")
			.Add([], "4120")
			.Add([Tried to load a font that has been registered.], "4121")
			.Add([The format of a font-file is invalid .Internal error. The consistency of the data was lost.], "4122")
			.Add([Cannot recognize a header of an afm file.], "4123")
			.Add([The specified annotation handle is invalid.], "4124")
			.Add([], "4125")
			.Add([Bit-per-component of a image which was set as mask-image is invalid.], "4126")
			.Add([Cannot recognize char-matrics-data  of an afm file.], "4127")
			.Add([1. The color_space parameter of HPDF_LoadRawImage is invalid.] + CR + [2. Color-space of a image which was set as mask-image is invalid.] + CR + [3. The function which is invalid in the present  ], "4128")
			.Add([Invalid value was set when invoking HPDF_SetCommpressionMode().], "4129")
			.Add([An invalid date-time value was set.], "4130")
			.Add([An invalid destination handle was set.], "4131")
			.Add([], "4132")
			.Add([An invalid document handle is set.], "4133")
			.Add([The function which is invalid in the present state was invoked.], "4134")
			.Add([An invalid encoder handle is set.], "4135")
			.Add([A combination between font and encoder is wrong.], "4136")
			.Add([], "4137")
			.Add([], "4138")
			.Add([An Invalid encoding name is specified.], "4139")
			.Add([The lengh of the key of encryption is invalid.], "4140")
			.Add([1. An invalid font handle was set.] + CR + [2. Unsupported font format. ], "4141")
			.Add([Internal error. The consistency of the data was lost.], "4142")
			.Add([A font which has the specified name is not found.], "4143")
			.Add([Unsupported image format.], "4144")
			.Add([Unsupported image format.], "4145")
			.Add([Cannot read a postscript-name from an afm file.], "4146")
			.Add([1. An invalid object is set.] + CR + [2. Internal error. The consistency of the data was lost. ], "4147")
			.Add([Internal error. The consistency of the data was lost.], "4148")
			.Add([1. Invoked HPDF_Image_SetColorMask() against the image-object which was set a mask-image.], "4149")
			.Add([An invalid outline-handle was specified.], "4150")
			.Add([An invalid page-handle was specified.], "4151")
			.Add([An invalid pages-handle was specified. (internel error)], "4152")
			.Add([An invalid value is set.], "4153")
			.Add([], "4154")
			.Add([Invalid PNG image format.], "4155")
			.Add([Internal error. The consistency of the data was lost.], "4156")
			.Add([Internal error. The "_FILE_NAME" entry for delayed loading is missing.], "4157")
			.Add([], "4158")
			.Add([Invalid .TTC file format.], "4159")
			.Add([The index parameter was exceed the number of included fonts], "4160")
			.Add([Cannot read a width-data from an afm file.], "4161")
			.Add([Internal error. The consistency of the data was lost.], "4162")
			.Add([An error has returned from PNGLIB while loading an image.], "4163")
			.Add([Internal error. The consistency of the data was lost.], "4164")
			.Add([Internal error. The consistency of the data was lost.], "4165")
			.Add([], "4166")
			.Add([], "4167")
			.Add([], "4168")
			.Add([Internal error. The consistency of the data was lost.], "4169")
			.Add([Internal error. The consistency of the data was lost.], "4170")
			.Add([Internal error. The consistency of the data was lost.], "4171")
			.Add([There are no graphics-states to be restored.], "4172")
			.Add([Internal error. The consistency of the data was lost.], "4173")
			.Add([The current font is not set.], "4174")
			.Add([An invalid font-handle was spacified.], "4175")
			.Add([An invalid font-size was set.], "4176")
			.Add([Invalid graphic mode.], "4177")
			.Add([Internal error. The consistency of the data was lost.], "4178")
			.Add([The specified value is not a multiple of 90.], "4179")
			.Add([An invalid page-size was set.], "4180")
			.Add([An invalid image-handle was set.], "4181")
			.Add([The specified value is out of range.], "4182")
			.Add([The specified value is out of range.], "4183")
			.Add([Unexpected EOF marker was detected.], "4184")
			.Add([Internal error. The consistency of the data was lost.], "4185")
			.Add([], "4186")
			.Add([The length of the specified text is too long.], "4187")
			.Add([The execution of a function was skipped because of other errors.], "4188")
			.Add([This font cannot be embedded. (restricted by license)], "4189")
			.Add([Unsupported ttf format. (cannot find unicode cmap.)], "4190")
			.Add([Unsupported ttf format.], "4191")
			.Add([Unsupported ttf format. (cannot find a necessary table)], "4192")
			.Add([Internal error. The consistency of the data was lost.], "4193")
			.Add([1. The library is not configured to use PNGLIB. 2. Internal error. The consistency of the data was lost.], "4194")
			.Add([Unsupported Jpeg format.], "4195")
			.Add([Failed to parse .PFB file.], "4196")
			.Add([Internal error. The consistency of the data was lost.], "4197")
			.Add([An error has occurred while executing a function of Zlib.], "4198")
			.Add([An error returned from Zlib.], "4199")
			.Add([An invalid URI was set.], "4200")
			.Add([An invalid page-layout was set.], "4201")
			.Add([An invalid page-mode was set.], "4208")
			.Add([An invalid page-num-style was set.], "4209")
			.Add([An invalid icon was set.], "4210")
			.Add([An invalid border-style was set.], "4211")
			.Add([An invalid page-direction was set.], "4212")
			.Add([An invalid font-handle was specified.], "4213")
		endwith	
	endproc 

enddefine


