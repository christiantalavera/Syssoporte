*!* Program: PDF Listener
*!* Author: Dorin Vasilescu
*!* Date:
*!* Copyright: 2007 Dorin Vasilescu
*!* Description: ReportListener implementation of _libpdf library  
*!* 
*!* Revision Information:
*!* 2007.07 - first beta release on Profox Romania, low response 

*!* ToDo - image conversion for non jpg/png images : DONE
*!*      - underline/strikethrough : limited support, only one line text objects

#include hpdf_types.h
#include hpdf_consts.h
#include pdflistener.h
  
define class pdfListener as UpdateListener of listener.vcx

	SendGDIPlusImage = 1

	*parent class custom props
	nLastPercent = 0		&& used to optimize DoStatus execution
							&&, only when percentDone changes
	thermFormWidth = 510
	thermFormHeight = 54
	thermMargin = 8
	includeSeconds = TRUE
	OutputType = -1
	
	*props used in PDF output
	*protected pages, fonts, font_substs, images, imagefiles, encodings, destinations, annotations ;
			, page_w, page_h, aFRXData[1], frxtextset, frxlineset, flagTextSet, flagLineSet, txtLeading   
	pages = .null.			&& Pages collection
	fonts = .null.			&& fonts collection
	font_substs = .null.	&& font substitutions, with one of the 14 native PDF fonts
	images = .null.			&& images collection
	imagefiles = .null.		&& saved images, to be deleted
	encodings = .null.		&& encodigs collections
	outlines = .null.		&& outlines nodes collection
	destinations = .null.	&& corresponding destinations to outlines nodes
	annotations = .null.	&& annotations collection

	oPdf = .null.			&& pdfGen instance, the PDF library wrapper 
	current_page = 0 		&& oPdf current page handle
	current_font = 0		&& oPdf current font handle
	
	page_w = 595.276		&& default width & height for A4
	page_h = 841.89		

	preview = TRUE			&& open PDF in viewer?
	txtLeading = 0			&& line spacing
		
	*these are replicated properties of pdfGen class
	*I need them in both places, I want that PDF lib able to be used independently 
	*info attributes
	info_author = ' '		&&pdf info
	info_creator = ' '
	info_producer = ' '
	info_title = ' '
	info_subject = ' '
	info_keywords = ' '

	*encryption
	owner_passwd = 'PDFListener'
	user_passwd = ''
	encrypt_mode = 'R3'		&& R2 or R3, R3 has 5 to 16 key length
	key_length = 16

	*permissions
	enable_print = TRUE
	enable_copy = TRUE
	
	*compression
	enable_compression = TRUE
		
	*pdf file, full path preferred
	pdfdoc = 'document.pdf'

	*font settings
	embed_fonts = TRUE			&& are subsets of TT fonts embedded in document?
	default_font = 'Helvetica'	&&default font if something goes wrong with font handling 
	encoding = 'CP1252'			&&default encoding

	* copy of previous frx record data for text/lines,
	*used to determine if changes in text/shape mode are needed 
	*(avoid unnecessary API calls for font/color settings)
	frxTextSet = .null.
	frxLineSet = .null.

	flagTextSet = TRUE		&& flag, force font/pen/fill settings time for text objects
	flagLineSet = TRUE		&&                  pen/fill                   lines/shapes

	JPEG_EncoderId = .null.
	
	*try to render each line individually, doesn't work as expected
	DynamicLineHeight = FALSE
	oGpImage = .null.		&&gdiplus instance

	****************
	procedure Init()
		
		local cId
		this.pages = Newobject('Collection')		&& Pages collection
		this.fonts = Newobject('Collection')		&& fonts collection
		this.font_substs = Newobject('Collection')	&& font substitutions
		this.images = Newobject('Collection')		&& images collection
		this.imagefiles = Newobject('Collection')	&& temporary images
		this.encodings = Newobject('Collection')	&& encodigs collections
		this.outlines = Newobject('Collection')		&& outlines node collection
		this.destinations = Newobject('Collection')	&& corresponding destinations to outlines nodes
		this.annotations = Newobject('Collection')	&& annotations collection

		*copy of frx data, collection of empty objects filled with SCATTER NAME
		Addproperty(this, 'aFRXData[1]', .null.)		
		
		*the "workhorse" of this class, PDF library
		this.oPdf = NewObject('pdfGen','_libpdf.prg')
		
		*the excellent Carlos Alloati progressbar class
		if not "ctl32_progressbar" $ lower(set("Classlib"))
			set classlib to ctl32_progressbar.vcx additive
		endif
		
		this.DeclareDLLs()

		*image conversion and temporary images stuff
		cID = space(20)
		pdfgen_CLSIDFromString(strconv(CLSID_JPG, 5), @cID)      
		this.JPEG_EncoderId = cID
		
		this.oGpImage = Newobject("GpImage", "_gdiplus.vcx")

		DoDefault()
		
	endproc 
	
	********************
	procedure BeforeReport
		local frxrec 
		
		*check file name, preview clauses
		if not Empty(this.CommandClauses.ToFile)
			this.pdfDoc = this.CommandClauses.ToFile
		endif
		
		this.preview = this.CommandClauses.Preview
		
		*reset internal handles collections and start a new pdf doc, set password, permissions
		this.Reset()

		*load fonts and FRX data
		this.SetFRXDataSession()
		
		this.LoadFonts()

		*load all records from frx into array property to avoid switching data session in Render()
		*
		select frx
		dimension this.aFRXData[Reccount('frx')]
		scan
			this.aFRXData[Recno()] = NewObject('empty')
			scatter memo name this.aFRXData[Recno()]
			frxrec = this.aFRXData[Recno()]
			frxrec.penred = Max( frxrec.penred,0 )
			frxrec.pengreen = Max( frxrec.pengreen,0 )
			frxrec.penblue = Max( frxrec.penblue, 0)
			if frxrec.mode = 0
				frxrec.fillred = Iif(frxrec.fillred = -1 , 255 , frxrec.fillred)
				frxrec.fillgreen = Iif(frxrec.fillgreen = -1 , 255 , frxrec.fillgreen)
				frxrec.fillblue = Iif(frxrec.fillblue = -1 , 255 , frxrec.fillblue)
			endif 
		endscan 
		
		this.frxTextSet = this.aFRXData[1]
		this.frxLineSet = this.aFRXData[1]
		
		this.SetCurrentDataSession()
		DoDefault()
	endproc
	
	
	***************
	procedure Reset
		
		*prepare report processing
		Dodefault()		
		this.pages = Newobject('Collection')		
		this.fonts = Newobject('Collection')		
		this.font_substs = Newobject('Collection')
		this.outlines = Newobject('Collection')		
		this.destinations = Newobject('Collection')
		this.annotations = Newobject('Collection')	
		this.images = NewObject('Collection')
		
		*start a new doc
		this.oPdf.FreeDoc()
		this.oPdf.doc = this.oPdf.New()

		if this.enable_compression 
			this.opdf.SetCompressionMode( HPDF_COMP_ALL )
		else
			this.opdf.SetCompressionMode( HPDF_COMP_NONE ) 
		endif 
		
		if not Empty(this.owner_passwd) or not Empty( this.user_passwd )
			this.oPdf.SetPassword( this.owner_passwd , this.user_passwd )
		endif 
		
		if not Empty(this.owner_passwd) or not Empty( this.user_passwd )
			this.oPdf.SetPermission()
		endif 
		this.oPdf.info_author = this.info_author 		
		this.oPdf.info_creator = this.info_creator
		this.oPdf.info_producer = this.info_producer
		this.oPdf.info_title = this.info_title
		this.oPdf.info_subject = this.info_subject
		this.oPdf.info_keywords = this.info_keywords
 
		
	endproc 
	
		
	******************************
	procedure Render(m.nFRXRecno ;
		, m.nLeft, m.nTop, m.nWidth, m.nHeight ;
		, m.nObjectContinuationType, m.cContentsToBeRendered, m.GDIPlusImage) 

		* 
		* a lot of code in this method, but I'm trying to avoid slowness caused by calling 
		* possible renderText(), renderLine(), renderShape(), context switching, stack, ...
		* Rendering is almost as fast as native one
	
		if this.TwoPassProcess and this.CurrentPass = 0
			Dodefault(m.nFRXRecno ;
				, m.nLeft, m.nTop, m.nWidth, m.nHeight ;
				, m.nObjectContinuationType, m.cContentsToBeRendered, m.GDIPlusImage) 
			return 
		endif 
	
	
		local txtAlignment, txtLeading, fontRegValue, cFontStyle, frx, cFontRepl ;
			,nL, nT, nR, nB, nX, nY, nW, nH, nRay ;
			,dash_mode, pattern_count, cTempFile
		
		if this.pages.Count < this.PageNo
			this.AddNewPage()
		endif 
		
		*get a copy of current frx record
		m.frx = this.aFRXData[m.nFRXRecno]

		*prepare coordinates
		m.nL = m.nLeft * CONV_FACTOR
		m.nT = this.page_h - m.nTop * CONV_FACTOR
		m.nR = (m.nLeft + m.nWidth) * CONV_FACTOR 
		m.nB = this.page_h - (m.nTop + m.nHeight) * CONV_FACTOR 
		m.nX = m.nL
		m.nY = m.nB
		m.nW = m.nWidth * CONV_FACTOR 
		m.nH = m.nHeight * CONV_FACTOR


		do case
		case Inlist(frx.objtype  ;
					, FRX_OBJTYP_FIELD ;
					, FRX_OBJTYP_LABEL )	
			
			*
			* text objects 
			* optimisations: 
			* 	using flagTextSet flag, API calls for setting font face/size are called only when font setting changed
			*	Page_SetFontAndSize() is executed only when font settings change
			
			if Empty( m.cContentsToBeRendered )
				* nothing to do 
				return 
			endif 
					
			*set font, based on handles added in BeforeReport, but only if different from previous 
			*   value stored in frxtextset
			* 
			m.fontRegValue = frx.fontface ; 
						+ Iif(Bittest(frx.FontStyle,0) , ' Bold', '' ) ;
						+ Iif(Bittest(frx.FontStyle,1) , ' Italic', '' )
				
			m.cFontStyle = Iif(Bittest(frx.FontStyle,0) , 'B', '' ) ;
						+ Iif(Bittest(frx.FontStyle,1) , ' I', '' ) ;
						+ Iif(Bittest(frx.FontStyle,2) , ' U', '' ) ;
						+ Iif(Bittest(frx.FontStyle,7) , ' S', '' ) 

			m.txtLeading = Fontmetric(2 , frx.FontFace , frx.fontsize, m.cFontStyle) * ;
								Iif( frx.objtype = FRX_OBJTYP_FIELD, LINE_ADJUST_FIELDS, LINE_ADJUST_LABELS) * ;
								Icase( frx.objtype = FRX_OBJTYP_FIELD, 1 ;
											  ,frx.spacing = 0,1 ;
											  ,frx.spacing = 1, 1.5 ;
											  ,frx.spacing = 2, 2, 1)
			m.top_adjust = Fontmetric(5 , frx.FontFace, frx.fontsize, m.cFontStyle)

			if this.flagTextSet or not frx.fontface == this.frxTextSet.fontface ;
				or not frx.fontsize = this.frxTextSet.fontsize or not frx.fontstyle = this.frxTextSet.fontstyle
						 
				this.current_font = this.fonts.Item( this.fonts.GetKey( m.fontRegValue) )
				this.oPdf.Page_SetFontAndSize(this.current_page, this.current_font, frx.fontsize)
	
			endif 
						
			*set line spacing, based on frx.spacing value			
			*
			if not txtLeading = this.txtLeading
				this.oPdf.Page_SetTextLeading(this.current_page, m.txtLeading )
				this.txtLeading = m.txtLeading
			endif 

			*set text alignment based on object type and settings from frx
			*
			txtAlignment = ;
				ICase( frx.objtype = FRX_OBJTYP_LABEL and not Empty(frx.picture) ;
					, Icase( '@I' $ frx.picture , HPDF_TALIGN_CENTER ;
							, '@J' $ frx.picture , HPDF_TALIGN_RIGHT, HPDF_TALIGN_LEFT ) ;
					,( frx.objtype = FRX_OBJTYP_FIELD );
							and not frx.offset = 0 ;
					, Icase( frx.offset = 2 , HPDF_TALIGN_CENTER ;
							, frx.offset = 1 , HPDF_TALIGN_RIGHT, HPDF_TALIGN_LEFT ) ;
					, HPDF_TALIGN_LEFT )
			
			* extra width of one char, seems that PDF API needs this, otherwise truncates text
			*
			m.nExtraW = Fontmetric(6 , frx.FontFace , frx.fontsize, m.cFontStyle) 
			m.nExtraWL = Icase(txtAlignment = HPDF_TALIGN_RIGHT, m.nExtraW ;
							,txtAlignment = HPDF_TALIGN_CENTER, m.nExtraW/2, 0)  
			m.nExtraWR = Icase(txtAlignment = HPDF_TALIGN_LEFT, m.nExtraW ;
							,txtAlignment = HPDF_TALIGN_CENTER, m.nExtraW/2, 0)  

			*check text filling color
			if frx.mode = 0 &&fill color set, background opaque
				if this.flagTextSet or not frx.fillred = this.frxTextSet.fillred ;
									  or not frx.fillgreen = this.frxTextSet.fillgreen ;
									  or not frx.fillblue = this.frxTextSet.fillblue
					this.oPdf.Page_SetRGBFill( this.current_page, frx.fillred/255, frx.fillgreen/255,frx.fillblue/255)
					this.oPdf.Page_Rectangle(this.current_page, m.nX, m.nY, m.nW, m.nH )
					this.oPdf.Page_Fill(this.current_page)

					*reset flag to force text filling color reassignment
					this.flagTextSet = TRUE
				endif
			endif
	
			if this.flagTextSet or not frx.penred = this.frxTextSet.penred ;
									  or not frx.pengreen = this.frxTextSet.pengreen ;
									  or not frx.penblue = this.frxTextSet.penblue
				*set page text pen color

				this.oPdf.Page_SetRGBFill( this.current_page, frx.penred/255, frx.pengreen/255,frx.penblue/255)
			endif  
			
			this.oPdf.Page_BeginText(this.current_page)
			this.oPdf.Page_TextRect(this.current_page, nL - nExtraWL  ;  
													, nT + top_adjust ;
													, nR + nExtraWR ;  
													, nB - txtLeading + top_adjust  ;  
													, Strconv( m.cContentsToBeRendered,6) ;
													, txtAlignment,0)
			this.oPdf.Page_EndText(this.current_page)
			
			
			*Temporary support for Underline/Strikethrough, but only first line of text objects
			*Need to be fully implemented in _libpdf.prg 
			*
			if 'U' $ cFontStyle		&&underline
				local underline_adjust
				m.underline_adjust = Fontmetric(2 , frx.FontFace , frx.fontsize, m.cFontStyle) ;
									- Fontmetric(3 , frx.FontFace , frx.fontsize, m.cFontStyle)/2
			

					
				this.oPdf.Page_SetRGBStroke( this.current_page, frx.penred/255, frx.pengreen/255,frx.penblue/255)
				this.oPdf.Page_SetLineWidth( this.current_page ;
						, Fontmetric(3, frx.FontFace , frx.fontsize, m.cFontStyle)/4)
						
				this.oPdf.Page_MoveTo( this.current_page , m.nL , m.nT - m.underline_adjust )
				this.oPdf.Page_LineTo( this.current_page , m.nR , m.nT - m.underline_adjust)
				this.oPdf.Page_Stroke(this.current_page)
			endif
			 				
			if 'S' $ cFontStyle		&&strikethrough
				local strike_adjust
				m.strike_adjust = Fontmetric(2 , frx.FontFace , frx.fontsize, m.cFontStyle) ;
									- Fontmetric(2 , frx.FontFace , frx.fontsize, m.cFontStyle)/2 ;
			
				this.oPdf.Page_SetRGBStroke( this.current_page, frx.penred/255, frx.pengreen/255,frx.penblue/255)
				this.oPdf.Page_SetLineWidth( this.current_page , 0)
						
				this.oPdf.Page_MoveTo( this.current_page , m.nL , m.nT - m.strike_adjust )
				this.oPdf.Page_LineTo( this.current_page , m.nR , m.nT - m.strike_adjust )
				this.oPdf.Page_Stroke(this.current_page)
			endif 				

			this.frxTextSet = this.aFRXData[m.nFRXRecno]

			*set flag to change text settings only if something changed
			if this.flagTextSet
				this.flagTextSet = FALSE
			endif 
			
			*but if next object is line/shape, force line/shape set
			this.flagLineSet = TRUE
		
		case Inlist(frx.objtype, FRX_OBJTYP_LINE, FRX_OBJTYP_RECTANGLE )  
			
			*line and rectangle objects
			
			* optimisations: 
			* 	using flagLineSet flag, API calls for setting drawing parameters are called only when changed
			*	Page_SetRGBStroke(), Page_SetLineWidth()

			*set dash mode, if necessary ( stored in frx.penpat field )
			if this.flagLineSet or not frx.penpat = this.frxLineSet.penpat
				dash_mode = chr0
				pattern_count = 0
			
				do case 
				case frx.penpat = FRX_PENPAT_SOLID 
					dash_mode = chr0
					pattern_count = 0
				case frx.penpat = FRX_PENPAT_DOTTED
					dash_mode = Chr(3)+chr0+chr0
					pattern_count = 1
				case frx.penpat = FRX_PENPAT_DASHED  
					dash_mode = Chr(18)+chr0+Chr(6)+chr0+chr0
					pattern_count = 2
				case frx.penpat = FRX_PENPAT_DASHDOT
					dash_mode = Chr(9)+CHR0+Chr(6)+CHR0+Chr(3)+CHR0+Chr(6)+CHR0+CHR0 
					pattern_count = 4
				case frx.penpat = FRX_PENPAT_DASHDOTDOT 
					dash_mode = Chr(9)+CHR0+Chr(3)+CHR0+Chr(3)+CHR0+Chr(3)+CHR0+Chr(3)+CHR0+Chr(3)+CHR0+CHR0   
					pattern_count = 6
				endcase   
				
				this.oPdf.Page_SetDash( this.current_page,  dash_mode , pattern_count , 0)
			endif 
			
			*pencolor settings 
			if this.flagLineSet or not frx.penred = this.frxLineSet.penred ;
									  or not frx.pengreen = this.frxLineSet.pengreen ;
									  or not frx.penblue = this.frxLineSet.penblue

				this.oPdf.Page_SetRGBStroke( this.current_page, frx.penred/255, frx.pengreen/255,frx.penblue/255)
			endif 

			
			*line pensize settings
			if this.flagLineSet or not frx.pensize = this.frxLineSet.pensize
				this.oPdf.Page_SetLineWidth( this.current_page, frx.pensize )
			endif 
			
			*draw simple lines
			if frx.objtype = FRX_OBJTYP_LINE
				if m.nHeight > m.nWidth 	&&vertical line
					this.oPdf.Page_MoveTo( this.current_page , m.nL, m.nB )
					this.oPdf.Page_LineTo( this.current_page , m.nL, m.nT )
				else 
					this.oPdf.Page_MoveTo( this.current_page , m.nL, m.nT )
					this.oPdf.Page_LineTo( this.current_page , m.nR , m.nT )
				endif 				
				this.oPdf.Page_Stroke(this.current_page)
			endif 
			
			*draw shapes
			if frx.objtype = FRX_OBJTYP_RECTANGLE

				if frx.mode = 0 &&fill color set, background opaque
					this.oPdf.Page_SetRGBFill( this.current_page, frx.fillred/255, frx.fillgreen/255,frx.fillblue/255)
				endif 
				
				do case
				case frx.offset = 0		&& simple rectangle

					this.oPdf.Page_Rectangle( this.current_page , nX, nY, nW, nH )

				case Between(frx.offset,1,98)	&&rounded corners
					
					*rounded corners using graphic path, slow ( not so slow though )
					nRay = Round(IIF( m.nW > m.nH, Min(frx.offset,Int(m.nH/2)), Min(frx.offset,Int(m.nW/2))) , 0)
				
					this.oPdf.Page_MoveTo( this.current_page , m.nX + m.nRay , m.nY )
					this.oPdf.Page_LineTo( this.current_page , m.nR - m.nRay , m.nY )
					this.oPdf.Page_CurveTo ( this.current_page, m.nR, m.nB, m.nR, m.nB, m.nR, m.nB + m.nRay ) 
					this.oPdf.Page_LineTo( this.current_page , m.nR , m.nT - m.nRay )
					this.oPdf.Page_CurveTo( this.current_page, m.nR, m.nT, m.nR, m.nT, m.nR - m.nRay , m.nT)
					this.oPdf.Page_LineTo( this.current_page, m.nX + m.nRay , m.nT )
					this.oPdf.Page_CurveTo( this.current_page, m.nX, m.nT, m.nX, m.nT, m.nX, m.nT - m.nRay )
					this.oPdf.Page_LineTo( this.current_page, m.nX , m.nY + m.nRay )
					this.oPdf.Page_CurveTo( this.current_page, m.nX, m.nY , m.nX, m.nY, m.nX + m.nRay , m.nY )

				case frx.offset = 99	&&ellipse/circle
					
					this.oPdf.Page_Ellipse( this.current_page, m.nX + m.nW/2 , m.nY + m.nH/2, m.nW/2, m.nH/2 )
					
				endcase 
			
				if frx.mode = 0 		&&fill color set, background opaque
					this.oPdf.Page_FillStroke(this.current_page)
				else
					this.oPdf.Page_Stroke(this.current_page)
				endif
				
			endif 

			*set flag to change line settings only if something changed
			this.frxLineSet = this.aFRXData[m.nFRXRecno]
			if this.flagLineSet
				this.flagLineSet = FALSE
			endif 
			
			*but if next object is text, force fonts/pen/fill set
			this.flagTextSet = TRUE
			
		case frx.objtype = FRX_OBJTYP_PICTURE 
			
			local m.imgHandle, m.imgFile
			local m.nImgHandle, m.result
			
			do case
			
			*picture from file, jpg and png supported
			case frx.offset = 0
				
				m.imgHandle = 0
				m.imgFile = Lower(frx.picture)
				
				*jpg and png natively supported by libharu
				if Inlist( Justext( m.imgFile) , 'jpg','png')
					
					if this.imagefiles.GetKey(m.imgFile) = 0
	
						m.cTempFile = Addbs(Sys(2023)) + Sys(2015) + '.' + Justext( m.imgFile )
						this.imagefiles.Add( m.cTempFile , Lower( m.imgFile ))
						Strtofile( Filetostr( m.imgFile ) , m.cTempFile )
					
					endif
						
					if this.images.GetKey( m.imgFile ) = 0
						if Lower(Justext(frx.picture)) = 'jpg' 
							m.imgHandle = this.oPdf.LoadJpegImageFromFile( this.imagefiles.Item( this.imagefiles.GetKey(m.imgFile)))
							if not m.imgHandle = 0
								try 
									this.images.Add(m.imgHandle , m.imgFile )
								catch
								endtry 
							endif 
						endif 
						if Lower(Justext(frx.picture)) = 'png' 
							m.imgHandle = this.oPdf.LoadPNGImageFromFile(this.imagefiles.Item( this.imagefiles.GetKey(m.imgFile)))
							if not imgHandle = 0
								try 
									this.images.Add(m.imgHandle , m.imgFile )
								catch
								endtry 
							endif 
						endif 
					else
						m.imgHandle = this.images.Item ( this.images.GetKey( m.imgFile ))
					endif
					
				*need conversion to jpg/png for images of another type
				*will use gdiplus to convert images
				else
				
					if this.imagefiles.GetKey(m.imgFile) = 0
	
						cTempFile = Addbs(Sys(2023)) + Sys(2015) + '.jpg' 
						this.oGpImage.CreateFromFile(Fullpath(m.imgFile))	
						this.oGpImage.SaveToFile(cTempFile,"image/jpeg", "quality=100")					
						imgHandle = this.oPdf.LoadJpegImageFromFile(m.cTempFile)
						this.imagefiles.Add( m.cTempFile , Lower( m.imgFile ))
					
					endif
						
					if this.images.GetKey( m.imgFile ) = 0
						m.imgHandle = this.oPdf.LoadJpegImageFromFile( this.imagefiles.Item( this.imagefiles.GetKey(m.imgFile)))
						if not m.imgHandle = 0
							try 
								this.images.Add(m.imgHandle , m.imgFile )
							catch
							endtry 
						endif 
					else
						m.imgHandle = this.images.Item ( this.images.GetKey( m.imgFile ))
					endif
				endif  

				*draw image on page
				if not imgHandle = 0
					this.oPdf.Page_DrawImage( this.current_page , m.imgHandle , m.nX, m.nY, m.nW, m.nH )
				endif 
					
			case (frx.offset = 1 or frx.offset = 2) and not m.GDIPlusImage = 0
			
				m.cTempFile = Lower(Addbs(Sys(2023)) + Sys(2015) + '.jpg')
				m.result = pdfgen_GdipSaveImageToFile(m.GDIPlusImage, Strconv(m.cTempFile,5)+CHR0, this.JPEG_EncoderId, 0)
				m.imgHandle = this.oPdf.LoadJpegImageFromFile(m.cTempFile)
				if not imgHandle = 0
					try 
						this.imagefiles.Add(m.cTempFile , m.cTempFile)
						this.oPdf.Page_DrawImage( this.current_page , m.imgHandle , m.nX, m.nY, m.nW, m.nH )
					catch
					endtry 
				endif 	
			endcase 
		
		endcase 
		
	endproc
	
	***********************
	procedure AfterReport()
		this.opdf.SaveToFile(this.pdfdoc)
	    if this.preview
	    	pdfgen_ShellExecute( 0,'Open',this.pdfDoc,'',JustPath(FullPath(this.pdfDoc)),1)
	    endif 
	    this.SetFRXDataSession()
	    use in frx
	    this.SetCurrentDataSession()
	    DoDefault()
	endproc
	
	********************
	procedure AddNewPage
		this.current_page = this.oPdf.AddPage()
		this.pages.Add(this.current_page)
		this.page_w = this.GetPageWidth() * CONV_FACTOR 
		this.page_h = this.GetPageHeight() * CONV_FACTOR 
		this.oPdf.Page_SetWidth( this.current_page, this.page_w )
		this.oPdf.Page_SetHeight( this.current_page, this.page_h )
		this.flagTextSet = TRUE
		this.flagLineSet = TRUE
	endproc 
	
	*********************
	procedure LoadFonts()
		local fontRegValue, hFont, i, iCnt, fontFRX, fontRepl 
		local array aFontSubst[1]

		this.SetFRXDataSession()
		
		*load default native font
		m.hFont = this.oPdf.GetFont( this.default_font , this.encoding )
		this.fonts.Add( hFont , this.default_font )
			
		*get font substitutes from page header user data
		locate for objtype = 9 and objcode = 1
		if not Empty(frx.user)
			m.iCnt = Alines(aFontSubst, frx.user)
			for m.i = 1 to m.iCnt

				if Left(aFontSubst[i],8) = 'PDFFONT:'
					m.fontFRX = Strextract(aFontSubst[i] , 'PDFFONT:' , '=')
					m.fontRepl = Strextract(aFontSubst[i],'=' )
					this.font_substs.Add( m.fontRepl, m.fontFRX )
					
					*check to see if this font is already loaded
					if this.fonts.GetKey( fontRepl ) = 0 
						m.hFont = this.oPdf.GetFont( m.fontRepl , this.encoding)
						this.fonts.Add( m.hFont , m.fontFRX )
					else
						m.hFont = this.fonts.Item( this.fonts.GetKey( m.fontRepl ))
						this.fonts.Add( m.hFont , m.fontFRX )
					endif 
				endif 
			endfor
		endif 
						
		

		*get fonts and styles from frx cursor
		select distinct Padr(fontface,255) as fontface ;
			, fontstyle from frx where objtype = FRX_OBJTYP_FONTRES into cursor frxfonts

		*then have them loaded by API
		scan for not Empty(fontface)
			m.fontRegValue = Trim(frxfonts.fontface) ;
				+ Iif(Bittest(frxfonts.FontStyle,0) , ' Bold', '' ) ;
				+ Iif(Bittest(frxfonts.FontStyle,1) , ' Italic', '' )

			if this.fonts.GetKey( fontRegValue ) = 0
				*check for native font substitute
				if this.font_substs.GetKey( fontRegValue ) = 0 
					*not found, load from font file
					this.AddFont( m.fontRegValue )
				else 
					this.fonts.Add( this.fonts.Item( this.font_substs.GetKey( m.fontRegValue )), m.fontRegValue)
				endif 
			endif
		endscan
		use in frxfonts
		select frx 
	endproc

	*********************************
	procedure AddFont( fontRegValue )
	
		local nHKey, cSubKey, nResult, lpszValueName, cData, nDataLen, cFontFile, cFontDesc, hFont
		local nKey, lpdwType
		
		m.nKey = 0
		m.cSubKey = Iif(Os(3) < '5'  ;
						, 'Software\Microsoft\Windows\CurrentVersion\Fonts' ;
						, 'Software\Microsoft\Windows NT\CurrentVersion\Fonts')

		pdfgen_RegOpenKey ( HKEY_LOCAL_MACHINE, @cSubKey, @nKey )
		m.cData = Space(255)
		m.nDataLen = 0
		m.lpszValueName = m.fontRegValue + ' (TrueType)'
		m.lpdwType = REG_SZ
	
		*get data length first
		pdfgen_RegQueryValueEx( nKey , lpszValueName, 0, @lpdwType , @cData, @nDataLen )
		m.cData = Space(m.nDataLen)
		*get key value
		pdfgen_RegQueryValueEx( m.nKey , m.lpszValueName, 0, @m.lpdwType , @m.cData, @m.nDataLen )
		m.cFontFile = GetEnv('windir')+'\Fonts\'+Left(m.cData,m.nDataLen-1)
		pdfgen_RegCloseKey(m.nKey)
		if File(m.cFontFile)
			m.cFontDesc = this.oPdf.LoadTTFontFromFile(m.cFontFile, this.embed_fonts )
			m.hFont = this.oPdf.GetFont(m.cFontDesc , this.encoding )
			this.fonts.Add(m.hFont, m.fontRegValue)
		else
			m.hFont = this.fonts.Item(1)
			this.fonts.Add(m.hFont, m.fontRegValue)
		endif 
					
		  
	endproc

	*********************
	procedure DeclareDLLs

		declare integer RegOpenKey in Win32API as pdfgen_RegOpenKey  ;
			integer nHKey, string @cSubKey, integer @nResult
		declare integer RegCloseKey in Win32API as pdfgen_RegCloseKey integer nHKey
		declare integer RegQueryValueEx in Win32API as pdfgen_RegQueryValueEx ;
			integer nHKey, string lpszValueName, integer dwReserved,;
			integer @lpdwType, string @cData, integer @nDataLen
		declare integer  ShellExecute in SHELL32.DLL as pdfgen_ShellExecute ;
			INTEGER nWinHandle ,;
		    STRING cOperation,;
	    	STRING cFileName,;
		    STRING cParameters,;
		    STRING cDirectory,;
	    	INTEGER nShowWindow
	    
	    *functions used to save general fields as jpegs in temporary files folder 
		declare integer GdipLoadImageFromFile IN GDIPLUS.DLL as pdfgen_GdipLoadImageFromFile ;
			 string filename, integer @imgHandle
		declare integer GdipSaveImageToFile in GDIPlus.DLL as pdfgen_GdipSaveImageToFile  ;
			integer image, string filename, string @ CLSID_clsidEncoder, ;
			integer encoderParams
		declare integer CLSIDFromString in ole32  as pdfgen_CLSIDFromString string, string @

	endproc
	
	*****************
	procedure Destroy
	
		if not Type('this.imagefiles.Count') = 'N'
			Dodefault()
			return 
		endif 
		
		if this.imagefiles.Count > 0
			
			local i
			for m.i = 1 to this.imagefiles.Count
				erase (this.imagefiles.Item(m.i))
			endfor
		endif 
		
		Dodefault()
		
	endproc 


	*********************
	procedure CreateTherm
		*modified to use Carlos Alloatti progress bar
		*
		if isnull(this.ThermForm)

			local liThermTop, liThermLeft, liThermWidth, liThermHeight, liSession
			if type("this.CommandClauses.StartDataSession") = "N"
				liSession = set("DATASESSION")
				try
					set datasession to (this.commandclauses.StartDataSession)
				catch when .t.
					this.resetDataSession()
				endtry
			endif
			this.ThermForm = CreateObject("FORM")


			with this.ThermForm
				.ScaleMode = SCALEMODE_PIXELS
				.Height = THIS.ThermFormHeight
				.HalfHeightCaption = .T.
				.Width = THIS.ThermFormWidth
				.AutoCenter = .T.
				.BorderStyle = BORDER_DOUBLE  && fixed dialog
				.ControlBox = .F.
				.Closable = (NOT THIS.IsRunning)
				.MaxButton = .F.
				.MinButton = .F.
				.Movable = (NOT THIS.IsRunning)
				.AlwaysOnTop = .T.
				.AllowOutput = .F.
				.AddObject("Therm","ctl32_progressbar")
				.AddObject("ThermLabel","ctl32_progressbarlabel")
				.ThermLabel.Visible = .T.
				.ThermLabel.FontBold = .T.
				.ThermLabel.Top = 4
				.ThermLabel.Width = .Width - (this.ThermMargin*2)
				.ThermLabel.Alignment = 2
				liThermHeight = .Height - (this.ThermMargin* 2) - .ThermLabel.Height
				liThermWidth =  .Width - (this.ThermMargin*2)
			endwith

			this.SetThermFormCaption()

			m.liThermTop = this.ThermMargin + 20
			m.liThermLeft = this.ThermMargin

			with this.ThermForm.Therm
				.Top = liThermTop
				.Left = liThermLeft
				.Height = liThermHeight
				.Width = liThermWidth
				.Visible = .T.
				.Caption = OUTPUTCLASS_RUNSTATUS_LOC
			endwith 

			if not empty(liSession)
				set datasession to (liSession)
			endif
		endif
			
		return not isnull(this.ThermForm)
	endproc
	
	******************************
	procedure DoStatus( cMessage )
		nodefault 
		LOCAL loParentForm, lcCaption, lcParentFormName
		IF (NOT (THIS.QuietMode or ;
				(THIS.IsRunning AND THIS.CommandClauses.NoDialog)))

			IF this.nlastpercent <> CEILING(this.percentDone*100)
				this.nlastpercent = CEILING(this.percentDone*100)
			ELSE
				RETURN
			ENDIF

			IF EMPTY(m.cMessage) OR ISNULL(m.cMessage)
				m.cMessage = ""
			ENDIF

			m.lcCaption = EVALUATE(THIS.ThermCaption)
			IF ((NOT ISNULL(THIS.ThermForm)) OR (THIS.CreateTherm()) )

				WITH THIS.ThermForm

					IF THIS.IsRunning
						.Closable = .F.
						.Movable = .F.
					ENDIF

					.Therm.Value = CEILING(THIS.PercentDone * 100)
					.ThermLabel.Caption = m.lcCaption

					IF NOT .Visible

						m.loParentForm = THIS.GetParentWindowRef()

						DO CASE
						CASE VARTYPE(m.loParentForm) # "O" AND (NOT _SCREEN.Visible)
							m.lcParentFormName = "MACDESKTOP"
						CASE VARTYPE(m.loParentForm) # "O"
							m.lcParentFormName = "SCREEN"
						CASE (NOT m.loParentForm.Visible) AND ;
								(m.loParentForm.DeskTop OR NOT EMPTY(m.loParentForm.MacDesktop) OR ;
								m.loParentForm.ShowWindow = 2 OR (NOT _SCREEN.Visible))
							m.lcParentFormName = "MACDESKTOP"
						CASE (NOT m.loParentForm.Visible)
							m.lcParentFormName = "SCREEN"
						OTHERWISE
							m.lcParentFormName = m.loParentForm.Name
						ENDCASE

						SHOW WINDOW (.Name) IN WINDOW (m.lcParentFormName)
						.AlwaysOnTop = .T.
						.AutoCenter = .T.
						.Visible = .T.

					ENDIF

				ENDWITH

			ENDIF

		ENDIF
	
	endproc

	*************************************
	procedure enable_print_assign(lValue)
		if Type('this.oPdf') = 'O'
			if not IsNull(this.oPdf) 
				if Vartype(lValue) = 'L'
					this.oPdf.enable_print = m.lValue
					this.oPdf.SetPermission()
				endif
			endif 
		endif
	endproc


	************************************
	procedure enable_copy_assign(lValue)
		if Type('this.oPdf') = 'O'
			if not IsNull(this.oPdf) 
				if Vartype(lValue) = 'L'
					this.oPdf.enable_copy = m.lValue
					this.oPdf.SetPermission()
				endif
			endif 
		endif
	endproc

enddefine 
