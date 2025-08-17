**************************************************
*-- Class:        wizardexample (c:\siaf_vfp8\libs\sfbase.vcx)
*-- ParentClass:  bcform (c:\siaf_vfp8\libs\sfbase.vcx)
*-- BaseClass:    form
*-- Time Stamp:   08/09/06 05:00:03 PM
*
DEFINE CLASS WizardExample AS form
Caption = "Wizard Example"
Name = "Form1"

ADD OBJECT pageframe1 AS pageframe WITH ;
PageCount = 3, ;
BorderWidth = 0, ;
Top = 12, ;
Left = 12, ;
Width = 336, ;
Height = 169, ;
Tabs = .F., ;
Name = "Pageframe1", ;
Page1.Caption = "Page1", ;
Page1.Name = "Page1", ;
Page2.Caption = "Page2", ;
Page2.Name = "Page2", ;
Page3.Caption = "Page3", ;
Page3.Name = "Page3"

ADD OBJECT CmdPrev AS commandbutton WITH ;
Top = 216, ;
Left = 60, ;
Height = 27, ;
Width = 84, ;
Caption = "Previous", ;
Name = "CmdPrev"

ADD OBJECT CmdNext AS commandbutton WITH ;
Top = 216, ;
Left = 204, ;
Height = 27, ;
Width = 84, ;
Caption = "Next", ;
Name = "CmdNext"

PROCEDURE CmdPrev.Click
With THISFORM.Pageframe1
if .ActivePage > 1 then
.ActivePage = .ActivePage - 1
endif
Endwith
ENDPROC

PROCEDURE CmdNext.Click
With THISFORM.Pageframe1
if .ActivePage < .PageCount then
.ActivePage = .ActivePage + 1
endif
Endwith
endproc

PROCEDURE Init
With THISFORM.pageframe1
.Page1.AddObject('Label1','label')
With .Page1.Label1
.Caption = "Wizard Page 1"
.Height = 17
.Left = 11
.Top = 20
.Width = 97
.Visible = .T.
Endwith

.Page2.AddObject('Label1','label')
With .Page2.Label1
.Caption = "Wizard Page 2"
.Height = 17
.Left = 11
.Top = 20
.Width = 97
.Visible = .T.
Endwith

.Page3.AddObject('Label1','label')
With .Page3.Label1
.Caption = "Wizard Page 3"
.Height = 17
.Left = 11
.Top = 20
.Width = 97
.Visible = .T.
Endwith
Endwith
ENDPROC
ENDDEFINE
*-- EndDefine: wizardexample
**************************************************
