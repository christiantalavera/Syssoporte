	*** INSERTAR USUARIO 
SET EXCLUSIVE ON 	
IF FILE('menu_niv01.xml') AND FILE('menu_niv02.xml') AND FILE('menu_niv03.xml') THEN 
	XMLTOCURSOR('menu_niv01.xml','cur_menu01',512)
	XMLTOCURSOR('menu_niv02.xml','cur_menu02',512)
	XMLTOCURSOR('menu_niv03.xml','cur_menu03',512)	

	USE SYSSOPORTE!menu_niv01   	IN 0 
	USE SYSSOPORTE!menu_niv02		IN 0
	USE SYSSOPORTE!menu_niv03		IN 0 
	
	SELECT * FROM menu_niv01 INTO CURSOR cur_menu_01_bak READWRITE 
	SELECT * FROM menu_niv02 INTO CURSOR cur_menu_02_bak READWRITE 
	SELECT * FROM menu_niv03 INTO CURSOR cur_menu_03_bak READWRITE 
	
	SELECT distinct usuario FROM menu_niv01 INTO CURSOR cur_user READWRITE 
	INSERT INTO CUR_USER (usuario) VALUES ('00000000000000000000')

	ZAP IN menu_niv01
	ZAP IN menu_niv02
	ZAP IN menu_niv03
	
	SELECT cur_user
	SCAN ALL 
		lcCuser_id = cur_user.usuario	
		SELECT cur_menu01
		SCAN ALL 
			SCATTER MEMVAR 
			m.usuario = lcCuser_id
			IF !SEEK(m.ano_eje+m.sec_ejec+m.usuario+m.id_menuniv01,'menu_niv01','id_menu') THEN 
				INSERT INTO menu_niv01 FROM MEMVAR 
			ELSE
				REPLACE menu_niv01.des_menuniv1  WITH cur_menu01.des_menuniv1 
				REPLACE menu_niv01.estado		 WITH cur_menu01.estado
				REPLACE menu_niv01.visible		 WITH cur_menu01.visible
			ENDIF 
		ENDSCAN 

		SELECT cur_menu02
		SCAN ALL 
			SCATTER MEMVAR 
			m.usuario = lcCuser_id			
			IF !SEEK(m.ano_eje+m.sec_ejec+ALLTRIM(m.usuario)+m.id_menuniv01+m.id_menuniv02,'menu_niv02','menu_niv2') THEN 
				INSERT INTO menu_niv02 FROM MEMVAR 
			ELSE
				REPLACE menu_niv02.des_menuniv2		WITH cur_menu02.des_menuniv2
				REPLACE menu_niv02.ejecutaniv2		WITH cur_menu02.ejecutaniv2
				REPLACE menu_niv02.tipo_nodoniv2	WITH cur_menu02.tipo_nodoniv2
				REPLACE menu_niv02.estado		 	WITH cur_menu02.estado
				REPLACE menu_niv02.visible		 	WITH cur_menu02.visible
			ENDIF 
		ENDSCAN 

		SELECT cur_menu03
		SCAN ALL 
			SCATTER MEMVAR 
			m.usuario = lcCuser_id			
			IF !SEEK(m.ano_eje+m.sec_ejec+ALLTRIM(m.usuario)+m.id_menuniv01+m.id_menuniv02+m.id_menuniv03,'menu_niv03','id_menu') THEN 
				INSERT INTO menu_niv03 FROM MEMVAR 
			ELSE
				REPLACE menu_niv03.des_menuniv3		WITH cur_menu03.des_menuniv3	
				REPLACE menu_niv03.ejecutaniv3		WITH cur_menu03.ejecutaniv3
				REPLACE menu_niv03.estado		 	WITH cur_menu03.estado
				REPLACE menu_niv03.visible		 	WITH cur_menu03.visible
			ENDIF 
		ENDSCAN 
	ENDSCAN 
	
	SELECT cur_menu_01_bak 
	SCAN ALL 
		SCATTER MEMVAR 
		IF SEEK(m.ano_eje+m.sec_ejec+m.usuario+m.id_menuniv01,'menu_niv01','id_menu') THEN 
			REPLACE menu_niv01.estado		 WITH cur_menu_01_bak.estado
			REPLACE menu_niv01.visible		 WITH cur_menu_01_bak.visible
		ENDIF 
	ENDSCAN 

	SELECT cur_menu_02_bak 
	SCAN ALL 
		SCATTER MEMVAR 
		IF SEEK(m.ano_eje+m.sec_ejec+ALLTRIM(m.usuario)+m.id_menuniv01+m.id_menuniv02,'menu_niv02','menu_niv2') THEN 
			REPLACE menu_niv02.estado		 	WITH cur_menu_02_bak.estado
			REPLACE menu_niv02.visible		 	WITH cur_menu_02_bak.visible
		ENDIF 
	ENDSCAN 
	
	SELECT cur_menu_03_bak 
	SCAN ALL 
		SCATTER MEMVAR 
		IF SEEK(m.ano_eje+m.sec_ejec+ALLTRIM(m.usuario)+m.id_menuniv01+m.id_menuniv02+m.id_menuniv03,'menu_niv03','id_menu') THEN 	
			REPLACE menu_niv03.estado		 	WITH cur_menu_03_bak.estado
			REPLACE menu_niv03.visible		 	WITH cur_menu_03_bak.visible
		ENDIF 
	ENDSCAN 
	USE IN menu_niv01
	USE IN menu_niv02
	USE IN menu_niv03	
	USE IN cur_menu_01_bak 
	USE IN cur_menu_02_bak 
	USE IN cur_menu_03_bak 		
	=MESSAGEBOX('Proceso Terminado',64,'Aviso')
	DELETE FILE XML\menu_niv01.XML
	DELETE FILE XML\menu_niv02.XML
	DELETE FILE XML\menu_niv03.XML
ENDIF 	
 
SET EXCLUSIVE off