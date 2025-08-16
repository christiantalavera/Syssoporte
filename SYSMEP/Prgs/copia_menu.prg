IF gcano_eje = '2015' THEN 
	lcAno = '2015'
	SELECT * FROM menu_niv01 WHERE ano_eje = '2014' INTO CURSOR menu01 READWRITE 
	SELECT * FROM menu_niv02 WHERE ano_eje = '2014' INTO CURSOR menu02 READWRITE 
	SELECT * FROM menu_niv03 WHERE ano_eje = '2014' INTO CURSOR menu03 READWRITE 
	SELECT menu01
	REPLACE ALL ano_eje WITH lcAno
	SELECT menu02
	REPLACE ALL ano_eje WITH lcAno
	SELECT menu03
	REPLACE ALL ano_eje WITH lcAno
	SELECT menu01
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = lcAno
		IF !SEEK(m.ano_eje+m.cuser_id+m.id_menuniv01,'menu_niv01','id_menu') THEN 
			INSERT INTO menu_niv01 FROM MEMVAR 
		ENDIF 
	ENDSCAN 

	SELECT menu02
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = lcAno
		IF !SEEK(m.ano_eje+ALLTRIM(m.cuser_id)+m.id_menuniv01+m.id_menuniv02,'menu_niv02','menu_niv2') THEN 
			INSERT INTO menu_niv02 FROM MEMVAR 
		ENDIF 
	ENDSCAN 
	SELECT menu03
	SCAN ALL 
		SCATTER MEMVAR 
		m.ano_eje = lcAno
		IF !SEEK(m.ano_eje+ALLTRIM(m.cuser_id)+m.id_menuniv01+m.id_menuniv02+m.id_menuniv03,'menu_niv03','id_menu') THEN 
			INSERT INTO menu_niv03 FROM MEMVAR 
		ENDIF 
	ENDSCAN 
	USE IN menu_niv01
	USE IN menu_niv02
	USE IN menu_niv03	
	=MESSAGEBOX('Proceso Terminado',64,'Aviso')	
ENDIF
