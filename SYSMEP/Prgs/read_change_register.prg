#DEFINE HKEY_CLASSES_ROOT           -2147483648  && BITSET(0,31)
#DEFINE HKEY_CURRENT_USER           -2147483647  && BITSET(0,31)+1
#DEFINE HKEY_LOCAL_MACHINE          -2147483646  && BITSET(0,31)+2
#DEFINE HKEY_USERS                  -2147483645  && BITSET(0,31)+3


cValue = []
oRegistry = CREATEOBJECT("Registry")
oRegistry.GetRegKey("kr_done1",@cValue,"Software\Notepad++",HKEY_CURRENT_USER)
cvalue = STR(INT(VAL(cvalue))+1)
oRegistry.setregkey("kr_done1",cvalue,"Software\Notepad++",HKEY_CURRENT_USER)

