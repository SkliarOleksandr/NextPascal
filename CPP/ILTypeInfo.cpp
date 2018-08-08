//---------------------------------------------------------------------------
#include "ILTypeInfo.h"
//---------------------------------------------------------------------------
namespace VM{

int GetRTTIStructSize(const TDataTypeID DataTypeID)
{
  int Size;
  switch (DataTypeID)
  {
	case dtEnum: Size = sizeof(TRTTIOrdinal); break;
	case dtRange: Size = sizeof(TRTTIOrdinal); break;
	case dtSet: Size = sizeof(TRTTIOrdinal); break;
	case dtStaticArray: Size = sizeof(TRTTIArray); break;
	case dtDynArray:
	case dtOpenArray: Size = sizeof(TRTTIDynArray); break;
	case dtPointer: Size = sizeof(TRTTIPointer); break;
	case dtWeakRef: Size = sizeof(TRTTIPointer); break;
	case dtRecord: Size = sizeof(TRTTIRecord); break;
	case dtClass: Size = sizeof(TRTTIClass); break;
	case dtClassOf: Size = sizeof(TRTTIPointer); break;
	case dtProcType: Size = sizeof(TRTTIProcType); break;
	case dtInterface: Size = sizeof(TRTTIInterface); break;
    default: Size = sizeof(TRTTIOrdinal);
  };
  return Size;
}

bool ClassInheritsFrom(const PRTTIClass ChildClass, const PRTTIClass AncestorClass)
{
  if (ChildClass == AncestorClass)
	return True;
  PRTTIClass Anc = PRTTIClass(ChildClass->Ancestor);
  while (Anc)
  {
	if (Anc == AncestorClass)
	  return True;
	Anc = PRTTIClass(Anc->Ancestor);
  };
  return False;
}

}

