//---------------------------------------------------------------------------
#include <cstring>
#include "CRStreams.h"

#ifdef _MSC_BUILD
#include <sys/stat.h>
#include <io.h>
#endif

namespace CRBase
{

#ifndef __BORLANDC__

size_t TStream::GetPosition() const
{
  return FPosition;
}

/* TMemoryStream */
Pointer TMemoryStream::GetMemory() const
{
   return FMem;
}

size_t TMemoryStream::GetSize() const
{
  return FSize;
}

bool TMemoryStream::SetSize(size_t Size)
{
  if(!Size)
  {
    FSize = 0;
    delete[](FMem);
    return true;
  }
  bool success;
  if(auto Mem = new uint8_t[Size])
  {
    memcpy(Mem, FMem, FSize < Size ? FSize : Size);
    if(FMem) delete[](FMem);
    FMem = Mem;
    FSize = Size;
    success = true;
  }
  else
  {
    success = false;
  }
  return success;
}

size_t TMemoryStream::ReadToBuffer(Pointer buf, size_t len)
{
	size_t remain = FSize - FPosition;
    if(!remain) {
      return 0;
    }
    if(len > remain) {
      len = remain;
    }
	memcpy(buf, FMem + FPosition, len);
	FPosition += len;
	return len;
}

bool TMemoryStream::SetPosition(size_t Position)
{
	bool success;
    if(Position > FSize) {
      success = false;
    }
    else {
      FPosition = Position;
      success = true;
	}
    return success;
}

size_t TMemoryStream::CopyFrom(TStream* ts, size_t len)
{
  size_t remain = FSize - FPosition;
  if(!remain)
      return 0;
  if(len > remain) {
    len = remain;
  }
  auto readlen = ts->ReadToBuffer(Pointer(FMem + FPosition), len);
  FPosition += len;
  return readlen;
}

size_t TMemoryStream::CopyFrom(const Pointer data, size_t len)
{
  size_t remain = FSize - FPosition;
  //buffer is full
  if(!remain) {
    return 0;
  }
  //if remained capacity less than buffer size
  if(len > remain) {
    len = remain;
  }
  memcpy(FMem + FPosition, data, len);
  FPosition += len;
  return len;
}

#endif // !__BORLANDC__

/* TFileStream */

#ifdef STM32F7

size_t TFileStream::GetSize() const
{
  return f_size(&file_);
}

bool TFileStream::SetSize(size_t Size)
{
	bool success = false;
	size_t position = file_.fptr;
	if(Size > GetSize())
	{
		success = SetPosition(Size);
		SetPosition(position);
	}
	else if(Size < GetSize() && SetPosition(Size))
		success = !f_truncate(&file_);
	return success;
}

size_t TFileStream::GetPosition() const
{
  return f_tell(&file_);
}

bool TFileStream::SetPosition(size_t Position)
{
  auto res = f_lseek(&file_, Position);
  return !res;
}

size_t TFileStream::ReadToBuffer(Pointer buf, size_t len)
{
  size_t readlen = 0;
  auto fres = f_read(&file_, buf, len, &readlen);
  if(fres != FR_OK) {
    return 0;
  }
  FPosition = file_.fptr;
  return readlen;
}

size_t TFileStream::CopyFrom(const Pointer buf, size_t len)
{
  size_t bytesWritten;
  result_ = f_write(&file_, buf, len, &bytesWritten);
  return bytesWritten;
}

#endif //STM32F7

#ifdef _MSC_BUILD

size_t TFileStream::GetSize() const
{
  struct stat st;
  fstat(_fileno(file_), &st);
  return st.st_size;
}

bool TFileStream::SetSize(size_t Size)
{
  bool success = false;
//  auto position = ftell(file_);
  fflush(file_);
  auto handle = _get_osfhandle(_fileno(file_));
  success = !_chsize(handle, Size);
  return success;
}

size_t TFileStream::GetPosition() const
{
  return ftell(file_);
}

bool TFileStream::SetPosition(size_t Position)
{
  fpos_t curPos = Position;
  auto err = fsetpos(file_, &curPos);
  return !err;
}

size_t TFileStream::ReadToBuffer(Pointer buf, size_t len)
{
  auto readlen = fread(buf, 1, len, file_);
  return readlen;
}


#endif //_MSC_BUILD

Boolean ReadBoolean(TStream* Stream)
{
  Boolean Result;
  Stream->Read(&Result, sizeof(Result));
  return Result;
}

Int8 ReadInt8(TStream* Stream)
{
  Int8 Result;
  Stream->Read(&Result, sizeof(Result));
  return Result;
}

UInt8 ReadUInt8(TStream* Stream)
{
  UInt8 Result;
  Stream->Read(&Result, sizeof(Result));
  return Result;
}

Int16 ReadInt16(TStream* Stream)
{
  Int16 Result;
  Stream->Read(&Result, sizeof(Result));
  return Result;
}

UInt16 ReadUInt16(TStream* Stream)
{
  Int8 Result;
  Stream->Read(&Result, sizeof(Result));
  return Result;
}

Int32 ReadInt32(TStream* Stream)
{
  Int32 Result;
  Stream->Read(&Result, sizeof(Result));
  return Result;
}

UInt32 ReadUInt32(TStream* Stream)
{
  UInt32 Result;
  Stream->Read(&Result, sizeof(Result));
  return Result;
}

Int64 ReadInt64(TStream* Stream)
{
  Int64 Result;
  Stream->Read(&Result, sizeof(Result));
  return Result;
}

UInt64 ReadUInt64(TStream* Stream)
{
  UInt64 Result;
  Stream->Read(&Result, sizeof(Result));
  return Result;
}

String ReadString(TStream* Stream)
{
  (void)Stream;
  //String Result;
  //Stream->ReadBuffer(&Result, sizeof(Result));
  //return Result;
  return EMPTY_STR;
}

AnsiString ReadAnsiString(TStream* Stream)
{
  (void)Stream;
  return "";
}

String ReadUTF8String(TStream* Stream)
{
  (void)Stream;
  return EMPTY_STR;
}

String AsUTF8String(TStream* Stream)
{
  (void)Stream;
  return EMPTY_STR;
}

Float64 ReadFloat64(TStream* Stream)
{
  Float64 Result;
  Stream->Read(&Result, sizeof(Result));
  return Result;
}

Float32 ReadFloat32(TStream* Stream)
{
  Float32 Result;
  Stream->Read(&Result, sizeof(Result));
  return Result;
}

Int64 ReadLength(TStream* Stream)
{
  (void)Stream;
  return 0;
}

}//CRBase


//---------------------------------------------------------------------------
#ifdef __BORLANDC__
#pragma package(smart_init)
#endif
