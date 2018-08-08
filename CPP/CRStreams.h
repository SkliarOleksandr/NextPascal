//---------------------------------------------------------------------------

#ifndef CRStreamsH
#define CRStreamsH

#include "CRUtils.h"

#ifdef STM32F7
#include "swo.hpp"
#include "ff.h"
#elif defined(_MSC_BUILD)
#include <tchar.h>
#include <cstdio>
#include <string>
#endif

namespace CRBase
{

#ifndef __BORLANDC__

class TStream
{
protected:
  size_t FPosition;
  virtual bool IsValid() const = 0; //workaround for VS bug with simultaneous explicit and virtual
public:
  TStream() : FPosition{}
  {  }
  virtual size_t GetSize() const = 0;
  virtual bool SetSize(size_t Size) = 0;
  virtual size_t ReadToBuffer(Pointer buf, size_t len) = 0;
  size_t Read(Pointer buf, size_t len)
  {
    return ReadToBuffer(buf, len);
  }
  template<typename T>
  size_t ReadData(T& data)
  {
    return ReadToBuffer((void*)&data, sizeof(T));
  }
  template<typename T>
  size_t ReadData(T* data)
  {
    return ReadToBuffer((void*)data, sizeof(T));
  }
  virtual size_t CopyFrom(const Pointer buf, size_t len) = 0;
  template<typename T>
  size_t CopyFrom(const T& data)
  {
    return CopyFrom((void*)&data, sizeof(T));
  }
  template<typename T>
  size_t CopyFrom(const T* data)
  {
    return CopyFrom((void*)data, sizeof(T));
  }
  virtual size_t GetPosition() const;
  virtual bool SetPosition(size_t Position) = 0;
  explicit operator bool() const
  {
    return IsValid();
  }
  virtual ~TStream()
  { }
};

class TMemoryStream : public TStream
{
private:
  using Base = TStream;
  using Self = TMemoryStream;
  PByte FMem;
  size_t FSize;
  virtual bool IsValid() const override
  {
    return FMem != nullptr;
  }
public:
  using Base::CopyFrom;
  TMemoryStream() : FMem{nullptr}, FSize{0}
  { }
  TMemoryStream(size_t Size)
  {
    FMem = new uint8_t[Size];
    if(FMem)
    {
      FSize = Size;
      std::memset(FMem, 0, Size);
    }
  }
  TMemoryStream(Base& ts)
  {
    size_t sSize = ts.GetSize();
    if(sSize)
    {
      FMem = new uint8_t[sSize];
      if(FMem)
      {
        ts.SetPosition(0);
        FSize = sSize;
        std::memset(FMem, 0, FSize);
        ts.ReadToBuffer(FMem, FSize);
        ts.SetPosition(0);
      }
    }
    else
    {
      FMem = nullptr;
      FSize = 0;
    }
  }
  TMemoryStream(TMemoryStream& ts) : TMemoryStream{(Base&)ts}
  { }
  virtual Pointer GetMemory() const;
  virtual size_t GetSize() const override;
  virtual bool SetSize(size_t Size) override;
  virtual size_t ReadToBuffer(Pointer buf, size_t len) override;
  virtual bool SetPosition(size_t Position) override;
  size_t CopyFrom(TStream* ts, size_t len);
  virtual size_t CopyFrom(const Pointer data, size_t len) override;
  virtual ~TMemoryStream()
  {
	delete[](FMem);
  }
};

  static const Int8 fmOpenRead = Int8(0x0);
  static const Int8 fmOpenWrite = Int8(0x1);
  static const Int8 fmOpenReadWrite = Int8(0x2);
  static const Int8 fmExclusive = Int8(0x4);
  static const Int8 fmShareCompat = Int8(0x0);
  static const Int8 fmShareExclusive = Int8(0x10);
  static const Int8 fmShareDenyWrite = Int8(0x20);
  static const Int8 fmShareDenyRead = Int8(0x30);
  static const Int8 fmShareDenyNone = Int8(0x40);

#endif //!__BORLANDC__

#ifdef STM32F7

  class TFileStream : public TStream
  {
  private:
    uint8_t AccessModeConvert(int am) //windows cbuilder to fatfs format
    {
      switch(am)
      {
      case fmOpenRead: return FA_READ | FA_OPEN_EXISTING;
      case fmOpenWrite: return FA_WRITE | FA_OPEN_ALWAYS;
      case fmOpenReadWrite: return FA_READ | FA_WRITE | FA_OPEN_ALWAYS;
      case fmExclusive:
      case fmShareExclusive:
      case fmShareDenyWrite:
      case fmShareDenyRead:
      case fmShareDenyNone:
      default:
          ;
      }
      return FA_READ | FA_OPEN_EXISTING;
    }
    virtual bool IsValid() const override
    {
      return result_ == FR_OK;
    }
  protected:
    using Base = TStream;
    FIL file_;
    FRESULT result_;
  public:
    using Base::CopyFrom;
    TFileStream() : file_{}, result_{ FR_INVALID_PARAMETER }
    { }
    TFileStream(const char* FilePath, int Mode = fmOpenRead) : file_{}
    {
      auto mode_ = AccessModeConvert(Mode);
      result_ = f_open(&file_, FilePath, mode_);
    }
    virtual size_t GetSize() const override;
    virtual bool SetSize(size_t Size) override;
    virtual size_t ReadToBuffer(Pointer buf, size_t len) override;
    virtual size_t GetPosition() const override;
    virtual bool SetPosition(size_t Position) override;
    virtual size_t CopyFrom(const Pointer data, size_t Size) override;
    virtual ~TFileStream() override
    {
      f_close(&file_);
    }
    FRESULT Close()
    {
      return f_close(&file_);
    }

    FRESULT GetResult()
    {
      return result_;
    }
  };

#elif defined(_MSC_BUILD) 

  class TFileStream : public TStream
  {
  private:
    PChar AccessModeConvert(int am)
    {
      switch(am)
      {
      case fmOpenRead: return L_"rb";
      case fmOpenWrite: return L_"wb";
      case fmOpenReadWrite: return L_"rb+";
      case fmExclusive:
      case fmShareExclusive:
      case fmShareDenyWrite:
      case fmShareDenyRead:
      case fmShareDenyNone:
      default:
        ;
      }
      return L_"r";
    }
    virtual bool IsValid() const override
    {
      return !file_;
    }
  protected:
    using Base = TStream;
    String path_;
    FILE* file_;
  public:
    TFileStream() : path_{}, file_{}
    { }
    TFileStream(const String& filePath, Int8 Mode = fmOpenRead) : path_(filePath)
    {
      auto mode_ = AccessModeConvert(Mode);
      file_ = _tfopen(path_.data(), mode_);
      if(!file_) {
        throw std::runtime_error("fopen failed");
      }
    }
    virtual size_t GetSize() const override;
    virtual bool SetSize(size_t Size) override;
    virtual size_t ReadToBuffer(Pointer buf, size_t len) override;
    virtual size_t GetPosition() const override;
    virtual bool SetPosition(size_t Position) override;
    virtual size_t CopyFrom(const Pointer data, size_t len) override { return 0; } //FIXME: implement
    virtual ~TFileStream()
    {
      fclose(file_);
    }
  };

#endif //STM32F746xx & visual studio

  Boolean ReadBoolean(TStream*  Stream);
  Int8 ReadInt8(TStream*  Stream);
  UInt8 ReadUInt8(TStream*  Stream);
  Int16 ReadInt16(TStream*  Stream);
  UInt16 ReadUInt16(TStream*  Stream);
  Int32 ReadInt32(TStream*  Stream);
  UInt32 ReadUInt32(TStream*  Stream);
  Int64 ReadInt64(TStream*  Stream);
  UInt64 ReadUInt64(TStream*  Stream);
  CRBase::String ReadString(TStream*  Stream);
  CRBase::AnsiString ReadAnsiString(TStream*  Stream);
  CRBase::String ReadUTF8String(TStream*  Stream);
  CRBase::String AsUTF8String(TStream*  Stream);
  Float64 ReadFloat64(TStream*  Stream);
  Float32 ReadFloat32(TStream*  Stream);
  Int64 ReadLength(TStream*  Stream);

  void WriteBoolean(TStream* Stream, const Boolean Value);
  void WriteString(TStream* Stream, const CRBase::String Value);
  void WriteUTF8String(TStream* Stream, const CRBase::String Value);
  void WriteAnsiString(TStream* Stream, const CRBase::AnsiString Value);
  void WriteInt8(TStream* Stream, const Int8 Value);
  void WriteUInt8(TStream* Stream, const UInt8 Value);
  void WriteInt16(TStream* Stream, const Int16 Value);
  void WriteUInt16(TStream* Stream, const UInt16 Value);
  void WriteInt32(TStream* Stream, const Int32 Value);
  void WriteUInt32(TStream* Stream, const UInt32 Value);
  void WriteInt64(TStream* Stream, const Int64 Value);
  void WriteUInt64(TStream* Stream, const UInt64 Value);
  void WriteFloat64(TStream* Stream, const Float64 Value);
  void WriteFloat32(TStream* Stream, const Float32 Value);
  void WriteLength(TStream* Stream, const UInt64 Value);

}//CRBase




//---------------------------------------------------------------------------
#endif
