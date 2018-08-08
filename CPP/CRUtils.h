//---------------------------------------------------------------------------

#ifndef CRUtilsH
#define CRUtilsH

#include <stdint.h>
#include <stdlib.h>
#include <string>
#include <cstring>
#include <algorithm>
#include "locale"
#include "codecvt"

#if !defined(__GNUG__)
  #define USE_UNICODE
#endif
#if defined __BORLANDC__
  #include <System.hpp>
  #include <System.Classes.hpp>
#endif

namespace CRBase {

typedef std::wstring	UnicodeString;
typedef std::string		AnsiString;

typedef wchar_t			UnicodeChar;
typedef char			AnsiChar;

typedef wchar_t*		PUnicodeChar;
typedef char*			PAnsiChar;

#ifdef USE_UNICODE
typedef UnicodeString	String;
typedef PUnicodeChar	PChar;
  #define L_ L""
  #define to_string to_wstring
#else
typedef AnsiString		String;
typedef PAnsiChar		PChar;
  #define L_ ""
#endif

const String EMPTY_STR = L_;

typedef int64_t              Int64;
typedef uint64_t             UInt64;

typedef float                Float32;
typedef double               Float64;

}//CRBase

#if !defined(__BORLANDC__)
  typedef bool           Boolean;
  typedef int            NativeInt;
  typedef unsigned int   NativeUInt;
  typedef NativeInt*     PNativeInt;
  typedef NativeUInt*    PNativeUInt;
  typedef uint8_t        Byte;
  typedef int8_t         Int8;
  typedef int16_t        Int16;
  typedef int32_t        Int32;


  typedef uint8_t        UInt8;
  typedef uint16_t       UInt16;
  typedef uint32_t       UInt32;

  typedef void*          Pointer;
  typedef Pointer*       PPointer;

  struct TGUID
  {
    UInt8 x[16];
  };

  struct Variant
  {
	  UInt8 x[16];
  };
  typedef Variant*       PVariant;

  class Exception : public std::exception
  {
    CRBase::String message_;
  public:
    explicit Exception(const char* str) : message_{str}
    { }
    explicit Exception(const CRBase::String& str) : message_{str}
    { }
    const char* what() const noexcept override
    {
      return message_.data();
    }
  };

  constexpr static bool True = true;
  constexpr static bool False = false;

/*class Exception
{
public:
	template<typename... Args>
	Exception(const CRBase::String& Message, Args...)
	{
		(void)Message;
	}
};*/

#else
  class EVMException: public Exception {
  public:
	using Exception::Exception;
	virtual const String what(){
	  return Message;
	}
  };
#endif

typedef UInt8*         PByte;

namespace CRBase {

static inline String Concat()
{
	return EMPTY_STR;
}
template<typename T1, typename... targs>
static inline String Concat(T1 head, targs... args)
{
	String S = String(head) + Concat(args...);
	return S;
}

static inline CRBase::String LowerCase(CRBase::String Str)
{
	std::transform(Str.begin(), Str.end(), Str.begin(), ::tolower);
	return Str;
}

static inline CRBase::AnsiString UnicodeToAnsi(const CRBase::String Str)
{
  #ifdef USE_UNICODE
	using codec = std::codecvt_utf8<wchar_t>;
	std::wstring_convert<codec, wchar_t> converter;
	return converter.to_bytes(Str);
  #else
	return Str;
  #endif
}
static inline CRBase::String AnsiToUnicode(const CRBase::PAnsiChar Str)
{
  #ifdef USE_UNICODE
	using codec = std::codecvt_utf8<wchar_t>;
	std::wstring_convert<codec, wchar_t> converter;
	return converter.from_bytes(Str);
  #else
	return Str;
  #endif
}

static inline CRBase::String AnsiToUnicode(const CRBase::AnsiString Str)
{
  #ifdef USE_UNICODE
	return AnsiToUnicode((const CRBase::PAnsiChar)Str.data());
  #else
	return Str;
  #endif
}

}//CRBase



  namespace _Impl {
	template<typename F>
	struct Final_action
	{
	  Final_action(F f): clean{f}
	  {   }
	  ~Final_action()
	  {
		clean();
	  }
	  F clean;
	};
  }

  /*  Usage example:
   *  auto final_action = finally([&]{ close(&handle1); close(&handle2) }
   */
  template<typename F>
  auto finally(F f) -> decltype(_Impl::Final_action<F>(f))
  {
	return _Impl::Final_action<F>(f);
  }


#if defined(__GNUG__)
namespace Utils {
  using std::string;
}//Utils
#endif


//---------------------------------------------------------------------------
#endif
