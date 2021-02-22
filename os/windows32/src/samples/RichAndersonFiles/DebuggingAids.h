#ifndef __DebubbingAids_h__
#define __DebubbingAids_h__

/********************************************************************
	filename: 	DebubbingAids
	file ext:	h
	author:		richard.andersen

	purpose:
	Copyright (C) 2006 - All Rights Reserved
*********************************************************************/

#include <ctype.h>
#include <sstream>
#include <assert.h>
#include <vector>
#include <boost/shared_ptr.hpp>
#include <string>

namespace Tools {

	#ifdef _DEBUG
	//Debug Items

	#ifndef TYPE_DEBUG_AID
		template<typename T>
		struct TypeInfo
		{
			const char* _typename_;

			TypeInfo()
				: _typename_( typeid(T).name() )
			{}
		};

		#define TYPE_DEBUG_AID(_T_) Tools::TypeInfo<_T_> __typeinfo_##_T_ ;
	#endif

	namespace MSWIN {

		extern void setthreadname(unsigned long dwThreadID, const char* szThreadName);
		extern void displaydebugstring(const char* szDebug);
		extern std::string GetFileName(const char* path);
		extern std::string CreateDebugString(const char* title,
												const char* statement = NULL,
												const char* codeStatement = NULL,
												const char* functionName = NULL,
												const char* fileName = NULL,
												int lineNbr = 0,
												const char* valueStr = NULL,
												bool getErrorNo = false);

		template<typename T>
		inline T& reference_cast(T* pointerT) { return *pointerT; }

		template<typename T>
		inline T& reference_cast(T& refT) { return refT; }

		template<typename T>
		inline T& reference_cast(const boost::shared_ptr<T>& refT) { return **refT; }

		template<typename T>
		inline const T& reference_cast(const T* pointerT) { return *pointerT; }

		template<typename T>
		inline const T& reference_cast(const T& refT) { return refT; }
	}

	//dwThreadID -- If -1 current thread
	inline void SetThreadName(unsigned long dwThreadID, const char* szThreadName)
	{
		MSWIN::setthreadname(dwThreadID, szThreadName);
	}

	inline void SetThreadName(const char* szThreadName)
	{
		MSWIN::setthreadname( -1, szThreadName);
	}


	#define COMPILER_ERROR(_ErrorReason_) typedef char ERROR_##_ErrorReason_ [-1];
	#define COMPILER_ERROR_COND(_Cond_, _ErrorReason_) typedef char ERROR_##_ErrorReason_ [ _Cond_ ? 1 : -1];

	#define COMPILER_DEFINE_INTERFACE_CHECK(__ClassName__) protected: typedef int ERROR_Must_Inherit_From_##__ClassName__;
	#define COMPILER_ERROR_COND_INTERFACE(__BaseClassName__) private: typedef __BaseClassName__::ERROR_Must_Inherit_From_##__BaseClassName__ BaseClass##__BaseClassName__;
	#define COMPILER_ERROR_COND_INTERFACE2(__NameSpace__, __BaseClassName__) private: typedef __NameSpace__::__BaseClassName__::ERROR_Must_Inherit_From_##__BaseClassName__ BaseClass##__BaseClassName__;

	inline void DisplayDebugString(const char* szDebug)
	{
		MSWIN::displaydebugstring(szDebug);
	}

	template<typename T>
	inline T DebugCond(T cond, const char* statement, const char* codeStatement, const char* functionName, const char* fileName, int lineNbr)
	{
		if(!cond)
		{
			std::stringstream valueStr;
			valueStr << cond;

			MSWIN::CreateDebugString("DebugCond:", statement, codeStatement, functionName, fileName, lineNbr, valueStr.str().c_str(), true);
		}
		return cond;
	}

	template<typename T>
	inline T DebugTrace(T result, const char* statement, const char* codeStatement, const char* functionName, const char* fileName, int lineNbr)
	{
		std::stringstream valueStr;
		valueStr << result;

		MSWIN::CreateDebugString("Trace:", statement, codeStatement, functionName, fileName, lineNbr, valueStr.str().c_str(), true);
		return result;
	}

	inline void DebugTrace(const char* statement, const char* codeStatement, const char* functionName, const char* fileName, int lineNbr)
	{
		MSWIN::CreateDebugString("Trace:", statement, codeStatement, functionName, fileName, lineNbr, "void", true);
	}

	template<typename T>
	inline T AssertCheck(T cond, const char* codeStatement, const char* functionName, const char* fileName, int lineNbr)
	{
		if(!cond)
		{
			std::stringstream valueStr;
			valueStr << cond;

			MSWIN::CreateDebugString("Assert:", NULL, codeStatement, functionName, fileName, lineNbr, valueStr.str().c_str(), true);
			assert(false);
		}
		return cond;
	}

	inline void NotAllowed(const char* statement, const char* codeStatement, const char* functionName, const char* fileName, int lineNbr)
	{
		MSWIN::CreateDebugString("Not Alowed:", statement, codeStatement, functionName, fileName, lineNbr);
		assert(false);
	}

	template<typename T>
	std::string DebugObject(const T& debugObject)
	{
		return debugObject.ToString().c_str();
	}

	template<typename T>
	std::string DebugObject(const std::vector<T>& debugObject)
	{
		std::string toString = "vector{";
		for each(const T value in debugObject)
		{
			toString += value.ToString();
			toString += ", ";
		}
		toString.replace(toString.length()-2, 2, "}");
		return toString;
	}

	template<typename T>
	void DebugStrings(const char* szcomment, const T& value,
						const char* functionName = NULL,
						const char* fileName = NULL,
						int lineNbr = 0)
	{
		std::stringstream debugStr;
		debugStr << szcomment;
		debugStr << value;

		Tools::MSWIN::CreateDebugString("Debug String:", debugStr.str().c_str(), NULL, functionName, fileName, lineNbr);
	}

	template<typename T, typename Y>
	void DebugStrings(const char* szcomment1, const T& value1, const char* szcomment2, const Y& value2,
		const char* functionName = NULL,
		const char* fileName = NULL,
		int lineNbr = 0)
	{
		std::stringstream debugStr;
		debugStr << szcomment1;
		debugStr << value1;
		debugStr << szcomment2;
		debugStr << value2;

		Tools::MSWIN::CreateDebugString("Debug String:", debugStr.str().c_str(), NULL, functionName, fileName, lineNbr);
	}

	#ifndef __FUNCTION__
	#define __FUNCTION__ ""
	#endif

	#define DEBUGCOND(_cond_, _statement_) Tools::DebugCond(_cond_, _statement_, #_cond_, __FUNCTION__, __FILE__, __LINE__)
	#define DEBUGSTRING(_str_)  Tools::MSWIN::CreateDebugString("Debug String:", _str_, NULL, __FUNCTION__, __FILE__, __LINE__);
	#define DEBUGSTRINGS(_conststr_, _str_) Tools::DebugStrings(_conststr_, _str_, __FUNCTION__, __FILE__, __LINE__);
	
	#define DEBUGSTRINGS2(_conststr1_, _str1_, _conststr2_, _str2_) Tools::DebugStrings(_conststr1_, _str1_, _conststr2_, _str2_, __FUNCTION__, __FILE__, __LINE__);

	#define DEBUGOBJECT(_object_) Tools::MSWIN::CreateDebugString("Debug Object:", Tools::DebugObject(Tools::MSWIN::reference_cast(_object_)).c_str(), NULL, __FUNCTION__, __FILE__, __LINE__);
	#define DEBUGOBJECTSTR(_object_, _str_) Tools::MSWIN::CreateDebugString("Debug Object<" _str_ ">:", Tools::DebugObject(Tools::MSWIN::reference_cast(_object_)).c_str(), NULL, __FUNCTION__, __FILE__, __LINE__);


	#define DEBUGTRACE(_body_, _statement_) Tools::DebugTrace(_body_, _statement_, #_body_, __FUNCTION__, __FILE__, __LINE__)
	#define DEBUGTRACEVOID(_body_, _statement_) { Tools::DebugTrace(_statement_, #_body_, __FUNCTION__, __FILE__, __LINE__); _body_; }


	#define TRACECOND(_cond_, _statement_) Tools::DebugCond(_cond_, _statement_, #_cond_, __FUNCTION__, __FILE__, __LINE__)
	#define TRACE(_body_, _statement_) Tools::DebugTrace(_body_, _statement_, #_body_, __FUNCTION__, __FILE__, __LINE__)
	#define TRACEVOID(_body_, _statement_) { Tools::DebugTrace(_statement_, #_body_, __FUNCTION__, __FILE__, __LINE__); _body_; }

	#define ASSERTCHECK(_body_) Tools::AssertCheck(_body_, #_body_, __FUNCTION__, __FILE__, __LINE__)
	#define ASSERT(_cond_) Tools::AssertCheck(_cond_, #_cond_, __FUNCTION__, __FILE__, __LINE__)

	#define NOTALLOWED(_statement_, _body_) Tools::NotAllowed(_statement_, #_body_, __FUNCTION__, __FILE__, __LINE__)

	#else
	//Non Debug Items

	#ifndef TYPE_DEBUG_AID
		#define TYPE_DEBUG_AID(_T_)
	#endif

	inline void SetThreadName(unsigned long, const char*)
	{}

	inline void SetThreadName(const char*)
	{}

	inline void DisplayDebugString(const char* szDebug)
	{}

	#define FILENAMELINENBR(_file_, _line_)
	#define COMPILER_ERROR(_ErrorReason_)
	#define COMPILER_ERROR_COND(_Cond_, _ErrorReason_)
	#define DEFINE_INTERFACE_CHECK(__ClassName__)
	#define COMPILER_ERROR_COND_INTERFACE(__BaseClassName__)

	#define TRACECOND(_cond_, _statement_) (_cond_)
	#define TRACE(_body_, _statement_) (_body_)
	#define TRACEVOID(_body_, _statement_)  _body_

	#define DEBUGCOND(_cond_, _statement_)
	#define DEBUGSTRING(_str_)
	#define DEBUGTRACE(_body_, _statement_)
	#define DEBUGTRACEVOID(_body_, _statement_)
	#define DEBUGOBJECT(_object_)
	#define DEBUGOBJECTSTR(_object_, _str_)

	#define DEBUGSTRINGS(_conststr_, _str_)
	#define DEBUGSTRINGS2(_conststr1_, _str1_, _conststr2_, _str2_)

	#define ASSERTCHECK(_body_) (_body_)
	#define ASSERT(_cond_)
	#define NOTALLOWED(_statement_, _code_) assert(false);

	#endif //end of _DEBUG

} //end of namespace Tools

#endif //end of __DebubbingAids_h_