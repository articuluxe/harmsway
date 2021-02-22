#include <Windows.h>

#include <stdexcpt.h>
#include <excpt.h>
#include <stdlib.h>
#include <sstream>
#include <errno.h>

#include "DebuggingAids.h"

namespace Tools {

	namespace MSWIN {

		void setthreadname(unsigned long dwThreadID, const char* szThreadName)
		{
			typedef struct tagTHREADNAME_INFO
			{
				unsigned long	dwType;			// must be 0x1000
				const char*		szName;			// pointer to name (in same addr space)
				unsigned long	dwThreadID;		// thread ID (-1 caller thread)
				unsigned long	dwFlags;		// reserved for future use, most be zero
			} THREADNAME_INFO;


			THREADNAME_INFO info;
			info.dwType = 0x1000;
			info.szName = szThreadName;
			info.dwThreadID = dwThreadID;
			info.dwFlags = 0;

			__try
			{
				RaiseException(
					0x406d1388,
					0,
					sizeof(info) / sizeof(unsigned long),
					(unsigned long *)&info
					) ;
			}
			__except (EXCEPTION_CONTINUE_EXECUTION)
			{
			}
		}

		void displaydebugstring(const char* szDebug)
		{
			OutputDebugString(szDebug);
		}

		std::string GetFileName(const char* path)
		{
			char drive[_MAX_DRIVE];
			char dir[_MAX_DIR];
			char fname[_MAX_FNAME];
			char ext[_MAX_EXT];
			errno_t err;

			err = _splitpath_s( path, drive, _MAX_DRIVE, dir, _MAX_DIR, fname, _MAX_FNAME, ext, _MAX_EXT );
			if (err != 0)
			{
				return path;
			}

			std::string filename = fname;
			filename += ext;

			return filename;
		}

		std::string CreateDebugString(const char* title,
										const char* statement,
										const char* codeStatement,
										const char* functionName,
										const char* fileName,
										int lineNbr,
										const char* valueStr,
										bool getErrorNo)
		{
			std::string debugString = title == NULL ? "" : title;
			 debugString += " ";
			if(fileName != NULL)
			{
				debugString += MSWIN::GetFileName(fileName);
				debugString += "@";
				std::stringstream x;
				x << lineNbr;
				debugString += x.str();
			}
			if(functionName != NULL)
			{
				debugString += "(";
				debugString += functionName;
				debugString += ")";
			}
			if(statement != NULL)
			{
				debugString += " ";
				debugString += statement;
			}
			if(codeStatement != NULL)
			{
				debugString += " <";
				if(valueStr != NULL)
				{
					debugString += valueStr;
					debugString += " = ";
				}
				debugString += codeStatement;
				debugString += ">";
			}
			if(getErrorNo)
			{
				int errorNum = 0;
				if(_get_errno(&errorNum) == 0 && errorNum != 0)
				{
					debugString += " ErrorNo Info: ";
					std::stringstream x;
					x << errorNum;
					debugString += x.str();
				}
			}

			debugString += "\n";
			DisplayDebugString(debugString.c_str());
			return debugString;
		}

	} //end of namespace MSWIN
} //end of namespace Tools
