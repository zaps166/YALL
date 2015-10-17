#pragma once

#include <exception>
#include <istream>
#include <string>

namespace llvm { class Module; }

namespace YaLLCompiler
{
	class Exception : public std::exception
	{
		std::string msg;
	public:
		inline Exception( const std::string &msg, uint32_t line = 0 ) :
			msg( msg + ( line > 0 ? ( " (line " + std::to_string( line ) + ")" ) : std::string() ) )
		{}

		virtual const char *what() const noexcept
		{
			return msg.c_str();
		}
	};

	llvm::Module *compile( std::istream &input );

	std::string getProgramName( llvm::Module *module );
	std::string getIR( llvm::Module *module );
}
