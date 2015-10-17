#include "YaLLCompiler.hpp"

#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/IR/Module.h>

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <string>
#include <vector>

using namespace llvm;
using namespace std;

int main( int argc, char *argv[] )
{
	if ( argc != 2 )
	{
		cerr << argv[ 0 ] << " must have only one orgument!" << endl;
		return -2;
	}

	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();

	atexit( []
	{
		llvm_shutdown();
	} );

	try
	{
		ifstream input( argv[ 1 ] );
		if ( !input.is_open() )
		{
			cerr << "Cannot open \"" << argv[ 1 ] << "\"" << endl;
			return -1;
		}
		auto module = YaLLCompiler::compile( input );
		input.close();

		cout << YaLLCompiler::getIR( module ) << endl;

		auto fn = module->getFunction( "main" );
		if ( !fn )
		{
			cerr << "The \"main\" function doesn't exists!" << endl;
			return -1;
		}

		string e;
		unique_ptr< ExecutionEngine > EE( EngineBuilder(
			#if LLVM_VERSION_MAJOR <= 3 && LLVM_VERSION_MINOR <= 5
				module
			#else
				unique_ptr< Module >( module )
			#endif
			)
			.setErrorStr( &e )
			.setOptLevel( CodeGenOpt::Aggressive )
			.setEngineKind( EngineKind::JIT )
			.create()
		);
		if ( !e.empty() )
		{
			cerr << e << '\n';
			return -1;
		}
		else if ( EE )
		{
//			EE->DisableLazyCompilation( false );
			EE->finalizeObject();

			GenericValue gv;
			vector< GenericValue > args;

			gv = EE->runFunction( fn, args );
			cerr.precision( 10 );
			cerr << "Result: " << gv.DoubleVal << '\n';
		}
	}
	catch ( exception &ex )
	{
		cerr << ex.what() << '\n';
		return -1;
	}

	return 0;
}
