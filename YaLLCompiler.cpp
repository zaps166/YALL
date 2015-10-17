#include "YaLLCompiler.hpp"

#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#include <unordered_map>
#include <iostream>
#include <vector>

#include <time.h>

#if __cplusplus > 201103L
	#define make_unique std::make_unique
#else
	#include <llvm/ADT/STLExtras.h>
	#define make_unique llvm::make_unique
#endif

using namespace llvm;
using namespace std;

Module *YaLLCompiler::compile( istream &input )
{
	using Tokens = SmallVector< StringRef, 0 >;

	/* Struktury */
	struct Operand
	{
		enum Type : uint8_t
		{
			Unknown,
			Number, //Double
			Variable,
			Array,
			ArrayFill,
			Call
		} type = Unknown;
		double number = 0.0;
		std::string label;
		std::unique_ptr< Operand > arrIdx;
		Tokens tokens; //For filling array or for function call
	};
	struct Blocks
	{
		inline Blocks( BasicBlock *nextBlock, BasicBlock *jumpBlock = nullptr ) :
			nextBlock( nextBlock ), jumpBlock( jumpBlock )
		{}

		BasicBlock *nextBlock = nullptr, *jumpBlock = nullptr;
	};

	/* Zmienne */
	Module *module = nullptr;

	vector< string > lines;

	LLVMContext &ctx = getGlobalContext();
	uint32_t lineCounter = 0;

	vector< Blocks > blockStack;
	unordered_map< string, AllocaInst * > variables;

	unique_ptr< IRBuilder<> > irBuilder, irBuilderAllocaBlock;
	bool lastIsRetOrBreakOrContinue = false;
	Function *currFn = nullptr;

	/* Funkcje pomocnicze */
	function< Value *( const Operand &operand ) > getValueFromOperand;

	auto throwBadUsageException = [ & ]( const string &tok )
	{
		 throw Exception( "Bad \"" + tok + "\" usage", lineCounter );
	};

	auto getTokens = []( const string &line )
	{
		Tokens tokens;
		if ( !line.empty() && line[ 0 ] != ';' )
		{
			size_t whitespaces;
			for ( whitespaces = 0 ; whitespaces < line.length() && ( line[ whitespaces ] == '\t' || line[ whitespaces ] == ' ' ) ; ++whitespaces );
			StringRef( line.data() + whitespaces, line.length() - whitespaces ).split( tokens, " ", -1, false );
		}
		return tokens;
	};

	auto createBlock = [ & ]( const string &name, Function *fn = nullptr )
	{
		return BasicBlock::Create( ctx, name, fn ? fn : currFn );
	};

	auto parseCond = [ & ]( const StringRef &condStr )
	{
		if ( condStr == "==" )
			return CmpInst::FCMP_UEQ;
		else if ( condStr == ">" )
			return CmpInst::FCMP_UGT;
		else if ( condStr == ">=" )
			return CmpInst::FCMP_UGE;
		else if ( condStr == "<" )
			return CmpInst::FCMP_ULT;
		else if ( condStr == "<=" )
			return CmpInst::FCMP_ULE;
		else if ( condStr == "!=" )
			return CmpInst::FCMP_UNE;
		throw Exception( "Bad condition", lineCounter );
	};

	auto castFPtoUI32 = [ & ]( Value *val )
	{
		return irBuilder->CreateCast( Instruction::FPToUI, val, Type::getInt32Ty( ctx ) );
	};

	function< Operand( const StringRef & ) > getOperand = [ & ]( const StringRef &tok )
	{
		Operand operand;
		if ( !tok.empty() )
		{
			auto fillTokens = [ & ]
			{
				tok.substr( 1, tok.size() - 2 ).split( operand.tokens, ",", -1, false );
			};

			if ( tok.size() == 3 && tok[ 0 ] == '\'' && tok[ tok.size() - 1 ] == '\'' ) //Znak (nie może być spacji)
			{
				operand.type = Operand::Number;
				operand.number = tok[ 1 ];
			}
			else if ( tok[ 0 ] == '-' || tok[ 0 ] == '+' || isdigit( tok[ 0 ] ) ) //Liczba
			{
				operand.type = Operand::Number;
				operand.number = stod( tok );
			}
			else if ( tok.startswith( "{" ) && tok.endswith( "}" ) ) //Wypełnianie tablicy
			{
				operand.type = Operand::ArrayFill;
				fillTokens();
			}
			else if ( tok.startswith( "(" ) && tok.endswith( ")" ) ) //Wywołanie funkcji
			{
				if ( tok.count( '(' ) > 1 && tok.count( ')' ) > 1 )
					throw Exception( "Nested function calls are not supported", lineCounter );
				operand.type = Operand::Call;
				fillTokens();
			}
			else //Zmienna
			{
				ssize_t bracket1Pos = tok.find( '[' ), bracket2Pos = tok.rfind( ']' );
				if ( bracket1Pos > 0 && bracket2Pos > bracket1Pos ) //Index tablicy
				{
					const auto newTok = tok.substr( bracket1Pos + 1, bracket2Pos - bracket1Pos - 1 );
					operand.arrIdx = make_unique< Operand >( getOperand( newTok ) );
					operand.type = Operand::Array;
					operand.label = tok.substr( 0, bracket1Pos );
				}
				else //Zwykła zmienna
				{
					operand.type = Operand::Variable;
					operand.label = tok;
				}
				if
				(
					operand.label.empty() ||
					*operand.label.begin() == '.' ||
					*operand.label.rbegin() == '.' ||
					isdigit( *operand.label.begin() ) ||
					( ssize_t )operand.label.find( ',' ) > -1 ||
					( ssize_t )operand.label.find( '-' ) > -1 ||
					( ssize_t )operand.label.find( '+' ) > -1 ||
					( ssize_t )operand.label.find( '*' ) > -1 ||
					( ssize_t )operand.label.find( '/' ) > -1
				) throw Exception( "Incorrect label name \"" + operand.label + "\"", lineCounter );
			}
		}
		return operand;
	};
	auto getOperands = [ & ]( Tokens::const_iterator begin, Tokens::const_iterator end )
	{
		vector< Operand > operands;
		for ( auto it = begin ; it != end ; ++it )
			operands.push_back( getOperand( *it ) );
		return operands;
	};

	auto getDoubleVal = [ & ]( double val = 0.0 )
	{
		return ConstantFP::get( ctx, APFloat( val ) );
	};

	auto getVariableFromOperand = [ & ]( const Operand &operand, bool initialize = false )->Value *
	{
		if ( operand.type != Operand::Variable && operand.type != Operand::Array )
			throw Exception( "Bad operand type for variable", lineCounter );
		auto &var = variables[ operand.label ];
		if ( !var )
		{
			if ( operand.type == Operand::Array )
				throw Exception( "Array must be created before", lineCounter );
			var = irBuilderAllocaBlock->CreateAlloca( Type::getDoubleTy( ctx ), nullptr, operand.label );
			if ( initialize )
				irBuilderAllocaBlock->CreateStore( getDoubleVal(), var );
		}
		if ( operand.type == Operand::Array )
			return irBuilder->CreateGEP( var, castFPtoUI32( getValueFromOperand( *operand.arrIdx ) ) );
		return var;
	};
	getValueFromOperand = [ & ]( const Operand &operand )->Value *
	{
		switch ( operand.type )
		{
			case Operand::Number:
				return getDoubleVal( operand.number );
			case Operand::Variable:
			case Operand::Array:
				return irBuilder->CreateLoad( getVariableFromOperand( operand, true ) );
			case Operand::ArrayFill:
				throw Exception( "Cannot fill array here", lineCounter );
			case Operand::Call:
			{
				const auto &fnName = operand.tokens[ 0 ];
				const auto fn = module->getFunction( fnName );
				if ( fn )
				{
					const auto numArgs = operand.tokens.size() - 1;
					if ( numArgs != fn->arg_size() )
						throw Exception( "Expected " + to_string( fn->arg_size() ) + ", got " + to_string( numArgs ) + " argument(s) for function \"" + fnName.str() + "\"", lineCounter );
					vector< Value * > args;
					args.reserve( numArgs );
					for ( const auto &o : getOperands( operand.tokens.begin() + 1, operand.tokens.end() ) )
						args.push_back( getValueFromOperand( o ) );
					return irBuilder->CreateCall( fn, args );
				}
				throw Exception( "Cannot call function \"" + fnName.str() + "\"", lineCounter );
			}
			default:
				throw Exception( "Unknown operand", lineCounter );
		}
	};
	auto getValueFromOperands = [ & ]( vector< Operand >::const_iterator begin, vector< Operand >::const_iterator end )
	{
		vector< Value * > vals;
		vals.reserve( end - begin );
		for ( auto it = begin ; it != end ; ++it )
			vals.push_back( getValueFromOperand( *it ) );
		return vals;
	};

	/* Wczytywanie funkcji i nazwy programu, tworzenie modułu LLVM */
	string line;
	while ( getline( input, line ) )
	{
		++lineCounter;

		const auto tokens = getTokens( line );
		if ( tokens.empty() )
		{
			lines.push_back( string() ); //Dodanie pustej linii dla zachowania numeracji
			continue;
		}

		if ( tokens[ 0 ] == "Program" )
		{
			if ( module )
				throw Exception( "Duplicate program name", lineCounter );
			else
			{
				module = new Module( tokens[ 1 ], ctx );
				lines.push_back( string() ); //Dodanie pustej linii dla zachowania numeracji
			}
			continue;
		}

		if ( module && ( tokens[ 0 ] == "Fn" || tokens[ 0 ] == "ExternFn" ) )
		{
			int numArgs;
			if ( tokens.size() == 3 && ( numArgs = atoi( tokens[ 2 ].data() ) ) >= 0 )
			{
				if ( module->getFunction( tokens[ 1 ] ) )
					throw Exception( "Duplicate function \"" + tokens[ 1 ].str() + "\"", lineCounter );
				else
					module->getOrInsertFunction( tokens[ 1 ], FunctionType::get( Type::getDoubleTy( ctx ), vector< Type * >( numArgs, Type::getDoubleTy( ctx ) ), false ) );
				if ( tokens[ 0 ] != "Fn" ) //ExternFn musi być ignorowane
					line.clear();
			} else throwBadUsageException( tokens[ 0 ] );
		}

		lines.push_back( line );
	}
	if ( !module )
		throw Exception( "Program must begins with its name", lineCounter );

	/* Kompilacja linia po linii */
	lineCounter = 0;
	for ( const auto &line : lines )
	{
		bool err = false;
		++lineCounter;

		const auto tokens = getTokens( line );
		if ( tokens.empty() )
			continue;
		if ( tokens[ 0 ] == "Fn" )
		{
			currFn = module->getFunction( tokens[ 1 ] );
			irBuilderAllocaBlock = make_unique< IRBuilder<> >( createBlock( "AllocaBlock" ) );
			if ( currFn->arg_size() > 0 ) //Wstawianie argumentów funkcji do tablicy
			{
				const auto argsArrName = "args";
				const auto argsArr = variables[ argsArrName ] = irBuilderAllocaBlock->CreateAlloca( Type::getDoubleTy( ctx ), irBuilderAllocaBlock->getInt32( currFn->arg_size() ), argsArrName );
				for ( auto it = currFn->arg_begin(), it_end = currFn->arg_end() ; it != it_end ; ++it )
					irBuilderAllocaBlock->CreateStore( it, irBuilderAllocaBlock->CreateGEP( argsArr, irBuilderAllocaBlock->getInt32( it->getArgNo() ) ) );
			}
			irBuilder = make_unique< IRBuilder<> >( createBlock( "StartBlock" ) );
		}
		else if ( !currFn )
			throw Exception( "Code must be inside function", lineCounter );
		else
		{
			if ( tokens.size() == 1 )
			{
				if ( tokens[ 0 ] == "Loop" ) //Nieskończona pętla
				{
					/* Stworzenie dwóch bloków: wnętrze pętli, blok za pętlą */
					auto loopBlock = createBlock( "LoopBlock" );
					auto nextBlock = createBlock( "LoopNextBlock" );

					/* Skok do bloku wnętrza pętli */
					irBuilder->CreateBr( loopBlock );

					/* Odłożenie na stos bloku następnego i początkowego */
					blockStack.push_back( Blocks( nextBlock, loopBlock ) );

					/* Przygotowanie do generowania kodu wnętrza pętli */
					irBuilder = make_unique< IRBuilder<> >( loopBlock );
				}
				else if ( tokens[ 0 ] == "End" ) //Koniec bloku/funkcji
				{
					if ( !blockStack.empty() ) //Wyjście z bloku
					{
						const auto &b = *blockStack.rbegin();
						if ( !lastIsRetOrBreakOrContinue ) //Jeżeli wcześniej nie było "Ret"/"Break"/"Continue"
						{
							if ( b.jumpBlock ) //Gdy pętla
								irBuilder->CreateBr( b.jumpBlock ); //Skocz na początek pętli (lub do jej bloku warunkowego)
							else
								irBuilder->CreateBr( b.nextBlock ); //Skocz do bloku za warunkiem
						}
						irBuilder = make_unique< IRBuilder<> >( b.nextBlock ); //Przygotowanie do generowania kodu poza blokiem
						blockStack.pop_back();
					}
					else //Koniec funkcji
					{
						if ( !lastIsRetOrBreakOrContinue ) //Jeżeli ostatnio nie było "Ret"/"Break"/"Continue"
							irBuilder->CreateRet( getDoubleVal() ); //Utwórz powrót z funkcji z wartością 0.0
						irBuilderAllocaBlock->CreateBr( ++currFn->getBasicBlockList().begin() ); //Skok z "AllocaBlock" do "StartBlock"
						variables.clear();
						currFn = nullptr;
					}
					lastIsRetOrBreakOrContinue = false;
				}
				else if ( tokens[ 0 ] == "Break" || tokens[ 0 ] == "Continue" )
				{
					auto it = blockStack.rbegin();
					for ( ; it != blockStack.rend() && !it->jumpBlock ; ++it );
					if ( it == blockStack.rend() )
						throw Exception( tokens[ 0 ].str() + " must be used inside Loop", lineCounter );
					else
					{
						if ( tokens[ 0 ] == "Break" )
							irBuilder->CreateBr( it->nextBlock );
						else if ( tokens[ 0 ] == "Continue" )
							irBuilder->CreateBr( it->jumpBlock );
						lastIsRetOrBreakOrContinue = true;
					}
				}
				continue;
			}
			if ( lastIsRetOrBreakOrContinue ) //Jeżeli ostatnio był "Ret"/"Break", to ignoruj resztę aż do zakończenia funkcji/bloku
				continue;
			if ( ( tokens.size() == 1 || tokens.size() == 2 ) && tokens[ 0 ] == "Ret" )
			{
				if ( tokens.size() == 2 )
					irBuilder->CreateRet( getValueFromOperand( getOperands( tokens.begin() + 1, tokens.end() )[ 0 ] ) );
				else
					irBuilder->CreateRet( getDoubleVal() );
				lastIsRetOrBreakOrContinue = true;
			}
			else if ( tokens.size() == 2 && tokens[ 0 ] == "Call" )
			{
				const auto call = getOperand( tokens[ 1 ] );
				if ( call.type == Operand::Call )
					getValueFromOperand( call );
				else
					throwBadUsageException( tokens[ 0 ] );
			}
			else if ( tokens.size() >= 2 && ( tokens[ 0 ] == "Putn" || tokens[ 0 ] == "Putc" ) )
			{
				const auto operands = getOperands( tokens.begin() + 1, tokens.end() );
				if ( operands.empty() )
					throwBadUsageException( tokens[ 0 ] );
				const char t = tokens[ 0 ][ 3 ];
				const auto func = module->getOrInsertFunction( "printf", FunctionType::get( Type::getInt32Ty( ctx ), { Type::getInt8PtrTy( ctx ) }, true ) );
				const auto printfFmt = cast< GlobalVariable >( module->getOrInsertGlobal( "printf_fmt_" + string( 1, t ) + "_" + to_string( operands.size() ), ArrayType::get( Type::getInt8Ty( ctx ), 3 ) ) );
				if ( !printfFmt->hasInitializer() )
				{
					string str;
					for ( size_t i = 0 ; ; ++i )
					{
						if ( i >= operands.size() )
							break;
						else if ( t == 'n' && i )
							str += ' ';
						str += ( t == 'n' ) ? "%g" : "%c";
					}
					printfFmt->setInitializer( ConstantDataArray::getString( ctx, str, true ) );
				}
				vector< Value * > values;
				values.reserve( operands.size() + 1 );
				values.push_back( printfFmt );
				for ( const auto &o : operands )
				{
					const auto val = getValueFromOperand( o );
					values.push_back( ( t == 'n' ) ? val : castFPtoUI32( val ) );
				}
				irBuilder->CreateCall( func, values );
			}
			else if ( tokens.size() == 4 && tokens[ 0 ] == "If" )
			{
				/* Parsowanie znaku warunku */
				const auto cond = parseCond( tokens[ 2 ] );

				/* Pobieranie wartości liczb do porównania */
				auto val1 = getValueFromOperand( getOperand( tokens[ 1 ] ) );
				auto val2 = getValueFromOperand( getOperand( tokens[ 3 ] ) );

				/* Stworzenie dwóch bloków: wyrażenie prawdziwe i fałszywe */
				auto condTrueBlock = createBlock( "CondTrueBlock" );
				auto condFalseBlock = createBlock( "CondFalseBlock" );

				/* Generowanie sprawdzenia warunku i skoku warunkowego */
				irBuilder->CreateCondBr( irBuilder->CreateFCmp( cond, val1, val2 ), condTrueBlock, condFalseBlock );

				/* Odłożenie na stos bloku następnego ("false") */
				blockStack.push_back( Blocks( condFalseBlock ) );

				/* Przygotowanie do generowania kodu bloku warunkowego dla "true" */
				irBuilder = make_unique< IRBuilder<> >( condTrueBlock );
			}
			else if ( tokens.size() >= 2 ) //Pierwszy operand to zmienna do ustawienia
			{
				const auto out = getOperand( tokens[ 1 ] );
				if ( out.type != Operand::Variable && out.type != Operand::Array )
					throw Exception( "Target must by a variable or an array", lineCounter );
				if ( out.label.empty() )
					throw Exception( "Target label can't be empty", lineCounter );

				/* Wyrażenie matematyczne infixowe */
				if ( tokens[ 0 ] == "Math" )
				{
					if ( tokens.size() != 3 )
						throwBadUsageException( tokens[ 0 ] );

					const auto &math = tokens[ 2 ];

					struct MathToken
					{
						MathToken( char op ) :
							op( op )
						{}
						MathToken( Value *val ) :
							val( val )
						{}

						char op = '\0';
						Value *val = nullptr;
					};
					vector< MathToken > postfix;

					/* Konwersja infix->postfix */
					vector< char > operators;
					for ( size_t i = 0, b = 0 ; ; ++i )
					{
						const char o1 = i < math.size() ? math[ i ] : '\0';
						if ( o1 == '\0' || o1 == '+' || o1 == '-' || o1 == '/' || o1 == '*' || o1 == '(' || o1 == ')' )
						{
							const auto val = math.substr( b, i - b );
							if ( !val.empty() ) //Jeżeli jest liczba (jak jest kilka operatorów po kolei, to jej nie ma)
								postfix.push_back( getValueFromOperand( getOperand( val ) ) );
							if ( o1 == '\0' ) //Jeżeli nie ma operatora, to koniec wyrażenia
								break;
							else
							{
								if ( ( o1 == '+' || o1 == '-' ) && ( !i || ( i > 0 && math[ i-1 ] == '(' ) ) ) //Operator przed liczbą
								{
									if ( i < ( math.size() - 1 ) && ( math[ i+1 ] == '+' || math[ i+1 ] == '-' ) ) //Wielokrotne operatory
										throw Exception( "Multiple math operators", lineCounter );
									continue;
								}
								if ( o1 == '(' )
									operators.push_back( o1 );
								else if ( o1 == ')' )
								{
									while ( !operators.empty() )
									{
										const char o2 = *operators.rbegin();
										operators.pop_back();
										if ( o2 == '(' )
											break;
										else
											postfix.push_back( o2 );
									}
								}
								else
								{
									while ( !operators.empty() )
									{
										auto getPriority = []( char op )
										{
											if ( op == '(' )
												return 0;
											if ( op == '+' || op == '-' || op == ')' )
												return 1;
											return 2; //if ( op == '*' || op == '/' )
										};
										const char o2 = *operators.rbegin();
										if ( getPriority( o1 ) > getPriority( o2 ) )
											break;
										else
										{
											postfix.push_back( o2 );
											operators.pop_back();
										}
									}
									operators.push_back( o1 );
								}
								b = i + 1;
							}
						}
					}
					while ( !operators.empty() )
					{
						postfix.push_back( *operators.rbegin() );
						operators.pop_back();
					}

					/* Obliczanie wyrażenia postfix */
					vector< Value * > values;
					for ( const auto &t : postfix )
					{
						if ( t.val ) //liczba
							values.push_back( t.val );
						else //operator
						{
							if ( values.size() < 2 )
								throw Exception( "Bad math expression", lineCounter );
							const auto val2 = *values.rbegin();
							values.pop_back();
							const auto val1 = *values.rbegin();
							values.pop_back();
							Value *result;
							switch ( t.op )
							{
								case '+':
									result = irBuilder->CreateFAdd( val1, val2 );
									break;
								case '-':
									result = irBuilder->CreateFSub( val1, val2 );
									break;
								case '*':
									result = irBuilder->CreateFMul( val1, val2 );
									break;
								case '/':
									result = irBuilder->CreateFDiv( val1, val2 );
									break;
//								default: //Nie powinno do tego dojść (wynika to z konwersji infix->postfix)
//									throw Exception( "Unknown operator", lineCounter );
							}
							values.push_back( result );
						}
					}

					/* Zapis wyniku */
					irBuilder->CreateStore( values[ 0 ], getVariableFromOperand( out ) );

					continue;
				}

				const auto operands = getOperands( tokens.begin() + 2, tokens.end() );
				if ( tokens[ 0 ] == "Set" )
				{
					if ( operands.size() == 1 )
						irBuilder->CreateStore( getValueFromOperand( operands[ 0 ] ), getVariableFromOperand( out ) );
					else
						throwBadUsageException( tokens[ 0 ] );
				}
				else if ( tokens[ 0 ] == "Array" )
				{
					if ( operands.size() == 1 && out.type == Operand::Variable )
					{
						Value *arrSize;
						vector< double > values;
						if ( operands[ 0 ].type == Operand::Number )
							arrSize = getValueFromOperand( operands[ 0 ] );
						else if ( operands[ 0 ].type == Operand::ArrayFill )
						{
							values.reserve( operands[ 0 ].tokens.size() );
							for ( const auto &v : operands[ 0 ].tokens )
								values.push_back( stod( v ) );
							if ( values.empty() )
								throw Exception( "Array size must be greater than 0", lineCounter );
							arrSize = getDoubleVal( values.size() );
						}
						else throw Exception( "Unsupported way of creating array", lineCounter );
						const auto arrSizeLabel = out.label + ".size";
						/* Tworzenie tablicy o zadanym rozmiarze */
						auto arr = variables[ out.label ] = irBuilderAllocaBlock->CreateAlloca( Type::getDoubleTy( ctx ), castFPtoUI32( arrSize ), out.label );
						/* Jeżeli tablica jest wypełniana - wypełnia ją */
						for ( size_t i = 0 ; i < values.size() ; ++i )
							irBuilderAllocaBlock->CreateStore( getDoubleVal( values[ i ] ), irBuilderAllocaBlock->CreateGEP( arr, irBuilderAllocaBlock->getInt32( i ) ) );
						/* Tworzenie zmiennej z rozmiarem tablicy */
						irBuilderAllocaBlock->CreateStore( arrSize, variables[ arrSizeLabel ] = irBuilderAllocaBlock->CreateAlloca( Type::getDoubleTy( ctx ), nullptr, arrSizeLabel ) );
					}
					else throwBadUsageException( tokens[ 0 ] );
				}
				else if ( tokens[ 0 ] == "Sqr" )
				{
					if ( operands.size() == 1 )
					{
						auto val = getValueFromOperand( operands[ 0 ] );
						irBuilder->CreateStore( irBuilder->CreateFMul( val, val ), getVariableFromOperand( out ) );
					}
					else throwBadUsageException( tokens[ 0 ] );
				}
				else if ( tokens[ 0 ] == "Mul" || tokens[ 0 ] == "Add" || tokens[ 0 ] == "Div" || tokens[ 0 ] == "Sub" )
				{
					if ( operands.size() >= 2 )
					{
						const auto vals = getValueFromOperands( operands.begin(), operands.end() );
						auto val = vals[ 0 ];
						for ( size_t i = 1 ; i < vals.size() ; ++i )
						{
							if ( tokens[ 0 ] == "Mul" )
								val = irBuilder->CreateFMul( val, vals[ i ] );
							else if ( tokens[ 0 ] == "Add" )
								val = irBuilder->CreateFAdd( val, vals[ i ] );
							else if ( tokens[ 0 ] == "Div" )
								val = irBuilder->CreateFDiv( val, vals[ i ] );
							else if ( tokens[ 0 ] == "Sub" )
								val = irBuilder->CreateFSub( val, vals[ i ] );
						}
						irBuilder->CreateStore( val, getVariableFromOperand( out ) );
					}
					else throwBadUsageException( tokens[ 0 ] );
				}
				else if ( tokens[ 0 ] == "Sin" || tokens[ 0 ] == "Cos" || tokens[ 0 ] == "Tan" || tokens[ 0 ] == "Sqrt" || tokens[ 0 ] == "Exp" || tokens[ 0 ] == "Ln" || tokens[ 0 ] == "Pow" )
				{
					const bool twoArgs = tokens[ 0 ] == "Pow";
					if ( ( operands.size() == 2 && twoArgs ) || ( operands.size() == 1 && !twoArgs ) )
					{
						vector< Type * > args;
						vector< Value * > values;
						string cFunc = tokens[ 0 ].lower();
						if ( cFunc == "ln" )
							cFunc = "log";
						args.reserve( twoArgs + 1 );
						values.reserve( args.capacity() );
						for ( size_t i = 0 ; i < args.capacity() ; ++i )
						{
							args.push_back( Type::getDoubleTy( ctx ) );
							values.push_back( getValueFromOperand( operands[ i ] ) );
						}
						const auto func = module->getOrInsertFunction( cFunc, FunctionType::get( Type::getDoubleTy( ctx ), args, false ) );
						irBuilder->CreateStore( irBuilder->CreateCall( func, values ), getVariableFromOperand( out ) );
					} else throwBadUsageException( tokens[ 0 ] );
				}
				else if ( tokens[ 0 ] == "Loop" )
				{
					/* Loop zmienna_iteracyjna początek_pętli koniec_pętli wartość_do_dodania_do_koniec_pętli krok_pętli */
					if ( operands.size() != 4 )
						throw Exception( "Incorrect argument count for \"Loop\"", lineCounter );
					else
					{
						/* Stworzenie bloku kroku pętli */
						auto stepBlock = createBlock( "LoopStepBlock" );
						/* Stworzenie bloku warunkowego pętli */
						auto condBlock = createBlock( "LoopCondBlock" );
						/* Stworzenie dwóch bloków: wnętrze pętli, blok za pętlą */
						auto loopBlock = createBlock( "LoopBlock" );
						auto nextBlock = createBlock( "LoopNextBlock" );

						/* Pobranie/tworzenie zmiennej iteratora */
						auto itVar = getVariableFromOperand( out );
						/* Zapis wartości początkowej w iteratorze */
						irBuilder->CreateStore( getValueFromOperand( operands[ 0 ] ), itVar );

						/* Określenie wartości końca pętli (dodanie do wartości końca pętli, wartości do dodania (zwykle ujemna)) */
						auto loopEnd = irBuilder->CreateFAdd( getValueFromOperand( operands[ 1 ] ), getValueFromOperand( operands[ 2 ] ) );
						/* Pobranie wartości kroku pętli */
						auto loopStep = getValueFromOperand( operands[ 3 ] );

						/* Skok do bloku warunkowego pętli */
						irBuilder->CreateBr( condBlock );

						/* Przygotowanie do generowanie kodu bloku kroku pętli */
						irBuilder = make_unique< IRBuilder<> >( stepBlock );
						/* Ładowanie wartości iteratora */
						auto it = irBuilder->CreateLoad( itVar );
						/* Dodawanie do iteratora wartości kroku pętli */
						irBuilder->CreateStore( irBuilder->CreateFAdd( it, loopStep ), itVar );
						/* Skok do bloku warunkowego */
						irBuilder->CreateBr( condBlock );

						/* Przygotowanie do generowanie kodu bloku warunkowego */
						irBuilder = make_unique< IRBuilder<> >( condBlock );
						/* Ładowanie wartości iteratora */
						it = irBuilder->CreateLoad( itVar );
						/* Warunek if (it <= loopEnd) */
						auto cond = irBuilder->CreateFCmpULE( it, loopEnd );
						/* Skok warunkowy albo do wnętrza pętli, albo do bloku poza pętlą */
						irBuilder->CreateCondBr( cond, loopBlock, nextBlock );

						/* Odłożenie na stos bloku następnego i początkowego */
						blockStack.push_back( Blocks( nextBlock, stepBlock ) );

						/* Przygotowanie do generowania kodu wnętrza pętli */
						irBuilder = make_unique< IRBuilder<> >( loopBlock );
					}
				}
				else err = true;
			}
			else err = true;
			if ( err )
				throw Exception( "Unknown instruction \"" + tokens[ 0 ].str() + "\"", lineCounter );
		}
	}
	lines.clear();
	lines.shrink_to_fit();
	if ( currFn )
		throw Exception( "Function must ends with \"End\"" );
	return module;
}

string YaLLCompiler::getProgramName( Module *module )
{
	return module ? module->getModuleIdentifier() : string();
}
string YaLLCompiler::getIR( Module *module )
{
	string ir;
	if ( module )
	{
		raw_string_ostream s( ir );
		s << *module;
	}
	return ir;
}
