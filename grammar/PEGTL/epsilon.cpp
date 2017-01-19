#include "pegtl.hh"
#include <iostream>
#include <fstream>
#include <string>
#include <chrono>

using namespace std;

struct pFile;
struct p_;
struct pS;
struct pBLOCKCOMMENT;
struct pLINECOMMENT;
struct pEOT;
struct pIdentifier;
struct pNAME;
struct pDIGIT;
struct pW;
struct pTopLevel;
struct pKEYWORD;
struct pDeclaration;
struct pFunctionDeclaration;
struct pFunctionParamList;
struct pFunctionParam;
struct pVariableDeclaration;
struct pVariableList;
struct pInitDecl;
struct pInitializer;
struct pBlock;
struct pStatement;
struct pExpression;
struct pAssignmentExpression;
struct p_AssignmentOperator;
struct pConstantExpression;
struct pConditionalExpression;
struct pLogicalOrExpression;
struct pLogicalAndExpression;
struct pEqualityExpression;
struct pRelationalExpression;
struct pUnaryExpression;
struct pPostfixExpression;
struct pFunctionCall;
struct p_FunctionCall;
struct p_ArgumentExpressionList;
struct pPrimaryExpression;
struct pFunctionExpression;
struct pConstant;
struct pIntegerLiteral;
struct pDECIMAL;
struct pBooleanLiteral;
struct pStringLiteral;
struct pSTRING_CONTENT;
struct pNullLiteral;

struct pFile : pegtl::seq<pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::star<pegtl::seq<pTopLevel ,p_ >>,pegtl::success>,pEOT >>, pegtl::success> {};
struct p_ : pegtl::seq<pegtl::star<pegtl::sor<pS ,pBLOCKCOMMENT ,pLINECOMMENT >>, pegtl::success> {};
struct pS : pegtl::seq<pegtl::one<'\t','\n','\r',' '>, pegtl::success> {};
struct pBLOCKCOMMENT : pegtl::seq<pegtl::seq<pegtl::one<'/'>,pegtl::seq<pegtl::one<'*'>,pegtl::seq<pegtl::star<pegtl::seq<pegtl::not_at<pegtl::seq<pegtl::one<'*'>,pegtl::one<'/'>>> ,pegtl::any>>,pegtl::seq<pegtl::one<'*'>,pegtl::one<'/'>>>>>, pegtl::success> {};
struct pLINECOMMENT : pegtl::seq<pegtl::seq<pegtl::one<'/'>,pegtl::seq<pegtl::one<'/'>,pegtl::star<pegtl::seq<pegtl::not_at<pegtl::one<'\n'>> ,pegtl::any>>>>, pegtl::success> {};
struct pEOT : pegtl::seq<pegtl::not_at<pegtl::any> , pegtl::success> {};
struct pIdentifier : pegtl::seq<pegtl::seq<pegtl::seq<pNAME ,pegtl::success>,p_ >, pegtl::success> {};
struct pNAME : pegtl::seq<pegtl::seq<pegtl::not_at<pDIGIT > ,pegtl::seq<pegtl::not_at<pKEYWORD > ,pegtl::plus<pW >>>, pegtl::success> {};
struct pDIGIT : pegtl::seq<pegtl::one<'0','1','2','3','4','5','6','7','8','9'>, pegtl::success> {};
struct pW : pegtl::seq<pegtl::one<'$','0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','_','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'>, pegtl::success> {};
struct pTopLevel : pegtl::seq<pegtl::sor<pDeclaration ,pegtl::seq<pegtl::success,pegtl::seq<pegtl::one<';'>,p_ >>>, pegtl::success> {};
struct pKEYWORD : pegtl::seq<pegtl::seq<pegtl::sor<pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::one<'e'>>>>,pegtl::seq<pegtl::one<'i'>,pegtl::one<'f'>>,pegtl::seq<pegtl::one<'r'>,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'t'>,pegtl::seq<pegtl::one<'u'>,pegtl::seq<pegtl::one<'r'>,pegtl::one<'n'>>>>>>,pegtl::seq<pegtl::one<'f'>,pegtl::seq<pegtl::one<'u'>,pegtl::seq<pegtl::one<'n'>,pegtl::seq<pegtl::one<'c'>,pegtl::seq<pegtl::one<'t'>,pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'o'>,pegtl::one<'n'>>>>>>>>,pegtl::seq<pegtl::one<'v'>,pegtl::seq<pegtl::one<'a'>,pegtl::one<'r'>>>>,pegtl::not_at<pW > >, pegtl::success> {};
struct pDeclaration : pegtl::seq<pegtl::sor<pFunctionDeclaration ,pVariableDeclaration >, pegtl::success> {};
struct pFunctionDeclaration : pegtl::seq<pegtl::seq<pegtl::seq<pegtl::one<'f'>,pegtl::seq<pegtl::one<'u'>,pegtl::seq<pegtl::one<'n'>,pegtl::seq<pegtl::one<'c'>,pegtl::seq<pegtl::one<'t'>,pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'o'>,pegtl::seq<pegtl::one<'n'>,pegtl::not_at<pW > >>>>>>>>,pegtl::seq<p_ ,pegtl::seq<pIdentifier ,pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pFunctionParamList ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::success>>>>>>>>, pegtl::success> {};
struct pFunctionParamList : pegtl::seq<pegtl::seq<pegtl::opt<pegtl::seq<pFunctionParam ,pegtl::star<pegtl::seq<pegtl::seq<pegtl::one<','>,p_ >,pFunctionParam >>>> ,pegtl::success>, pegtl::success> {};
struct pFunctionParam : pegtl::seq<pegtl::seq<pegtl::seq<pIdentifier ,pegtl::success>,p_ >, pegtl::success> {};
struct pVariableDeclaration : pegtl::seq<pegtl::seq<pegtl::seq<pegtl::one<'v'>,pegtl::seq<pegtl::one<'a'>,pegtl::seq<pegtl::one<'r'>,pegtl::not_at<pW > >>>,pegtl::seq<p_ ,pegtl::seq<pVariableList ,pegtl::seq<pegtl::seq<pegtl::one<';'>,p_ >,pegtl::success>>>>, pegtl::success> {};
struct pVariableList : pegtl::seq<pegtl::seq<pInitDecl ,pegtl::seq<pegtl::star<pegtl::seq<pegtl::seq<pegtl::one<','>,p_ >,pInitDecl >>,pegtl::success>>, pegtl::success> {};
struct pInitDecl : pegtl::seq<pegtl::seq<pegtl::seq<pIdentifier ,pegtl::seq<pegtl::opt<pegtl::seq<pegtl::seq<pegtl::one<'='>,pegtl::seq<pegtl::not_at<pegtl::one<'='>> ,p_ >>,pInitializer >> ,pegtl::success>>,p_ >, pegtl::success> {};
struct pInitializer : pegtl::seq<pAssignmentExpression , pegtl::success> {};
struct pBlock : pegtl::seq<pegtl::seq<pegtl::seq<pegtl::one<'{'>,p_ >,pegtl::seq<pegtl::star<pegtl::sor<pegtl::seq<pStatement ,p_ >,pegtl::seq<pDeclaration ,p_ >>>,pegtl::seq<pegtl::seq<pegtl::one<'}'>,p_ >,pegtl::success>>>, pegtl::success> {};
struct pStatement : pegtl::seq<pegtl::sor<pBlock ,pegtl::seq<pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'f'>,pegtl::not_at<pW > >>,pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'1'>,pegtl::seq<p_ ,pegtl::seq<pBlock ,pegtl::success>>>>>>>>>>>>>,pegtl::seq<pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'f'>,pegtl::not_at<pW > >>,pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'2'>,pegtl::seq<p_ ,pegtl::seq<pBlock ,pegtl::success>>>>>>>>>>>>>,pegtl::seq<pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'f'>,pegtl::not_at<pW > >>,pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'3'>,pegtl::seq<p_ ,pegtl::seq<pBlock ,pegtl::success>>>>>>>>>>>>>,pegtl::seq<pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'f'>,pegtl::not_at<pW > >>,pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'4'>,pegtl::seq<p_ ,pegtl::seq<pBlock ,pegtl::success>>>>>>>>>>>>>,pegtl::seq<pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'f'>,pegtl::not_at<pW > >>,pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'5'>,pegtl::seq<p_ ,pegtl::seq<pBlock ,pegtl::success>>>>>>>>>>>>>,pegtl::seq<pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'f'>,pegtl::not_at<pW > >>,pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'6'>,pegtl::seq<p_ ,pegtl::seq<pBlock ,pegtl::success>>>>>>>>>>>>>,pegtl::seq<pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'f'>,pegtl::not_at<pW > >>,pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'7'>,pegtl::seq<p_ ,pegtl::seq<pBlock ,pegtl::success>>>>>>>>>>>>>,pegtl::seq<pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'f'>,pegtl::not_at<pW > >>,pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'8'>,pegtl::seq<p_ ,pegtl::seq<pBlock ,pegtl::success>>>>>>>>>>>>>,pegtl::seq<pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'f'>,pegtl::not_at<pW > >>,pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'9'>,pegtl::seq<p_ ,pegtl::seq<pBlock ,pegtl::success>>>>>>>>>>>>>,pegtl::seq<pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'f'>,pegtl::not_at<pW > >>,pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'A'>,pegtl::seq<p_ ,pegtl::seq<pBlock ,pegtl::success>>>>>>>>>>>>>,pegtl::seq<pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'f'>,pegtl::not_at<pW > >>,pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'B'>,pegtl::seq<p_ ,pegtl::seq<pBlock ,pegtl::success>>>>>>>>>>>>>,pegtl::seq<pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'f'>,pegtl::not_at<pW > >>,pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'C'>,pegtl::seq<p_ ,pegtl::seq<pBlock ,pegtl::success>>>>>>>>>>>>>,pegtl::seq<pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'f'>,pegtl::not_at<pW > >>,pegtl::seq<p_ ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::seq<pegtl::opt<pegtl::seq<pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::seq<pegtl::one<'e'>,pegtl::not_at<pW > >>>>,pegtl::seq<p_ ,pBlock >>> ,pegtl::success>>>>>>>,pegtl::seq<pegtl::seq<pegtl::one<'r'>,pegtl::seq<pegtl::one<'e'>,pegtl::seq<pegtl::one<'t'>,pegtl::seq<pegtl::one<'u'>,pegtl::seq<pegtl::one<'r'>,pegtl::seq<pegtl::one<'n'>,pegtl::not_at<pW > >>>>>>,pegtl::seq<p_ ,pegtl::seq<pegtl::opt<pExpression > ,pegtl::seq<pegtl::seq<pegtl::one<';'>,p_ >,pegtl::success>>>>,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<';'>,p_ >,pegtl::success>>>, pegtl::success> {};
struct pExpression : pegtl::seq<pegtl::seq<pAssignmentExpression ,pegtl::star<pegtl::seq<pegtl::seq<pegtl::one<','>,p_ >,pegtl::seq<pAssignmentExpression ,pegtl::success>>>>, pegtl::success> {};
struct pAssignmentExpression : pegtl::seq<pegtl::sor<pegtl::seq<pUnaryExpression ,pegtl::seq<p_AssignmentOperator ,pAssignmentExpression >>,pConditionalExpression >, pegtl::success> {};
struct p_AssignmentOperator : pegtl::seq<pegtl::sor<pegtl::seq<pegtl::seq<pegtl::one<'='>,pegtl::seq<pegtl::not_at<pegtl::one<'='>> ,p_ >>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'*'>,pegtl::seq<pegtl::one<'='>,p_ >>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'/'>,pegtl::seq<pegtl::one<'='>,p_ >>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'%'>,pegtl::seq<pegtl::one<'='>,p_ >>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'+'>,pegtl::seq<pegtl::one<'='>,p_ >>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'-'>,pegtl::seq<pegtl::one<'='>,p_ >>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'<'>,pegtl::seq<pegtl::one<'<'>,pegtl::seq<pegtl::one<'='>,p_ >>>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'>'>,pegtl::seq<pegtl::one<'>'>,pegtl::seq<pegtl::one<'='>,p_ >>>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'>'>,pegtl::seq<pegtl::one<'>'>,pegtl::seq<pegtl::one<'>'>,pegtl::seq<pegtl::one<'='>,p_ >>>>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'&'>,pegtl::seq<pegtl::one<'='>,p_ >>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'^'>,pegtl::seq<pegtl::one<'='>,p_ >>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'|'>,pegtl::seq<pegtl::one<'='>,p_ >>,pegtl::success>>, pegtl::success> {};
struct pConstantExpression : pegtl::seq<pConditionalExpression , pegtl::success> {};
struct pConditionalExpression : pegtl::seq<pegtl::seq<pLogicalOrExpression ,pegtl::star<pegtl::seq<pegtl::seq<pegtl::one<'?'>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::seq<pegtl::one<':'>,pegtl::seq<pegtl::not_at<pegtl::one<'>'>> ,p_ >>,pegtl::seq<pLogicalOrExpression ,pegtl::success>>>>>>, pegtl::success> {};
struct pLogicalOrExpression : pegtl::seq<pegtl::seq<pLogicalAndExpression ,pegtl::star<pegtl::seq<pegtl::seq<pegtl::one<'|'>,pegtl::seq<pegtl::one<'|'>,p_ >>,pegtl::seq<pLogicalAndExpression ,pegtl::success>>>>, pegtl::success> {};
struct pLogicalAndExpression : pegtl::seq<pegtl::seq<pEqualityExpression ,pegtl::star<pegtl::seq<pegtl::seq<pegtl::one<'&'>,pegtl::seq<pegtl::one<'&'>,p_ >>,pegtl::seq<pEqualityExpression ,pegtl::success>>>>, pegtl::success> {};
struct pEqualityExpression : pegtl::seq<pegtl::seq<pRelationalExpression ,pegtl::star<pegtl::seq<pegtl::sor<pegtl::seq<pegtl::seq<pegtl::one<'='>,pegtl::seq<pegtl::one<'='>,p_ >>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'!'>,pegtl::seq<pegtl::one<'='>,p_ >>,pegtl::success>>,pRelationalExpression >>>, pegtl::success> {};
struct pRelationalExpression : pegtl::seq<pegtl::seq<pUnaryExpression ,pegtl::star<pegtl::seq<pegtl::sor<pegtl::seq<pegtl::seq<pegtl::one<'<'>,pegtl::seq<pegtl::not_at<pegtl::one<'='>> ,p_ >>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'<'>,pegtl::seq<pegtl::one<'='>,pegtl::seq<pegtl::not_at<pegtl::one<'='>> ,p_ >>>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'>'>,pegtl::seq<pegtl::not_at<pegtl::one<'='>> ,p_ >>,pegtl::success>,pegtl::seq<pegtl::seq<pegtl::one<'>'>,pegtl::seq<pegtl::one<'='>,pegtl::seq<pegtl::not_at<pegtl::one<'='>> ,p_ >>>,pegtl::success>>,pUnaryExpression >>>, pegtl::success> {};
struct pUnaryExpression : pegtl::seq<pegtl::sor<pPostfixExpression ,pegtl::seq<pegtl::seq<pegtl::one<'!'>,pegtl::seq<pegtl::not_at<pegtl::one<'='>> ,p_ >>,pegtl::seq<pUnaryExpression ,pegtl::success>>>, pegtl::success> {};
struct pPostfixExpression : pegtl::seq<pegtl::sor<pFunctionCall ,pPrimaryExpression >, pegtl::success> {};
struct pFunctionCall : pegtl::seq<pegtl::seq<pPrimaryExpression ,pegtl::plus<p_FunctionCall >>, pegtl::success> {};
struct p_FunctionCall : pegtl::seq<pegtl::seq<pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pegtl::opt<p_ArgumentExpressionList > ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::success>>>,pegtl::success>, pegtl::success> {};
struct p_ArgumentExpressionList : pegtl::seq<pegtl::seq<pAssignmentExpression ,pegtl::star<pegtl::seq<pegtl::seq<pegtl::one<','>,p_ >,pAssignmentExpression >>>, pegtl::success> {};
struct pPrimaryExpression : pegtl::seq<pegtl::sor<pConstant ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pExpression ,pegtl::seq<pegtl::one<')'>,p_ >>>,pFunctionExpression ,pIdentifier >, pegtl::success> {};
struct pFunctionExpression : pegtl::seq<pegtl::seq<pegtl::seq<pegtl::one<'f'>,pegtl::seq<pegtl::one<'u'>,pegtl::seq<pegtl::one<'n'>,pegtl::seq<pegtl::one<'c'>,pegtl::seq<pegtl::one<'t'>,pegtl::seq<pegtl::one<'i'>,pegtl::seq<pegtl::one<'o'>,pegtl::seq<pegtl::one<'n'>,pegtl::not_at<pW > >>>>>>>>,pegtl::seq<p_ ,pegtl::seq<pegtl::opt<pegtl::seq<pIdentifier ,p_ >> ,pegtl::seq<pegtl::seq<pegtl::one<'('>,p_ >,pegtl::seq<pFunctionParamList ,pegtl::seq<pegtl::seq<pegtl::one<')'>,p_ >,pegtl::seq<pBlock ,pegtl::seq<pegtl::seq<pegtl::one<':'>,pegtl::seq<pegtl::one<':'>,p_ >>,pegtl::success>>>>>>>>, pegtl::success> {};
struct pConstant : pegtl::seq<pegtl::sor<pIntegerLiteral ,pBooleanLiteral ,pStringLiteral ,pNullLiteral >, pegtl::success> {};
struct pIntegerLiteral : pegtl::seq<pegtl::seq<pegtl::seq<pDECIMAL ,pegtl::success>,p_ >, pegtl::success> {};
struct pDECIMAL : pegtl::seq<pegtl::sor<pegtl::seq<pegtl::one<'1','2','3','4','5','6','7','8','9'>,pegtl::star<pDIGIT >>,pegtl::one<'0'>>, pegtl::success> {};
struct pBooleanLiteral : pegtl::seq<pegtl::sor<pegtl::seq<pegtl::seq<pegtl::one<'t'>,pegtl::seq<pegtl::one<'r'>,pegtl::seq<pegtl::one<'u'>,pegtl::seq<pegtl::one<'e'>,pegtl::not_at<pW > >>>>,pegtl::seq<pegtl::success,p_ >>,pegtl::seq<pegtl::seq<pegtl::one<'f'>,pegtl::seq<pegtl::one<'a'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'s'>,pegtl::seq<pegtl::one<'e'>,pegtl::not_at<pW > >>>>>,pegtl::seq<pegtl::success,p_ >>>, pegtl::success> {};
struct pStringLiteral : pegtl::seq<pegtl::seq<pegtl::one<'"'>,pegtl::seq<pegtl::seq<pegtl::star<pSTRING_CONTENT >,pegtl::success>,pegtl::seq<pegtl::one<'"'>,p_ >>>, pegtl::success> {};
struct pSTRING_CONTENT : pegtl::seq<pegtl::seq<pegtl::not_at<pegtl::one<'\n','\"','\\'>> ,pegtl::any>, pegtl::success> {};
struct pNullLiteral : pegtl::seq<pegtl::seq<pegtl::seq<pegtl::one<'n'>,pegtl::seq<pegtl::one<'u'>,pegtl::seq<pegtl::one<'l'>,pegtl::seq<pegtl::one<'l'>,pegtl::not_at<pW > >>>>,pegtl::seq<pegtl::success,p_ >>, pegtl::success> {};

template<typename T>
struct action : pegtl::nothing<T> {};
template<>
struct action<pFile> {
   static void apply(const pegtl::input& in){}
};

int main(int argc, char const *argv[]) {
   for (int i = 1; i < argc; i++) {
      ifstream ifs(argv[i]);
      if (ifs.fail()){
         cerr << "File Not Found: " << argv[i] << endl;
         return -1;
      }
      string input((istreambuf_iterator<char>(ifs)), istreambuf_iterator<char>());
      auto start = chrono::high_resolution_clock::now();
      bool result = pegtl::parse<pFile,action>(input,"");
      auto end = chrono::high_resolution_clock::now();
      if (result) {
         cout << argv[i] << " " << std::chrono::duration_cast<chrono::milliseconds>(end-start).count() << " [ms]" << endl;
      } else {
         cout << argv[i] << " syntax error" << endl;
      }
   }
}