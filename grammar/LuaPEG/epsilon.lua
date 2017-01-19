local lpeg = require "lpeg"
local File = lpeg.V"File"
local _ = lpeg.V"_"
local S = lpeg.V"S"
local BLOCKCOMMENT = lpeg.V"BLOCKCOMMENT"
local LINECOMMENT = lpeg.V"LINECOMMENT"
local EOT = lpeg.V"EOT"
local Identifier = lpeg.V"Identifier"
local NAME = lpeg.V"NAME"
local DIGIT = lpeg.V"DIGIT"
local W = lpeg.V"W"
local TopLevel = lpeg.V"TopLevel"
local KEYWORD = lpeg.V"KEYWORD"
local Declaration = lpeg.V"Declaration"
local FunctionDeclaration = lpeg.V"FunctionDeclaration"
local FunctionParamList = lpeg.V"FunctionParamList"
local FunctionParam = lpeg.V"FunctionParam"
local VariableDeclaration = lpeg.V"VariableDeclaration"
local VariableList = lpeg.V"VariableList"
local InitDecl = lpeg.V"InitDecl"
local Initializer = lpeg.V"Initializer"
local Block = lpeg.V"Block"
local Statement = lpeg.V"Statement"
local Expression = lpeg.V"Expression"
local AssignmentExpression = lpeg.V"AssignmentExpression"
local _AssignmentOperator = lpeg.V"_AssignmentOperator"
local ConstantExpression = lpeg.V"ConstantExpression"
local ConditionalExpression = lpeg.V"ConditionalExpression"
local LogicalOrExpression = lpeg.V"LogicalOrExpression"
local LogicalAndExpression = lpeg.V"LogicalAndExpression"
local EqualityExpression = lpeg.V"EqualityExpression"
local RelationalExpression = lpeg.V"RelationalExpression"
local UnaryExpression = lpeg.V"UnaryExpression"
local PostfixExpression = lpeg.V"PostfixExpression"
local FunctionCall = lpeg.V"FunctionCall"
local _FunctionCall = lpeg.V"_FunctionCall"
local _ArgumentExpressionList = lpeg.V"_ArgumentExpressionList"
local PrimaryExpression = lpeg.V"PrimaryExpression"
local FunctionExpression = lpeg.V"FunctionExpression"
local Constant = lpeg.V"Constant"
local IntegerLiteral = lpeg.V"IntegerLiteral"
local DECIMAL = lpeg.V"DECIMAL"
local BooleanLiteral = lpeg.V"BooleanLiteral"
local StringLiteral = lpeg.V"StringLiteral"
local STRING_CONTENT = lpeg.V"STRING_CONTENT"
local NullLiteral = lpeg.V"NullLiteral"
G = lpeg.P{ File,
      File = (_ ) * (((((TopLevel ) * (_ ))^0) * (lpeg.P"" --[[Source]])) * (EOT ));
      _ = ( ( S  )  +  ( BLOCKCOMMENT  )  +  ( LINECOMMENT  ) )^0;
      S = lpeg.R("\t\n") + lpeg.P'\r' + lpeg.P" " ;
      BLOCKCOMMENT = (lpeg.P "/" ) * ((lpeg.P "*" ) * ((((-((lpeg.P "*" ) * (lpeg.P "/" ))) * (lpeg.P(1)))^0) * ((lpeg.P "*" ) * (lpeg.P "/" ))));
      LINECOMMENT = (lpeg.P "/" ) * ((lpeg.P "/" ) * (((-(lpeg.P '\n' )) * (lpeg.P(1)))^0));
      EOT = -(lpeg.P(1));
      Identifier = ((NAME ) * (lpeg.P"" --[[Name]])) * (_ );
      NAME = (-DIGIT ) * ((-KEYWORD ) * (W ^1));
      DIGIT = lpeg.R("09") ;
      W = lpeg.P"$" + lpeg.R("09") + lpeg.R("AZ") + lpeg.P"_" + lpeg.R("az") ;
      TopLevel = ( ( Statement  )  +  ( Declaration  ) )^1;
      KEYWORD = ( ( (lpeg.P "e" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * (lpeg.P "e" ))) )  +  ( (lpeg.P "i" ) * (lpeg.P "f" ) )  +  ( (lpeg.P "r" ) * ((lpeg.P "e" ) * ((lpeg.P "t" ) * ((lpeg.P "u" ) * ((lpeg.P "r" ) * (lpeg.P "n" ))))) )  +  ( (lpeg.P "f" ) * ((lpeg.P "u" ) * ((lpeg.P "n" ) * ((lpeg.P "c" ) * ((lpeg.P "t" ) * ((lpeg.P "i" ) * ((lpeg.P "o" ) * (lpeg.P "n" ))))))) )  +  ( (lpeg.P "v" ) * ((lpeg.P "a" ) * (lpeg.P "r" )) ) ) * (-W );
      Declaration =  ( FunctionDeclaration  )  +  ( VariableDeclaration  ) ;
      FunctionDeclaration = ((lpeg.P "f" ) * ((lpeg.P "u" ) * ((lpeg.P "n" ) * ((lpeg.P "c" ) * ((lpeg.P "t" ) * ((lpeg.P "i" ) * ((lpeg.P "o" ) * ((lpeg.P "n" ) * (-W ))))))))) * ((_ ) * ((Identifier ) * ((_ ) * (((lpeg.P "(" ) * (_ )) * ((FunctionParamList ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * (lpeg.P"" --[[FunctionDecl]]))))))));
      FunctionParamList = (((FunctionParam ) * ((((lpeg.P "," ) * (_ )) * (FunctionParam ))^0))^-1) * (lpeg.P"" --[[List]]);
      FunctionParam = ((Identifier ) * (lpeg.P"" --[[Param]])) * (_ );
      VariableDeclaration = ((lpeg.P "v" ) * ((lpeg.P "a" ) * ((lpeg.P "r" ) * (-W )))) * ((_ ) * ((VariableList ) * (((lpeg.P ";" ) * (_ )) * (lpeg.P"" --[[Declaration]]))));
      VariableList = (InitDecl ) * (((((lpeg.P "," ) * (_ )) * (InitDecl ))^0) * (lpeg.P"" --[[VarList]]));
      InitDecl = ((Identifier ) * (((((lpeg.P "=" ) * ((-(lpeg.P "=" )) * (_ ))) * (Initializer ))^-1) * (lpeg.P"" --[[VarDecl]]))) * (_ );
      Initializer = AssignmentExpression ;
      Block = ((lpeg.P "{" ) * (_ )) * ((( ( (Statement ) * (_ ) )  +  ( (Declaration ) * (_ ) ) )^0) * (((lpeg.P "}" ) * (_ )) * (lpeg.P"" --[[Block]])));
      Statement =  ( Block  )  +  ( ((lpeg.P "i" ) * ((lpeg.P "f" ) * (-W ))) * ((_ ) * (((lpeg.P "(" ) * (_ )) * ((Expression ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * ((lpeg.P "e" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * ((lpeg.P "e" ) * ((lpeg.P "1" ) * ((_ ) * ((Block ) * (lpeg.P"" --[[If1]]))))))))))))) )  +  ( ((lpeg.P "i" ) * ((lpeg.P "f" ) * (-W ))) * ((_ ) * (((lpeg.P "(" ) * (_ )) * ((Expression ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * ((lpeg.P "e" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * ((lpeg.P "e" ) * ((lpeg.P "2" ) * ((_ ) * ((Block ) * (lpeg.P"" --[[If2]]))))))))))))) )  +  ( ((lpeg.P "i" ) * ((lpeg.P "f" ) * (-W ))) * ((_ ) * (((lpeg.P "(" ) * (_ )) * ((Expression ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * ((lpeg.P "e" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * ((lpeg.P "e" ) * ((lpeg.P "3" ) * ((_ ) * ((Block ) * (lpeg.P"" --[[If3]]))))))))))))) )  +  ( ((lpeg.P "i" ) * ((lpeg.P "f" ) * (-W ))) * ((_ ) * (((lpeg.P "(" ) * (_ )) * ((Expression ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * ((lpeg.P "e" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * ((lpeg.P "e" ) * ((lpeg.P "4" ) * ((_ ) * ((Block ) * (lpeg.P"" --[[If4]]))))))))))))) )  +  ( ((lpeg.P "i" ) * ((lpeg.P "f" ) * (-W ))) * ((_ ) * (((lpeg.P "(" ) * (_ )) * ((Expression ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * ((lpeg.P "e" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * ((lpeg.P "e" ) * ((lpeg.P "5" ) * ((_ ) * ((Block ) * (lpeg.P"" --[[If5]]))))))))))))) )  +  ( ((lpeg.P "i" ) * ((lpeg.P "f" ) * (-W ))) * ((_ ) * (((lpeg.P "(" ) * (_ )) * ((Expression ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * ((lpeg.P "e" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * ((lpeg.P "e" ) * ((lpeg.P "6" ) * ((_ ) * ((Block ) * (lpeg.P"" --[[If6]]))))))))))))) )  +  ( ((lpeg.P "i" ) * ((lpeg.P "f" ) * (-W ))) * ((_ ) * (((lpeg.P "(" ) * (_ )) * ((Expression ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * ((lpeg.P "e" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * ((lpeg.P "e" ) * ((lpeg.P "7" ) * ((_ ) * ((Block ) * (lpeg.P"" --[[If7]]))))))))))))) )  +  ( ((lpeg.P "i" ) * ((lpeg.P "f" ) * (-W ))) * ((_ ) * (((lpeg.P "(" ) * (_ )) * ((Expression ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * ((lpeg.P "e" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * ((lpeg.P "e" ) * ((lpeg.P "8" ) * ((_ ) * ((Block ) * (lpeg.P"" --[[If8]]))))))))))))) )  +  ( ((lpeg.P "i" ) * ((lpeg.P "f" ) * (-W ))) * ((_ ) * (((lpeg.P "(" ) * (_ )) * ((Expression ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * ((lpeg.P "e" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * ((lpeg.P "e" ) * ((lpeg.P "9" ) * ((_ ) * ((Block ) * (lpeg.P"" --[[If9]]))))))))))))) )  +  ( ((lpeg.P "i" ) * ((lpeg.P "f" ) * (-W ))) * ((_ ) * (((lpeg.P "(" ) * (_ )) * ((Expression ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * ((lpeg.P "e" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * ((lpeg.P "e" ) * ((lpeg.P "A" ) * ((_ ) * ((Block ) * (lpeg.P"" --[[IfA]]))))))))))))) )  +  ( ((lpeg.P "i" ) * ((lpeg.P "f" ) * (-W ))) * ((_ ) * (((lpeg.P "(" ) * (_ )) * ((Expression ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * ((lpeg.P "e" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * ((lpeg.P "e" ) * ((lpeg.P "B" ) * ((_ ) * ((Block ) * (lpeg.P"" --[[IfB]]))))))))))))) )  +  ( ((lpeg.P "i" ) * ((lpeg.P "f" ) * (-W ))) * ((_ ) * (((lpeg.P "(" ) * (_ )) * ((Expression ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * ((lpeg.P "e" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * ((lpeg.P "e" ) * ((lpeg.P "C" ) * ((_ ) * ((Block ) * (lpeg.P"" --[[IfC]]))))))))))))) )  +  ( ((lpeg.P "i" ) * ((lpeg.P "f" ) * (-W ))) * ((_ ) * (((lpeg.P "(" ) * (_ )) * ((Expression ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * (((((lpeg.P "e" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * ((lpeg.P "e" ) * (-W ))))) * ((_ ) * (Block )))^-1) * (lpeg.P"" --[[If]]))))))) )  +  ( ((lpeg.P "r" ) * ((lpeg.P "e" ) * ((lpeg.P "t" ) * ((lpeg.P "u" ) * ((lpeg.P "r" ) * ((lpeg.P "n" ) * (-W ))))))) * ((_ ) * (((Expression )^-1) * (((lpeg.P ";" ) * (_ )) * (lpeg.P"" --[[Return]])))) )  +  ( (Expression ) * (((lpeg.P ";" ) * (_ )) * (lpeg.P"" --[[ExpressionStatement]])) ) ;
      Expression = (AssignmentExpression ) * ((((lpeg.P "," ) * (_ )) * ((AssignmentExpression ) * (lpeg.P"" --[[Expression]])))^0);
      AssignmentExpression =  ( (UnaryExpression ) * ((_AssignmentOperator ) * (AssignmentExpression )) )  +  ( ConditionalExpression  ) ;
      _AssignmentOperator =  ( ((lpeg.P "=" ) * ((-(lpeg.P "=" )) * (_ ))) * (lpeg.P"" --[[Assign]]) )  +  ( ((lpeg.P "*" ) * ((lpeg.P "=" ) * (_ ))) * (lpeg.P"" --[[AssignMul]]) )  +  ( ((lpeg.P "/" ) * ((lpeg.P "=" ) * (_ ))) * (lpeg.P"" --[[AssignDiv]]) )  +  ( ((lpeg.P "%" ) * ((lpeg.P "=" ) * (_ ))) * (lpeg.P"" --[[AssignMod]]) )  +  ( ((lpeg.P "+" ) * ((lpeg.P "=" ) * (_ ))) * (lpeg.P"" --[[AssignAdd]]) )  +  ( ((lpeg.P "-" ) * ((lpeg.P "=" ) * (_ ))) * (lpeg.P"" --[[AssignSub]]) )  +  ( ((lpeg.P "<" ) * ((lpeg.P "<" ) * ((lpeg.P "=" ) * (_ )))) * (lpeg.P"" --[[AssignLeftShift]]) )  +  ( ((lpeg.P ">" ) * ((lpeg.P ">" ) * ((lpeg.P "=" ) * (_ )))) * (lpeg.P"" --[[AssignRightShift]]) )  +  ( ((lpeg.P ">" ) * ((lpeg.P ">" ) * ((lpeg.P ">" ) * ((lpeg.P "=" ) * (_ ))))) * (lpeg.P"" --[[AssignLogicalRightShift]]) )  +  ( ((lpeg.P "&" ) * ((lpeg.P "=" ) * (_ ))) * (lpeg.P"" --[[AssignBitwiseAnd]]) )  +  ( ((lpeg.P "^" ) * ((lpeg.P "=" ) * (_ ))) * (lpeg.P"" --[[AssignBitwiseXOr]]) )  +  ( ((lpeg.P "|" ) * ((lpeg.P "=" ) * (_ ))) * (lpeg.P"" --[[AssignBitwiseOr]]) ) ;
      ConstantExpression = ConditionalExpression ;
      ConditionalExpression = (LogicalOrExpression ) * ((((lpeg.P "?" ) * (_ )) * ((Expression ) * (((lpeg.P ":" ) * ((-(lpeg.P ">" )) * (_ ))) * ((LogicalOrExpression ) * (lpeg.P"" --[[Conditional]])))))^0);
      LogicalOrExpression = (LogicalAndExpression ) * ((((lpeg.P "|" ) * ((lpeg.P "|" ) * (_ ))) * ((LogicalAndExpression ) * (lpeg.P"" --[[Or]])))^0);
      LogicalAndExpression = (EqualityExpression ) * ((((lpeg.P "&" ) * ((lpeg.P "&" ) * (_ ))) * ((EqualityExpression ) * (lpeg.P"" --[[And]])))^0);
      EqualityExpression = (RelationalExpression ) * ((( ( ((lpeg.P "=" ) * ((lpeg.P "=" ) * (_ ))) * (lpeg.P"" --[[Equals]]) )  +  ( ((lpeg.P "!" ) * ((lpeg.P "=" ) * (_ ))) * (lpeg.P"" --[[NotEquals]]) ) ) * (RelationalExpression ))^0);
      RelationalExpression = (UnaryExpression ) * ((( ( ((lpeg.P "<" ) * ((-(lpeg.P "=" )) * (_ ))) * (lpeg.P"" --[[LessThan]]) )  +  ( ((lpeg.P "<" ) * ((lpeg.P "=" ) * ((-(lpeg.P "=" )) * (_ )))) * (lpeg.P"" --[[LessThanEquals]]) )  +  ( ((lpeg.P ">" ) * ((-(lpeg.P "=" )) * (_ ))) * (lpeg.P"" --[[GreaterThan]]) )  +  ( ((lpeg.P ">" ) * ((lpeg.P "=" ) * ((-(lpeg.P "=" )) * (_ )))) * (lpeg.P"" --[[GreaterThanEquals]]) ) ) * (UnaryExpression ))^0);
      UnaryExpression =  ( PostfixExpression  )  +  ( ((lpeg.P "!" ) * ((-(lpeg.P "=" )) * (_ ))) * ((UnaryExpression ) * (lpeg.P"" --[[Not]])) ) ;
      PostfixExpression =  ( FunctionCall  )  +  ( PrimaryExpression  ) ;
      FunctionCall = (PrimaryExpression ) * ((_FunctionCall )^1);
      _FunctionCall = (((lpeg.P "(" ) * (_ )) * ((_ArgumentExpressionList ^-1) * (((lpeg.P ")" ) * (_ )) * (lpeg.P"" --[[List]])))) * (lpeg.P"" --[[Apply]]);
      _ArgumentExpressionList = (AssignmentExpression ) * ((((lpeg.P "," ) * (_ )) * (AssignmentExpression ))^0);
      PrimaryExpression =  ( Constant  )  +  ( ((lpeg.P "(" ) * (_ )) * ((Expression ) * ((lpeg.P ")" ) * (_ ))) )  +  ( FunctionExpression  )  +  ( Identifier  ) ;
      FunctionExpression = ((lpeg.P "f" ) * ((lpeg.P "u" ) * ((lpeg.P "n" ) * ((lpeg.P "c" ) * ((lpeg.P "t" ) * ((lpeg.P "i" ) * ((lpeg.P "o" ) * ((lpeg.P "n" ) * (-W ))))))))) * ((_ ) * ((((Identifier ) * (_ ))^-1) * (((lpeg.P "(" ) * (_ )) * ((FunctionParamList ) * (((lpeg.P ")" ) * (_ )) * ((Block ) * (((lpeg.P ":" ) * ((lpeg.P ":" ) * (_ ))) * (lpeg.P"" --[[FunctionExpr]]))))))));
      Constant =  ( IntegerLiteral  )  +  ( BooleanLiteral  )  +  ( StringLiteral  )  +  ( NullLiteral  ) ;
      IntegerLiteral = ((DECIMAL ) * (lpeg.P"" --[[Integer]])) * (_ );
      DECIMAL =  ( (lpeg.R("19") ) * (DIGIT ^0) )  +  ( lpeg.P "0"  ) ;
      BooleanLiteral =  ( ((lpeg.P "t" ) * ((lpeg.P "r" ) * ((lpeg.P "u" ) * ((lpeg.P "e" ) * (-W ))))) * ((lpeg.P"" --[[True]]) * (_ )) )  +  ( ((lpeg.P "f" ) * ((lpeg.P "a" ) * ((lpeg.P "l" ) * ((lpeg.P "s" ) * ((lpeg.P "e" ) * (-W )))))) * ((lpeg.P"" --[[False]]) * (_ )) ) ;
      StringLiteral = (lpeg.P "\"" ) * (((STRING_CONTENT ^0) * (lpeg.P"" --[[String]])) * ((lpeg.P "\"" ) * (_ )));
      STRING_CONTENT = (-(lpeg.P'\n' + lpeg.P"\"" + lpeg.P'\\' )) * (lpeg.P(1));
      NullLiteral = ((lpeg.P "n" ) * ((lpeg.P "u" ) * ((lpeg.P "l" ) * ((lpeg.P "l" ) * (-W ))))) * ((lpeg.P"" --[[Null]]) * (_ ));
}
function evalExp (s)
   latency = -1.0
   for i = 0, 5 do
      local t1 = os.clock()
      local t = lpeg.match(G, s)
      local e1 = os.clock() - t1
      --print("elapsedTime1 : ", e1)
      if latency == -1.0 or latency > e1 then
         latency = e1
      end
      if not t then return -2 end
      if (string.len(s) > t) then return -2 end
   end
   return (latency * 1000.0)
end

fileName = arg[1]
fh, msg = io.open(fileName, "r")
if fh then
   data = fh:read("*a")
else
   print(msg)
end
latency = evalExp(data)
if latency == -1 then
   print(fileName..", sytnaxerror")
elseif latency == -2 then
   print(fileName..", unconsumed ")
else
   print(fileName..", "..latency.." [ms]")
end