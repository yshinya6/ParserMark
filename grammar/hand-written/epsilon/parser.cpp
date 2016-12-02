#include <iostream>
#include <fstream>
#include <string>
#include <chrono>

using namespace std;

class Context {
  public:
    int pos;
    int len;
    string file;
    Context(string input="");
    void next();
    void next(int i);
    char peek();
    char lookahead(int la);
    bool range(char start, char end);
    bool range(char ch, char start, char end);
    bool match1(char c1);
    bool match2(char c1, char c2);
    bool match3(char c1, char c2, char c3);
    bool match4(char c1, char c2, char c3, char c4);
    bool match5(char c1, char c2, char c3, char c4, char c5);
    bool match6(char c1, char c2, char c3, char c4, char c5, char c6);
    bool match8(char c1, char c2, char c3, char c4, char c5, char c6, char c7, char c8);
};

Context::Context(string input){
  len = input.size();
  pos = 0;
  file = input;
}

void Context::next(){
  ++pos;
}

void Context::next(int i) {
  pos += i;
}

char Context::peek(){
  if (pos<len) {
    return file[pos];
  }
  return '\0';
}

char Context::lookahead(int la){
  int lapos = pos + la;
  if (lapos < len) {
    return file[lapos];
  }
  return '\0';
}

bool Context::range(char start, char end) {
  return (start <= peek() && peek() <= end );
}

bool Context::range(char ch, char start, char end) {
  return (start <= ch && ch <= end );
}

bool Context::match1(char c1) {
  return peek() == c1;
}

bool Context::match2(char c1, char c2) {
  return (peek() == c1 && lookahead(1) == c2);
}

bool Context::match3(char c1, char c2, char c3) {
  return (peek() == c1 && lookahead(1) == c2 && lookahead(2) == c3);
}

bool Context::match4(char c1, char c2, char c3, char c4) {
  return (peek() == c1 && lookahead(1) == c2 && lookahead(2) == c3 && lookahead(3) == c4);
}

bool Context::match5(char c1, char c2, char c3, char c4, char c5) {
  return (peek() == c1 && lookahead(1) == c2 && lookahead(2) == c3 && lookahead(3) == c4 && lookahead(4) == c5);
}

bool Context::match6(char c1, char c2, char c3, char c4, char c5, char c6){
  return (peek() == c1 && lookahead(1) == c2 && lookahead(2) == c3 && lookahead(3) == c4 && lookahead(4) == c5 && lookahead(5) == c6);
}

bool Context::match8(char c1, char c2, char c3, char c4, char c5, char c6, char c7, char c8){
  return (peek() == c1 && lookahead(1) == c2 && lookahead(2) == c3 && lookahead(3) == c4 && lookahead(4) == c5 && lookahead(5) == c6 && lookahead(6) == c7 && lookahead(7) == c8);
}

class Parser {
  public:
    Context ctx;
    Parser(string file);
    bool File();
    bool Space();
    bool S();
    bool BLOCKCOMMENT();
    bool LINECOMMENT();

    // Declaration
    bool Declaration();
    bool FunctionDeclaration();
    bool FunctionParamList();
    bool FunctionParam();
    bool VariableDeclaration();
    bool VariableList();
    bool InitDecl();
    bool Initializer();

    // Statement
    bool Block();
    bool BlockInner();
    bool Statement();
    bool IfStatement();
    bool ElseChoice();
    bool ReturnStatement();
    bool ExpressionStatement();

    // Expression
    bool Expression();
    bool ExpressionSeq();
    bool AssignmentExpression();
    bool AssignSeq();
    bool AssignmentOp();
    bool ConditionalExpression();
    bool ConditionalSeq();
    bool LogicalOrExpression();
    bool LogicalOrSeq();
    bool LogicalAndExpression();
    bool LogicalAndSeq();
    bool EqualityExpression();
    bool EqualitySeq();
    bool RelationalExpression();
    bool RelationalSeq();
    bool UnaryExpression();
    bool UnarySeq();
    bool PostfixExpression();
    bool FunctionCall();
    bool _FunctionCall();
    bool _ArgumentExpressionList();
    bool PrimaryExpression();
    bool FunctionExpression();
    bool GroupedExpression();

    // Identifier
    bool Identifier();
    bool KEYWORD();
    bool isWord(char c);

    // Literal
    bool Literal();
    bool IntegerLiteral();
    bool StringLiteral();
    bool NullLiteral();
    bool DECIMAL();
    bool BooleanLiteral();
    bool STRING_CONTENT();
    bool True();
    bool False();

};

Parser::Parser(string file) {
  ctx = Context(file);
}

bool Parser::File() {
  Space();
  if (!Declaration()) {
    return false;
  }
  while (true) {
    int ppos = ctx.pos;
    if (!Declaration()) {
      ctx.pos = ppos;
      break;
    }
  }
  std::cout << "pos: " << ctx.pos << '\n';
  std::cout << "len: "<< ctx.len << '\n';
  if (ctx.pos < ctx.len) {
    return false;
  }
  return true;
}

bool Parser::Space() {
  while (S() || BLOCKCOMMENT() || LINECOMMENT()) {
  }
  return true;
}

bool Parser::S() {
  if (ctx.peek() == ' ' || ctx.peek() == '\t' || ctx.peek() == '\n' || ctx.peek() == '\r' ) {
    ctx.next();
    return true;
  }
  return false;
}

bool Parser::BLOCKCOMMENT() {
  if (!ctx.match2('/', '*')) {
    return false;
  }
  ctx.next(2);
  while (true) {
    if (ctx.match2('*', '/')) {
      break;
    }
    if (ctx.pos >= ctx.len) {
      break;
    }
    ctx.next();
  }
  if (!ctx.match2('*', '/')) {
    return false;
  }
  ctx.next(2);
  return true;
}

bool Parser::LINECOMMENT() {
  if (!ctx.match2('/', '/')) {
    return false;
  }
  ctx.next(2);
  while (true) {
    if (ctx.match1('\n')) {
      break;
    }
    if (ctx.pos >= ctx.len) {
      break;
    }
    ctx.next();
  }
  return true;
}

bool Parser::Identifier() {
  if (ctx.range('0', '9')) {
    return false;
  }
  if (KEYWORD()) {
    return false;
  }
  if (!isWord(ctx.peek())) {
    return false;
  }
  ctx.next();
  while (isWord(ctx.peek())) {
    ctx.next();
  }
  Space();
  return true;
}

bool Parser::KEYWORD(){
  switch (ctx.peek()) {
    case 'i':
    if (ctx.match2('i', 'f') && !isWord(ctx.lookahead(2))) {
      return true;
    }
    break;
    case 'v':
    if (ctx.match3('v', 'a', 'r') && !isWord(ctx.lookahead(3))) {
      return true;
    }
    break;
    case 'e':
    if (ctx.match4('e', 'l', 's', 'e') && !isWord(ctx.lookahead(4))) {
      return true;
    }
    break;
    case 'r':
    if (ctx.match6('r', 'e', 't', 'u', 'r', 'n') && !isWord(ctx.lookahead(6))) {
      return true;
    }
    break;
    case 'f':
    if (ctx.match8('f','u','n','c','t','i','o','n') && !isWord(ctx.lookahead(8))) {
      return true;
    }
    break;
    default:
    return false;
  }
  return false;
}

bool Parser::isWord(char c) {
  return ('a'<= c && c <= 'z') || ('A'<= c && c <= 'Z') || ('0'<= c && c <= '9') || c == '_' || c == '$';
}

 /* Declaration */

bool Parser::Declaration() {
  bool ret = false;
  int ppos = ctx.pos;

  switch (ctx.peek()) {
    case 'f':
      if (!FunctionDeclaration()) {
        ctx.pos = ppos;
      }
      else {
        ret = true;
      }
      break;
    case 'v':
      if (!VariableDeclaration()) {
        ctx.pos = ppos;
      }
      else {
        ret = true;
      }
      break;
    default:
      break;
  }
  return ret;
}

bool Parser::FunctionDeclaration() {
  if (!ctx.match8('f','u','n','c','t','i','o','n')) {
    return false;
  }
  ctx.next(8);
  Space();
  int ppos = ctx.pos;
  if (!Identifier()) {
    ctx.pos = ppos;
  }
  Space();
  if (!FunctionParamList()) {
    return false;
  }
  Space();
  if (!Block()) {
    return false;
  }
  return true;
}

bool Parser::FunctionParamList() {
  if (!ctx.match1('(')) {
    return false;
  }
  ctx.next();
  Space();
  int ppos = ctx.pos;
  if (!FunctionParam()) {
    ctx.pos = ppos;
  }
  if (!ctx.match1(')')) {
    return false;
  }
  ctx.next();
  Space();
  return true;
}

bool Parser::FunctionParam() {
  if (!Identifier()) {
    return false;
  }
  while (true) {
    int ppos = ctx.pos;
    if (!ctx.match1(',')) {
      break;
    }
    ctx.next();
    Space();
    if (!Identifier()) {
      ctx.pos = ppos;
      break;
    }
  }
  return true;
}

bool Parser::VariableDeclaration() {
  if (!ctx.match3('v', 'a', 'r')) {
    return false;
  }
  ctx.next(3);
  Space();
  if (!VariableList()) {
    return false;
  }
  if (!ctx.match1(';')) {
    return false;
  }
  ctx.next();
  Space();
  return true;
}

bool Parser::VariableList() {
  if (!InitDecl()) {
    return false;
  }
  while (true) {
    int ppos = ctx.pos;
    if (!ctx.match1(',')) {
      break;
    }
    ctx.next();
    Space();
    if (!InitDecl()) {
      ctx.pos = ppos;
      break;
    }
  }
  return true;
}

bool Parser::InitDecl() {
  if (!Identifier()) {
    return false;
  }
  int ppos = ctx.pos;
  if (!Initializer()) {
    ctx.pos = ppos;
  }
  Space();
  return true;
}

bool Parser::Initializer() {
  if (!ctx.match1('=')) {
    return false;
  }
  ctx.next();
  Space();
  if (!AssignmentExpression()) {
    return false;
  }
  return true;
}


 /* Statement */
bool Parser::Block() {
  if (!ctx.match1('{')) {
    return false;
  }
  ctx.next();
  Space();
  while (true) {
    int ppos = ctx.pos;
    if (!BlockInner()) {
      ctx.pos = ppos;
      break;
    }
    Space();
  }
  if (!ctx.match1('}')) {
    return false;
  }
  ctx.next();
  Space();
  return true;
}

bool Parser::BlockInner() {
  bool ret = false;
  int ppos = ctx.pos;
  if (!ret) {
    if (!Statement()) {
      ctx.pos = ppos;
    }
    else {
      ret = true;
    }
  }
  if (!ret) {
    if (!Declaration()) {
      ctx.pos = ppos;
    }
    else {
      ret = true;
    }
  }
  return ret;
}

bool Parser::Statement() {
  bool ret = false;
  int ppos = ctx.pos;
  switch (ctx.peek()) {
    case '{':
      if(!Block()){
        ctx.pos = ppos;
        break;
      }
      else {
        ret = true;
      }
    case 'i':
      if(!IfStatement()){
        ctx.pos = ppos;
      }
      else {
        ret = true;
      }
      break;
    case 'r':
      if(!ReturnStatement()){
        ctx.pos = ppos;
      }
      else {
        ret = true;
      }
      break;
    default:
      break;
  }
  if (!ret) {
    if(!ExpressionStatement()){
      ctx.pos = ppos;
    }
    else {
      ret = true;
    }
  }
  return ret;
}

bool Parser::IfStatement() {
  if (!ctx.match2('i', 'f')) {
    return false;
  }
  ctx.next(2);
  Space();
  if (!ctx.match1('(')) {
    return false;
  }
  ctx.next();
  Space();
  if (!Expression()) {
    return false;
  }
  if (!ctx.match1(')')) {
    return false;
  }
  ctx.next();
  Space();
  if (!Block()) {
    return false;
  }
  int ppos = ctx.pos;
  if (ctx.match4('e', 'l', 's', 'e')) {
    ctx.next(4);
    if (!ElseChoice()) {
      ctx.pos = ppos;
    }
  }
  return true;
}

bool Parser::ElseChoice() {
  if (ctx.range('1', '9')) {
    ctx.next();
  }
  else if (ctx.range('A', 'C')) {
    ctx.next();
  }
  Space();
  if (!Block()) {
    return false;
  }
  return true;
}

bool Parser::ReturnStatement() {
  if (!ctx.match6('r', 'e', 't', 'u', 'r', 'n')) {
    return false;
  }
  ctx.next(6);
  Space();
  int ppos = ctx.pos;
  if (!Expression()) {
    ctx.pos = ppos;
  }
  if (!ctx.match1(';')) {
    return false;
  }
  ctx.next();
  Space();
  return true;
}

bool Parser::ExpressionStatement() {
  if (!Expression()) {
    return false;
  }
  if (!ctx.match1(';')) {
    return false;
  }
  ctx.next();
  Space();
  return true;
}

/* Expression */

bool Parser::Expression() {
  if (AssignmentExpression()) {
    while (true) {
      int ppos = ctx.pos;
      if (!ExpressionSeq()) {
        ctx.pos = ppos;
        break;
      }
    }
    return true;
  }
  return false;
}

bool Parser::ExpressionSeq() {
  if (!ctx.match1(',')) {
    return false;
  }
  ctx.next();
  Space();
  if (!AssignmentExpression()) {
    return false;
  }
  return true;
}

bool Parser::AssignmentExpression() {
  int ppos = ctx.pos;
  bool ret = false;
  if (!ret) {
    if (!AssignSeq()) {
      ctx.pos = ppos;
    }
    else{
      ret = true;
    }
  }
  if (!ret) {
    if (!ConditionalExpression()) {
      ctx.pos = ppos;
    }
    else{
      ret = true;
    }
  }
  return ret;
}

bool Parser::AssignSeq() {
  if (!UnaryExpression()) {
    return false;
  }
  if (!AssignmentOp()) {
    return false;
  }
  Space();
  if (!AssignmentExpression()) {
    return false;
  }
  return true;
}

bool Parser::AssignmentOp() {
  switch (ctx.peek()) {
    case '=':
      ctx.next();
      return true;
    case '*':
      if (ctx.match2('*', '=')) {
        ctx.next(2);
        return true;
      }
      break;
    case '/':
      if (ctx.match2('/', '=')) {
        ctx.next(2);
        return true;
      }
      break;
    case '%':
      if (ctx.match2('%', '=')) {
        ctx.next(2);
        return true;
      }
      break;
    case '+':
      if (ctx.match2('+', '=')) {
        ctx.next(2);
        return true;
      }
      break;
    case '-':
      if (ctx.match2('-', '=')) {
        ctx.next(2);
        return true;
      }
      break;
    case '<':
      if (ctx.match3('<','<','=')) {
        ctx.next(3);
        return true;
      }
      break;
    case '>':
      if (ctx.match3('>','>','=')) {
        ctx.next(3);
        return true;
      }
      if (ctx.match4('>','>','>','=')) {
        ctx.next(4);
        return true;
      }
      break;
    case '&':
      if (ctx.match2('&', '=')) {
        ctx.next(2);
        return true;
      }
      break;
    case '|':
      if (ctx.match2('|', '=')) {
        ctx.next(2);
        return true;
      }
      break;
  }
  return false;
}

bool Parser::ConditionalExpression() {
  if (LogicalOrExpression()) {
    while (true) {
      int ppos = ctx.pos;
      if (!ConditionalSeq()) {
        ctx.pos = ppos;
        break;
      }
    }
    return true;
  }
  return false;
}

bool Parser::ConditionalSeq() {
  if (!ctx.match1('?')) {
    return false;
  }
  ctx.next();
  Space();
  if (!Expression()) {
    return false;
  }
  if (!ctx.match1(':')) {
    return false;
  }
  ctx.next();
  Space();
  if (!LogicalOrExpression()) {
    return false;
  }
  return true;
}

bool Parser::LogicalOrExpression() {
  if (LogicalAndExpression()) {
    while (true) {
      int ppos = ctx.pos;
      if (!LogicalOrSeq()) {
        ctx.pos = ppos;
        break;
      }
    }
    return true;
  }
  return false;
}

bool Parser::LogicalOrSeq() {
  if (!ctx.match2('|','|')) {
    return false;
  }
  ctx.next(2);
  Space();
  if (!LogicalAndExpression()) {
    return false;
  }
  return true;
}

bool Parser::LogicalAndExpression() {
  if (EqualityExpression()) {
    while (true) {
      int ppos = ctx.pos;
      if (!LogicalAndSeq()) {
        ctx.pos = ppos;
        break;
      }
    }
    return true;
  }
  return false;
}

bool Parser::LogicalAndSeq() {
  if (!ctx.match2('&','&')) {
    return false;
  }
  ctx.next(2);
  Space();
  if (!EqualityExpression()) {
    return false;
  }
  return true;
}

bool Parser::EqualityExpression() {
  if (RelationalExpression()) {
    while (true) {
      int ppos = ctx.pos;
      if (!EqualitySeq()) {
        ctx.pos = ppos;
        break;
      }
    }
    return true;
  }
  return false;
}

bool Parser::EqualitySeq() {
  if (!(ctx.match2('=','=')||ctx.match2('!', '='))) {
    return false;
  }
  ctx.next(2);
  Space();
  if (!RelationalExpression()) {
    return false;
  }
  return true;
}

bool Parser::RelationalExpression() {
  if (UnaryExpression()) {
    while (true) {
      int ppos = ctx.pos;
      if (!RelationalSeq()) {
        ctx.pos = ppos;
        break;
      }
    }
    return true;
  }
  return false;
}

bool Parser::RelationalSeq() {
  if (ctx.match2('<','=') || ctx.match2('>', '=')) {
    ctx.next(2);
  }
  else if (ctx.match1('<') || ctx.match1('>')) {
    ctx.next();
  }
  else {
    return false;
  }
  Space();
  if (!UnaryExpression()) {
    return false;
  }
  return true;
}

bool Parser::UnaryExpression() {
  int ppos = ctx.pos;
  bool ret = false;
  if (!ret) {
    if (!PostfixExpression()) {
      ctx.pos = ppos;
    }
    else {
      ret = true;
    }
  }
  if (!ret) {
    if (!UnarySeq()) {
      ctx.pos = ppos;
    }
    else {
      ret = true;
    }
  }
  return ret;
}

bool Parser::UnarySeq() {
  if (!ctx.match1('!')){
    return false;
  }
  ctx.next(1);
  Space();
  if (!UnaryExpression()) {
    return false;
  }
  return true;
}

bool Parser::PostfixExpression() {
  int ppos = ctx.pos;
  bool ret = false;
  if (!ret) {
    if (!FunctionCall()) {
      ctx.pos = ppos;
    }
    else {
      ret = true;
    }
  }
  if (!ret) {
    if (!PrimaryExpression()) {
      ctx.pos = ppos;
    }
    else {
      ret = true;
    }
  }
  return ret;
}

bool Parser::FunctionCall() {
  if (!PrimaryExpression()) {
    return false;
  }
  if (!_FunctionCall()) {
    return false;
  }
  while (true) {
    int ppos = ctx.pos;
    if (!_FunctionCall()) {
      ctx.pos = ppos;
      break;
    }
  }
  return true;
}

bool Parser::_FunctionCall() {
  if (!ctx.match1('(')) {
    return false;
  }
  ctx.next();
  Space();
  int ppos = ctx.pos;
  if (!_ArgumentExpressionList()) {
    ctx.pos = ppos;
  }
  if (!ctx.match1(')')) {
    return false;
  }
  ctx.next();
  Space();
  return true;
}

bool Parser::_ArgumentExpressionList() {
  return Expression();
}

bool Parser::PrimaryExpression() {
  int ppos = ctx.pos;
  bool ret = false;
  if (!ret) {
    if (!Literal()) {
      ctx.pos = ppos;
    }
    else {
      ret = true;
    }
  }
  if (!ret) {
    if (!GroupedExpression()) {
      ctx.pos = ppos;
    }
    else {
      ret = true;
    }
  }
  if (!ret) {
    if (!FunctionExpression()) {
      ctx.pos = ppos;
    }
    else {
      ret = true;
    }
  }
  if (!ret) {
    if (!Identifier()) {
      ctx.pos = ppos;
    }
    else {
      ret = true;
    }
  }
  return ret;
}

bool Parser::GroupedExpression() {
  if (!ctx.match1('(')) {
    return false;
  }
  ctx.next();
  Space();
  if (!Expression()) {
    return false;
  }
  if (!ctx.match1(')')) {
    return false;
  }
  ctx.next();
  Space();
  return true;
}

bool Parser::FunctionExpression() {
  if (!ctx.match1('f')) {
    return false;
  }
  if (!ctx.match8('f','u','n','c','t','i','o','n')) {
    return false;
  }
  ctx.next(8);
  Space();
  int ppos = ctx.pos;
  if (!Identifier()) {
    ctx.pos = ppos;
  }
  Space();
  if (!FunctionParamList()) {
    return false;
  }
  Space();
  if (!Block()) {
    return false;
  }
  if (!ctx.match2(':',':')) {
    return false;
  }
  ctx.next(2);
  Space();
  return true;
}

/* Literal */

// Integer / Boolean / String / Null
bool Parser::Literal() {
  bool ret = false;
  int ppos = ctx.pos;
  if (!ret) {
    if (IntegerLiteral()) {
      ret = true;
    } else {
      ctx.pos = ppos;
    }
  }
  if (!ret) {
    if (BooleanLiteral()) {
      ret = true;
    } else {
      ctx.pos = ppos;
    }
  }
  if (!ret) {
    if (StringLiteral()) {
      ret = true;
    } else {
      ctx.pos = ppos;
    }
  }
  if (!ret) {
    if (NullLiteral()) {
      ret = true;
    } else {
      ctx.pos = ppos;
    }
  }
  return ret;
}

bool Parser::IntegerLiteral() {
  if (DECIMAL()) {
    Space();
    return true;
  }
  return false;
}

// [1-9] [0-9]* / '0'
bool Parser::DECIMAL() {
  bool ret = false;
  if (!ret) {
    if (ctx.range('1', '9')) {
      ctx.next();
      while (ctx.range('0', '9')) {
        ctx.next();
      }
      ret = true;
    }
  }
  if (!ret) {
    if (ctx.peek() == '0') {
      ret = true;
    }
    ctx.next();
  }
  return ret;
}

bool Parser::NullLiteral() {
  if (ctx.match4('n', 'u', 'l', 'l')) {
    ctx.next(4);
    Space();
    return true;
  }
  return false;
}

bool Parser::StringLiteral() {
  int ppos = ctx.pos;
  if (ctx.peek() != '"') {
    return false;
  }
  ctx.next();
  while (STRING_CONTENT()) {
    ctx.next();
  }
  if (ctx.peek() != '"') {
    ctx.pos = ppos;
    return false;
  }
  ctx.next();
  Space();
  return true;
}

bool Parser::STRING_CONTENT() {
  if (ctx.peek() == '"' || ctx.peek() == '\n' || ctx.peek() == '\\') {
    return false;
  }
  return true;
}

// True / False
bool Parser::BooleanLiteral() {
  switch (ctx.peek()) {
    case 't':
      return Parser::True();
    case 'f':
      return Parser::False();
    default:
      return false;
  }
}

// "true" _
bool Parser::True(){
  if (ctx.match4('t', 'r', 'u', 'e')) {
    ctx.next(4);
    Space();
    return true;
  }
  return false;
}

// "false" _
bool Parser::False(){
  if (ctx.match5('f', 'a', 'l', 's', 'e')) {
    ctx.next(5);
    Space();
    return true;
  }
  return false;
}

int main(int argc, char const *argv[]) {
  for (int i = 1; i < argc; i++) {
    ifstream ifs(argv[i]);
    if (ifs.fail()){
       cerr << "File Not Found: " << argv[i] << endl;
       return -1;
    }
    string input((istreambuf_iterator<char>(ifs)), istreambuf_iterator<char>());
    Parser parser(input);
    auto start = chrono::high_resolution_clock::now();
    bool result = parser.File();
    auto end = chrono::high_resolution_clock::now();

    if (result) {
      cout << "success!" << endl;
      cout << std::chrono::duration_cast<chrono::milliseconds>(end-start).count() << " [ms]" << endl;
    } else {
      cout << "syntax error" << endl;
    }
  }
}
