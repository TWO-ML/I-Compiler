#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <memory>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <cctype>
#include <cstdlib>
#include <iterator>
#include <algorithm>
#include <cmath>

using namespace std;

// Include all token and AST definitions from parser
enum class TokenKind
{
    Identifier, Integer, Real, Boolean,
    KW_var, KW_type, KW_integer, KW_real, KW_boolean,
    KW_record, KW_array, KW_is, KW_end, KW_while, KW_loop,
    KW_for, KW_in, KW_reverse, KW_if, KW_then, KW_else,
    KW_print, KW_routine, KW_and, KW_or, KW_xor, KW_not,
    KW_return, LParen, RParen, LBracket, RBracket,
    Comma, Colon, Dot, Range, Assign, Arrow,
    Plus, Minus, Star, Slash, Percent,
    LT, LE, GT, GE, EQ, NE, Apostrophe,
    Separator, EndOfFile, Error
};

struct Token
{
    TokenKind kind;
    string lexeme;
    int line = 1;
    int col = 1;
    long long intValue = 0;
    long double realValue = 0.0L;
    bool boolValue = false;
};

struct Lexer
{
    string src;
    size_t i = 0;
    int line = 1, col = 1;
    bool hasPendingError = false;
    Token pendingErrorToken;

    explicit Lexer(string s) : src(std::move(s)) {}

    bool eof() const { return i >= src.size(); }
    char peek(size_t k = 0) const { return (i + k < src.size()) ? src[i + k] : '\0'; }

    char advance()
    {
        if (eof()) return '\0';
        char c = src[i++];
        if (c == '\n') { line++; col = 1; } else { col++; }
        return c;
    }
    bool match(char expected) { if (peek() == expected) { advance(); return true; } return false; }

    static bool isIdentStart(char c) { return std::isalpha(static_cast<unsigned char>(c)) || c == '_'; }
    static bool isIdentCont(char c) { return std::isalnum(static_cast<unsigned char>(c)) || c == '_'; }

    void skipWhitespaceAndComments()
    {
        for (;;)
        {
            while (peek() == ' ' || peek() == '\t' || peek() == '\r') advance();
            if (peek() == '/' && peek(1) == '/')
            {
                while (peek() != '\n' && !eof()) advance();
                continue;
            }
            if (peek() == '/' && peek(1) == '*')
            {
                advance(); advance();
                while (!eof())
                {
                    if (peek() == '*' && peek(1) == '/') { advance(); advance(); break; }
                    advance();
                }
                continue;
            }
            break;
        }
    }

    static TokenKind keywordKind(const string &s, bool &isBoolLit, bool &boolVal)
    {
        static const unordered_map<string, TokenKind> kw = {
            {"var", TokenKind::KW_var}, {"type", TokenKind::KW_type},
            {"integer", TokenKind::KW_integer}, {"real", TokenKind::KW_real}, {"boolean", TokenKind::KW_boolean},
            {"record", TokenKind::KW_record}, {"array", TokenKind::KW_array},
            {"is", TokenKind::KW_is}, {"end", TokenKind::KW_end},
            {"while", TokenKind::KW_while}, {"loop", TokenKind::KW_loop},
            {"for", TokenKind::KW_for}, {"in", TokenKind::KW_in}, {"reverse", TokenKind::KW_reverse},
            {"if", TokenKind::KW_if}, {"then", TokenKind::KW_then}, {"else", TokenKind::KW_else},
            {"print", TokenKind::KW_print}, {"routine", TokenKind::KW_routine},
            {"and", TokenKind::KW_and}, {"or", TokenKind::KW_or}, {"xor", TokenKind::KW_xor}, {"not", TokenKind::KW_not},
            {"return", TokenKind::KW_return},
        };
        isBoolLit = false; boolVal = false;
        if (s == "true") { isBoolLit = true; boolVal = true; return TokenKind::Boolean; }
        if (s == "false") { isBoolLit = true; boolVal = false; return TokenKind::Boolean; }
        auto it = kw.find(s);
        return (it != kw.end()) ? it->second : TokenKind::Identifier;
    }

    Token make(TokenKind k, const string &lex, int l, int c)
    {
        Token t; t.kind = k; t.lexeme = lex; t.line = l; t.col = c; return t;
    }

    Token next()
    {
        if (hasPendingError) { hasPendingError = false; return pendingErrorToken; }
        skipWhitespaceAndComments();
        if (hasPendingError) { hasPendingError = false; return pendingErrorToken; }
        int tl = line, tc = col;
        if (eof()) return make(TokenKind::EndOfFile, "<eof>", tl, tc);
        char c = advance();
        if (c == '\n') return make(TokenKind::Separator, "\\n", tl, tc);
        if (isIdentStart(c))
        {
            string s(1, c);
            while (isIdentCont(peek())) s.push_back(advance());
            bool isBool = false, bval = false;
            TokenKind k = keywordKind(s, isBool, bval);
            Token t = make(k, s, tl, tc);
            if (k == TokenKind::Boolean) t.boolValue = bval;
            return t;
        }
        if (std::isdigit(static_cast<unsigned char>(c)))
        {
            string s(1, c);
            while (std::isdigit(static_cast<unsigned char>(peek()))) s.push_back(advance());
            if (peek() == '.' && peek(1) != '.' && std::isdigit(static_cast<unsigned char>(peek(1))))
            {
                s.push_back(advance());
                while (std::isdigit(static_cast<unsigned char>(peek()))) s.push_back(advance());
                Token t = make(TokenKind::Real, s, tl, tc);
                t.realValue = std::strtold(s.c_str(), nullptr);
                return t;
            }
            else
            {
                Token t = make(TokenKind::Integer, s, tl, tc);
                t.intValue = std::stoll(s);
                return t;
            }
        }
        switch (c)
        {
        case ':': return match('=') ? make(TokenKind::Assign, ":=", tl, tc) : make(TokenKind::Colon, ":", tl, tc);
        case '.': return match('.') ? make(TokenKind::Range, "..", tl, tc) : make(TokenKind::Dot, ".", tl, tc);
        case '=': return match('>') ? make(TokenKind::Arrow, "=>", tl, tc) : make(TokenKind::EQ, "=", tl, tc);
        case '<': return match('=') ? make(TokenKind::LE, "<=", tl, tc) : make(TokenKind::LT, "<", tl, tc);
        case '>': return match('=') ? make(TokenKind::GE, ">=", tl, tc) : make(TokenKind::GT, ">", tl, tc);
        case '/': return match('=') ? make(TokenKind::NE, "/=", tl, tc) : make(TokenKind::Slash, "/", tl, tc);
        case '\'': return make(TokenKind::Apostrophe, "'", tl, tc);
        case '+': return make(TokenKind::Plus, "+", tl, tc);
        case '-': return make(TokenKind::Minus, "-", tl, tc);
        case '*': return make(TokenKind::Star, "*", tl, tc);
        case '%': return make(TokenKind::Percent, "%", tl, tc);
        case '(': return make(TokenKind::LParen, "(", tl, tc);
        case ')': return make(TokenKind::RParen, ")", tl, tc);
        case '[': return make(TokenKind::LBracket, "[", tl, tc);
        case ']': return make(TokenKind::RBracket, "]", tl, tc);
        case ',': return make(TokenKind::Comma, ",", tl, tc);
        case ';': return make(TokenKind::Separator, ";", tl, tc);
        }
        return make(TokenKind::Error, string(1, c), tl, tc);
    }
};

struct ParseError : std::runtime_error { using std::runtime_error::runtime_error; };

enum class NodeKind
{
    Program, VarDecl, TypeDecl, RecordType, ArrayType, UserType, PrimType,
    RoutineDecl, RoutineHeader, RoutineBodyBlock, RoutineBodyExpr, Params, Param,
    Body, StmtList, Assign, While, For, If, Print, Call, ReturnStmt,
    Expr, Rel, Simple, Factor, Summand, Primary, ModPrimary, Name, Index, Member
};

struct AST
{
    NodeKind kind;
    string text;
    vector<unique_ptr<AST>> kids;
    int line = 1, col = 1;
    AST(NodeKind k, string t = "", int l = 1, int c = 1) : kind(k), text(std::move(t)), line(l), col(c) {}
    AST *add(unique_ptr<AST> ch) { kids.emplace_back(std::move(ch)); return kids.back().get(); }
};

unique_ptr<AST> nodeAt(NodeKind k, const Token &tok, string text = "")
{
    return std::make_unique<AST>(k, std::move(text), tok.line, tok.col);
}

// Forward declaration - we'll include parser code or copy necessary parts
// For now, we'll assume we can parse the AST

class WASMCompiler {
private:
    ostringstream wat;
    int labelCounter = 0;
    int localCounter = 0;
    
    struct VarInfo {
        string name;
        string type;  // "i32", "f64", "i32" for boolean
        int localIndex = -1;
        bool isGlobal = false;
        bool isArray = false;
        int arraySize = -1;
        string elemType;
    };
    
    unordered_map<string, VarInfo> globalVars;
    vector<unordered_map<string, VarInfo>> localScopes;
    unordered_map<string, int> routineParams;  // routine name -> param count
    
    string getWASMType(const string& piType) {
        if (piType == "integer") return "i32";
        if (piType == "real") return "f64";
        if (piType == "boolean") return "i32";
        return "i32";  // default
    }
    
    string newLabel() { return "L" + to_string(labelCounter++); }
    
    void emit(const string& code) { wat << code << "\n"; }
    void emitIndent(int indent, const string& code) { wat << string(indent * 2, ' ') << code << "\n"; }
    
    string inferTypeFromAST(AST* node) {
        if (!node) return "i32";
        if (node->kind == NodeKind::Primary) {
            if (!node->text.empty()) {
                if (node->text == "true" || node->text == "false") return "i32";
                if (isdigit(node->text[0]) || node->text[0] == '-' || node->text[0] == '+') {
                    if (node->text.find('.') != string::npos) return "f64";
                    return "i32";
                }
            }
        }
        return "i32";
    }
    
public:
    string compile(AST* root) {
        wat.str("");
        wat.clear();
        
        // WASM module header with WASI imports
        emit("(module");
        emit("  (import \"wasi_snapshot_preview1\" \"fd_write\"");
        emit("    (func $fd_write (param i32 i32 i32 i32) (result i32)))");
        emit("  (memory 1)");
        emit("  (export \"memory\" (memory 0))");
        emit("");
        emit("  ;; Helper function to print i32");
        emit("  (func $print_i32 (param $value i32)");
        emit("    (local $buf i32)");
        emit("    (local.set $buf (i32.const 0))");
        emit("    ;; Simple implementation: store value in memory and print");
        emit("    ;; TODO: Implement proper number-to-string conversion");
        emit("    i32.const 1  ;; stdout");
        emit("    i32.const 0  ;; iovs pointer");
        emit("    i32.const 1  ;; iovs_len");
        emit("    i32.const 0  ;; nwritten pointer");
        emit("    call $fd_write");
        emit("    drop");
        emit("  )");
        emit("");
        
        // Process global declarations and routines
        localScopes.push_back({});
        
        for (auto& child : root->kids) {
            if (child->kind == NodeKind::VarDecl) {
                compileVarDecl(child.get(), 2);
            } else if (child->kind == NodeKind::RoutineDecl) {
                compileRoutine(child.get(), 2);
            }
        }
        
        // Find and compile Main routine
        bool hasMain = false;
        for (auto& child : root->kids) {
            if (child->kind == NodeKind::RoutineDecl && 
                child->kids.size() > 0 &&
                child->kids[0]->kind == NodeKind::RoutineHeader &&
                child->kids[0]->text == "Main") {
                hasMain = true;
                break;
            }
        }
        
        if (hasMain) {
            emit("  (func (export \"_start\")");
            emit("    call $Main");
            emit("  )");
        }
        
        emit(")");
        
        return wat.str();
    }
    
    string compileExpression(AST* expr, int indent) {
        if (!expr) return "";
        
        switch (expr->kind) {
            case NodeKind::Primary: {
                return compilePrimary(expr, indent);
            }
            case NodeKind::Name: {
                string varName = expr->text;
                // Look up variable
                VarInfo* var = nullptr;
                for (int i = localScopes.size() - 1; i >= 0; i--) {
                    if (localScopes[i].count(varName)) {
                        var = &localScopes[i][varName];
                        break;
                    }
                }
                if (!var && globalVars.count(varName)) {
                    var = &globalVars[varName];
                }
                if (var) {
                    if (var->isGlobal) {
                        emitIndent(indent, "global.get $" + varName);
                    } else {
                        emitIndent(indent, "local.get $" + varName);
                    }
                }
                return var ? var->type : "i32";
            }
            case NodeKind::Expr: {
                // Logical operations: and, or, xor
                if (expr->text == "and" || expr->text == "or" || expr->text == "xor") {
                    compileExpression(expr->kids[0].get(), indent);
                    compileExpression(expr->kids[1].get(), indent);
                    if (expr->text == "and") {
                        emitIndent(indent, "i32.and");
                    } else if (expr->text == "or") {
                        emitIndent(indent, "i32.or");
                    } else if (expr->text == "xor") {
                        emitIndent(indent, "i32.xor");
                    }
                    return "i32";
                }
                break;
            }
            case NodeKind::Factor: {
                // Addition/subtraction
                if (expr->text == "+" || expr->text == "-") {
                    string type1 = compileExpression(expr->kids[0].get(), indent);
                    string type2 = compileExpression(expr->kids[1].get(), indent);
                    string opType = (type1 == "f64" || type2 == "f64") ? "f64" : "i32";
                    if (expr->text == "+") {
                        emitIndent(indent, opType + ".add");
                    } else {
                        emitIndent(indent, opType + ".sub");
                    }
                    return opType;
                }
                break;
            }
            case NodeKind::Simple: {
                // Multiplication/division/modulo
                if (expr->text == "*" || expr->text == "/" || expr->text == "%") {
                    string type1 = compileExpression(expr->kids[0].get(), indent);
                    string type2 = compileExpression(expr->kids[1].get(), indent);
                    string opType = (type1 == "f64" || type2 == "f64") ? "f64" : "i32";
                    if (expr->text == "*") {
                        emitIndent(indent, opType + ".mul");
                    } else if (expr->text == "/") {
                        emitIndent(indent, opType + ".div_" + (opType == "f64" ? "s" : "s"));
                    } else {
                        emitIndent(indent, "i32.rem_s");
                    }
                    return opType;
                }
                break;
            }
            case NodeKind::Rel: {
                // Comparison operations
                string type1 = compileExpression(expr->kids[0].get(), indent);
                string type2 = compileExpression(expr->kids[1].get(), indent);
                string opType = (type1 == "f64" || type2 == "f64") ? "f64" : "i32";
                
                if (expr->text == "<") {
                    emitIndent(indent, opType + ".lt_" + (opType == "f64" ? "s" : "s"));
                } else if (expr->text == "<=") {
                    emitIndent(indent, opType + ".le_" + (opType == "f64" ? "s" : "s"));
                } else if (expr->text == ">") {
                    emitIndent(indent, opType + ".gt_" + (opType == "f64" ? "s" : "s"));
                } else if (expr->text == ">=") {
                    emitIndent(indent, opType + ".ge_" + (opType == "f64" ? "s" : "s"));
                } else if (expr->text == "=") {
                    emitIndent(indent, opType + ".eq");
                } else if (expr->text == "/=") {
                    emitIndent(indent, opType + ".ne");
                }
                // Comparisons already return i32, no need to wrap
                return "i32";
            }
            case NodeKind::Call: {
                string funcName = expr->text;
                emitIndent(indent, ";; call " + funcName);
                for (auto& arg : expr->kids) {
                    compileExpression(arg.get(), indent);
                }
                emitIndent(indent, "call $" + funcName);
                return "i32";  // Assume i32 return for now
            }
            case NodeKind::ModPrimary: {
                // Variable access with indexing or member access
                if (expr->kids.size() > 0 && expr->kids[0]->kind == NodeKind::Name) {
                    string varName = expr->kids[0]->text;
                    VarInfo* var = nullptr;
                    for (int i = localScopes.size() - 1; i >= 0; i--) {
                        if (localScopes[i].count(varName)) {
                            var = &localScopes[i][varName];
                            break;
                        }
                    }
                    if (!var && globalVars.count(varName)) {
                        var = &globalVars[varName];
                    }
                    
                    if (var) {
                        if (var->isGlobal) {
                            emitIndent(indent, "global.get $" + varName);
                        } else {
                            emitIndent(indent, "local.get $" + varName);
                        }
                        
                        // Handle indexing
                        for (size_t i = 1; i < expr->kids.size(); i++) {
                            if (expr->kids[i]->kind == NodeKind::Index) {
                                compileExpression(expr->kids[i]->kids[0].get(), indent);
                                emitIndent(indent, "i32.add");
                                emitIndent(indent, "i32.load");
                            }
                        }
                    }
                    return var ? var->type : "i32";
                }
                break;
            }
            default:
                break;
        }
        return "i32";
    }
    
    string compilePrimary(AST* primary, int indent) {
        if (!primary) return "i32";
        
        if (primary->text == "true") {
            emitIndent(indent, "i32.const 1");
            return "i32";
        } else if (primary->text == "false") {
            emitIndent(indent, "i32.const 0");
            return "i32";
        } else if (!primary->text.empty()) {
            // Check if it's a number
            if (isdigit(primary->text[0]) || primary->text[0] == '-' || primary->text[0] == '+') {
                if (primary->text.find('.') != string::npos) {
                    // Real number
                    double val = stod(primary->text);
                    emitIndent(indent, "f64.const " + to_string(val));
                    return "f64";
                } else {
                    // Integer
                    long long val = stoll(primary->text);
                    emitIndent(indent, "i32.const " + to_string((int)val));
                    return "i32";
                }
            }
        }
        
        // If it's a name, compile it
        if (primary->kind == NodeKind::Name) {
            return compileExpression(primary, indent);
        }
        
        return "i32";
    }
    
    void compileStatement(AST* stmt, int indent) {
        if (!stmt) return;
        
        switch (stmt->kind) {
            case NodeKind::Assign: {
                AST* lhs = stmt->kids[0].get();
                AST* rhs = stmt->kids[1].get();
                
                // Compile RHS
                compileExpression(rhs, indent);
                
                // Compile LHS and store
                if (lhs->kind == NodeKind::ModPrimary && lhs->kids.size() > 0) {
                    AST* nameNode = lhs->kids[0].get();
                    if (nameNode->kind == NodeKind::Name) {
                        string varName = nameNode->text;
                        VarInfo* var = nullptr;
                        for (int i = localScopes.size() - 1; i >= 0; i--) {
                            if (localScopes[i].count(varName)) {
                                var = &localScopes[i][varName];
                                break;
                            }
                        }
                        if (!var && globalVars.count(varName)) {
                            var = &globalVars[varName];
                        }
                        
                        if (var) {
                            if (lhs->kids.size() > 1 && lhs->kids[1]->kind == NodeKind::Index) {
                                // Array assignment
                                compileExpression(nameNode, indent);  // base address
                                compileExpression(lhs->kids[1]->kids[0].get(), indent);  // index
                                emitIndent(indent, "i32.add");
                                emitIndent(indent, "i32.store");
                            } else {
                                if (var->isGlobal) {
                                    emitIndent(indent, "global.set $" + varName);
                                } else {
                                    emitIndent(indent, "local.set $" + varName);
                                }
                            }
                        }
                    }
                }
                break;
            }
            case NodeKind::Print: {
                for (auto& expr : stmt->kids) {
                    compileExpression(expr.get(), indent);
                    // Print using WASI fd_write
                    // Stack: value
                    // We need to convert to string and write to stdout
                    // For simplicity, we'll print numbers directly
                    // TODO: Implement proper string conversion
                    emitIndent(indent, "call $print_i32");
                }
                break;
            }
            case NodeKind::If: {
                string elseLabel = newLabel();
                string endLabel = newLabel();
                
                compileExpression(stmt->kids[0].get(), indent);
                emitIndent(indent, "i32.eqz");
                emitIndent(indent, "br_if $" + elseLabel);
                
                // Then branch
                if (stmt->kids.size() > 1 && stmt->kids[1]->kind == NodeKind::Body) {
                    for (auto& child : stmt->kids[1]->kids) {
                        compileStatement(child.get(), indent);
                    }
                }
                emitIndent(indent, "br $" + endLabel);
                emitIndent(indent, "$" + elseLabel + ":");
                
                // Else branch
                if (stmt->kids.size() > 2 && stmt->kids[2]->kind == NodeKind::Body) {
                    for (auto& child : stmt->kids[2]->kids) {
                        compileStatement(child.get(), indent);
                    }
                }
                emitIndent(indent, "$" + endLabel + ":");
                break;
            }
            case NodeKind::While: {
                string loopLabel = newLabel();
                string endLabel = newLabel();
                
                emitIndent(indent, "$" + loopLabel + ":");
                compileExpression(stmt->kids[0].get(), indent);
                emitIndent(indent, "i32.eqz");
                emitIndent(indent, "br_if $" + endLabel);
                
                if (stmt->kids.size() > 1 && stmt->kids[1]->kind == NodeKind::Body) {
                    for (auto& child : stmt->kids[1]->kids) {
                        compileStatement(child.get(), indent);
                    }
                }
                emitIndent(indent, "br $" + loopLabel);
                emitIndent(indent, "$" + endLabel + ":");
                break;
            }
            case NodeKind::For: {
                // for Identifier in Range [ reverse ] loop Body end
                if (stmt->kids.size() < 2) break;
                
                string loopVarName = stmt->text;
                string loopLabel = newLabel();
                string endLabel = newLabel();
                bool isReverse = false;
                
                // Check for reverse
                for (size_t i = 1; i < stmt->kids.size(); i++) {
                    if (stmt->kids[i]->kind == NodeKind::Name && stmt->kids[i]->text == "reverse") {
                        isReverse = true;
                        break;
                    }
                }
                
                // Range: Expression [ .. Expression ] or single Expression (array)
                AST* rangeNode = stmt->kids[0].get();
                
                if (rangeNode->kind == NodeKind::Expr && rangeNode->text == "..") {
                    // Range with two expressions
                    if (rangeNode->kids.size() >= 2) {
                        // Evaluate start and end (simplified - assume constants for now)
                        // TODO: Handle non-constant ranges
                        emitIndent(indent, ";; for loop: " + loopVarName);
                        emitIndent(indent, "(local $" + loopVarName + " i32)");
                        
                        // Initialize loop variable
                        compileExpression(rangeNode->kids[0].get(), indent);
                        if (isReverse) {
                            compileExpression(rangeNode->kids[1].get(), indent);
                        } else {
                            compileExpression(rangeNode->kids[0].get(), indent);
                        }
                        emitIndent(indent, "local.set $" + loopVarName);
                        
                        emitIndent(indent, "$" + loopLabel + ":");
                        
                        // Check condition
                        emitIndent(indent, "local.get $" + loopVarName);
                        if (isReverse) {
                            compileExpression(rangeNode->kids[0].get(), indent);
                            emitIndent(indent, "i32.lt_s");
                        } else {
                            compileExpression(rangeNode->kids[1].get(), indent);
                            emitIndent(indent, "i32.gt_s");
                        }
                        emitIndent(indent, "br_if $" + endLabel);
                        
                        // Body
                        AST* bodyNode = nullptr;
                        for (size_t i = 1; i < stmt->kids.size(); i++) {
                            if (stmt->kids[i]->kind == NodeKind::Body) {
                                bodyNode = stmt->kids[i].get();
                                break;
                            }
                        }
                        if (bodyNode) {
                            for (auto& child : bodyNode->kids) {
                                compileStatement(child.get(), indent);
                            }
                        }
                        
                        // Increment/decrement
                        emitIndent(indent, "local.get $" + loopVarName);
                        if (isReverse) {
                            emitIndent(indent, "i32.const 1");
                            emitIndent(indent, "i32.sub");
                        } else {
                            emitIndent(indent, "i32.const 1");
                            emitIndent(indent, "i32.add");
                        }
                        emitIndent(indent, "local.set $" + loopVarName);
                        emitIndent(indent, "br $" + loopLabel);
                        emitIndent(indent, "$" + endLabel + ":");
                    }
                } else {
                    // Single expression (array iteration) - simplified
                    emitIndent(indent, ";; for loop over array: " + loopVarName);
                    emitIndent(indent, "(local $" + loopVarName + " i32)");
                    emitIndent(indent, "i32.const 1");
                    emitIndent(indent, "local.set $" + loopVarName);
                    emitIndent(indent, "$" + loopLabel + ":");
                    // TODO: Implement array iteration
                    emitIndent(indent, "br $" + endLabel);
                    emitIndent(indent, "$" + endLabel + ":");
                }
                break;
            }
            case NodeKind::ReturnStmt: {
                if (stmt->kids.size() > 0) {
                    compileExpression(stmt->kids[0].get(), indent);
                }
                emitIndent(indent, "return");
                break;
            }
            case NodeKind::Call: {
                compileExpression(stmt, indent);
                emitIndent(indent, "drop");
                break;
            }
            case NodeKind::Body: {
                for (auto& child : stmt->kids) {
                    compileStatement(child.get(), indent);
                }
                break;
            }
            case NodeKind::VarDecl: {
                compileVarDecl(stmt, indent);
                break;
            }
            default:
                break;
        }
    }
    
    void compileVarDecl(AST* varDecl, int indent) {
        if (varDecl->kind != NodeKind::VarDecl) return;
        
        string varName = varDecl->text;
        VarInfo var;
        var.name = varName;
        
        // Determine type
        if (varDecl->kids.size() > 0) {
            AST* typeNode = varDecl->kids[0].get();
            if (typeNode->kind == NodeKind::PrimType) {
                var.type = getWASMType(typeNode->text);
            } else if (typeNode->kind == NodeKind::ArrayType) {
                var.isArray = true;
                if (typeNode->kids.size() > 1) {
                    AST* elemType = typeNode->kids[1].get();
                    if (elemType->kind == NodeKind::PrimType) {
                        var.elemType = getWASMType(elemType->text);
                    }
                }
                var.type = "i32";  // Array base address
            } else if (typeNode->kind == NodeKind::UserType) {
                // User-defined type - need to resolve it
                // For now, default to i32 (should be resolved from type table)
                var.type = "i32";
            } else if (typeNode->kind == NodeKind::RecordType) {
                // Record type - store as i32 pointer
                var.type = "i32";
            }
        }
        
        // Ensure type is set (default to i32 if not determined)
        if (var.type.empty()) {
            var.type = "i32";
        }
        
        // Check if global or local
        bool isGlobal = (localScopes.size() == 1);
        
        if (isGlobal) {
            var.isGlobal = true;
            globalVars[varName] = var;
            emitIndent(indent, "(global $" + varName + " (mut " + var.type + ")");
            if (varDecl->kids.size() > 1) {
                // Has initializer
                compileExpression(varDecl->kids[1].get(), indent + 1);
            } else {
                emitIndent(indent + 1, var.type + ".const 0");
            }
            emitIndent(indent, ")");
        } else {
            var.isGlobal = false;
            var.localIndex = localCounter++;
            localScopes.back()[varName] = var;
            emitIndent(indent, "(local $" + varName + " " + var.type + ")");
            if (varDecl->kids.size() > 1) {
                compileExpression(varDecl->kids[1].get(), indent);
                emitIndent(indent, "local.set $" + varName);
            }
        }
    }
    
    void compileRoutine(AST* routine, int indent) {
        if (routine->kind != NodeKind::RoutineDecl || routine->kids.empty()) return;
        
        AST* header = routine->kids[0].get();
        if (header->kind != NodeKind::RoutineHeader) return;
        
        string funcName = header->text;
        if (funcName.empty()) return;
        
        emitIndent(indent, "(func $" + funcName);
        
        // Parameters
        localScopes.push_back({});
        localCounter = 0;
        if (header->kids.size() > 0 && header->kids[0]->kind == NodeKind::Params) {
            AST* params = header->kids[0].get();
            for (auto& param : params->kids) {
                if (param->kind == NodeKind::Param) {
                    string paramName = param->text;
                    string paramType = "i32";
                    if (param->kids.size() > 0) {
                        AST* typeNode = param->kids[0].get();
                        if (typeNode->kind == NodeKind::PrimType) {
                            paramType = getWASMType(typeNode->text);
                        }
                    }
                    emitIndent(indent + 1, "(param $" + paramName + " " + paramType + ")");
                    VarInfo var;
                    var.name = paramName;
                    var.type = paramType;
                    var.localIndex = localCounter++;
                    localScopes.back()[paramName] = var;
                }
            }
        }
        
        // Return type
        string returnType = "";
        size_t bodyIndex = 1;  // Body is usually at index 1 (after Params)
        if (header->kids.size() > 1 && header->kids[1]->kind == NodeKind::PrimType) {
            returnType = getWASMType(header->kids[1]->text);
            emitIndent(indent + 1, "(result " + returnType + ")");
            bodyIndex = 2;  // If return type exists, body is at index 2
        }
        
        // Body - check in header (RoutineBodyBlock is added to RoutineHeader)
        if (header->kids.size() > bodyIndex) {
            AST* body = header->kids[bodyIndex].get();
            if (body->kind == NodeKind::RoutineBodyBlock) {
                if (body->kids.size() > 0 && body->kids[0]->kind == NodeKind::Body) {
                    for (auto& stmt : body->kids[0]->kids) {
                        compileStatement(stmt.get(), indent + 1);
                    }
                }
            } else if (body->kind == NodeKind::RoutineBodyExpr) {
                if (body->kids.size() > 0) {
                    compileExpression(body->kids[0].get(), indent + 1);
                }
            }
        }
        
        if (returnType.empty()) {
            emitIndent(indent + 1, "return");
        }
        
        localScopes.pop_back();
        emitIndent(indent, ")");
    }
};

// Helper function to get node kind name (needed for parser integration)
string tokenKindName(TokenKind k) {
    switch (k) {
#define C(x) case TokenKind::x: return #x;
        C(Identifier) C(Integer) C(Real) C(Boolean)
        C(KW_var) C(KW_type) C(KW_integer) C(KW_real) C(KW_boolean)
        C(KW_record) C(KW_array) C(KW_is) C(KW_end) C(KW_while) C(KW_loop)
        C(KW_for) C(KW_in) C(KW_reverse) C(KW_if) C(KW_then) C(KW_else)
        C(KW_print) C(KW_routine) C(KW_and) C(KW_or) C(KW_xor) C(KW_not)
        C(KW_return) C(LParen) C(RParen) C(LBracket) C(RBracket)
        C(Comma) C(Colon) C(Dot) C(Range) C(Assign) C(Arrow)
        C(Plus) C(Minus) C(Star) C(Slash) C(Percent)
        C(LT) C(LE) C(GT) C(GE) C(EQ) C(NE) C(Apostrophe)
        C(Separator) C(EndOfFile) C(Error)
#undef C
    }
    return "?";
}

// Include the full parser implementation
// For brevity, we'll include a simplified version that has the essential parsing methods
// In production, you would include the full parser from parser.cpp

struct Parser {
    Lexer &L;
    Token t;
    bool buildAST = true;
    vector<string> errors;

    explicit Parser(Lexer &lex, bool ast = true) : L(lex), buildAST(ast) { advance(); }

    [[noreturn]] void throwErr(const string &msg) {
        std::ostringstream os;
        os << "Syntax error at " << t.line << ":" << t.col << ": " << msg
           << " (got " << tokenKindName(t.kind) << " \"" << t.lexeme << "\")";
        throw ParseError(os.str());
    }
    
    void error(const string &msg) {
        std::ostringstream os;
        os << "Syntax error at " << t.line << ":" << t.col << ": " << msg
           << " (got " << tokenKindName(t.kind) << " \"" << t.lexeme << "\")";
        errors.push_back(os.str());
        while (t.kind != TokenKind::EndOfFile &&
               t.kind != TokenKind::Separator &&
               t.kind != TokenKind::KW_end &&
               t.kind != TokenKind::RParen && t.kind != TokenKind::RBracket) {
            advance();
        }
        if (t.kind == TokenKind::Separator) advance();
    }
    
    void advance() {
        t = L.next();
        skipSeparators();
    }
    
    bool check(TokenKind k) const { return t.kind == k; }
    bool accept(TokenKind k) {
        if (t.kind == k) { advance(); return true; }
        return false;
    }
    void expect(TokenKind k, const char *what) {
        if (!accept(k)) throwErr(string("expected ") + what);
    }
    void skipSeparators() {
        while (t.kind == TokenKind::Separator) t = L.next();
    }

    unique_ptr<AST> parseProgram() {
        auto root = node(NodeKind::Program);
        while (!check(TokenKind::EndOfFile)) {
            if (isStartOfSimpleDecl()) {
                root->add(parseSimpleDecl());
            } else if (check(TokenKind::KW_routine)) {
                root->add(parseRoutineDecl());
            } else {
                error("expected declaration (var/type/routine)");
                if (check(TokenKind::EndOfFile)) break;
                advance();
            }
        }
        return root;
    }

    bool isStartOfSimpleDecl() {
        return check(TokenKind::KW_var) || check(TokenKind::KW_type);
    }

    unique_ptr<AST> parseSimpleDecl() {
        if (accept(TokenKind::KW_var)) {
            string name = t.lexeme;
            int l = t.line, c = t.col;
            expect(TokenKind::Identifier, "identifier after 'var'");
            auto nodeVar = node(NodeKind::VarDecl, name, l, c);
            if (accept(TokenKind::Colon)) {
                nodeVar->add(parseType());
                if (accept(TokenKind::KW_is)) {
                    nodeVar->add(parseExpression());
                }
            } else if (accept(TokenKind::KW_is)) {
                nodeVar->add(parseExpression());
            } else {
                throwErr("expected ':' or 'is' in variable declaration");
            }
            return nodeVar;
        }
        if (accept(TokenKind::KW_type)) {
            string name = t.lexeme;
            int l = t.line, c = t.col;
            expect(TokenKind::Identifier, "type name");
            expect(TokenKind::KW_is, "keyword 'is'");
            auto td = node(NodeKind::TypeDecl, name, l, c);
            td->add(parseType());
            return td;
        }
        throwErr("expected simple declaration");
        return nullptr;
    }

    unique_ptr<AST> parseType() {
        if (check(TokenKind::KW_integer) || check(TokenKind::KW_real) || check(TokenKind::KW_boolean)) {
            string w = t.lexeme;
            int l = t.line, c = t.col;
            advance();
            return node(NodeKind::PrimType, w, l, c);
        }
        if (check(TokenKind::KW_array) || check(TokenKind::KW_record)) {
            return parseUserType();
        }
        if (check(TokenKind::Identifier)) {
            string name = t.lexeme;
            int l = t.line, c = t.col;
            advance();
            return node(NodeKind::UserType, name, l, c);
        }
        throwErr("expected type");
        return nullptr;
    }

    unique_ptr<AST> parseUserType() {
        if (check(TokenKind::KW_record)) {
            Token recTok = t;
            advance();
            auto rec = nodeAt(NodeKind::RecordType, recTok);
            while (!check(TokenKind::KW_end) && !check(TokenKind::EndOfFile)) {
                if (!check(TokenKind::KW_var)) {
                    throwErr("in record: expected 'var' declaration");
                }
                rec->add(parseSimpleDecl());
            }
            expect(TokenKind::KW_end, "end");
            return rec;
        }
        if (check(TokenKind::KW_array)) {
            Token arrTok = t;
            advance();
            auto arr = nodeAt(NodeKind::ArrayType, arrTok);
            expect(TokenKind::LBracket, "'[' after 'array'");
            if (check(TokenKind::RBracket)) {
                advance();
            } else {
                arr->add(parseExpression());
                expect(TokenKind::RBracket, "']' after array size");
            }
            arr->add(parseType());
            return arr;
        }
        throwErr("expected 'record' or 'array'");
        return nullptr;
    }

    unique_ptr<AST> parseRoutineDecl() {
        if (!check(TokenKind::KW_routine)) throwErr("expected 'routine'");
        Token routTok = t;
        auto hdr = parseRoutineHeader();
        if (check(TokenKind::KW_is) || check(TokenKind::Arrow)) {
            hdr->add(parseRoutineBody());
        }
        auto decl = nodeAt(NodeKind::RoutineDecl, routTok);
        decl->add(std::move(hdr));
        return decl;
    }

    unique_ptr<AST> parseRoutineHeader() {
        Token routineTok = t;
        expect(TokenKind::KW_routine, "'routine'");
        Token nameTok = t;
        expect(TokenKind::Identifier, "routine name");
        auto H = nodeAt(NodeKind::RoutineHeader, nameTok, nameTok.lexeme);
        expect(TokenKind::LParen, "'('");
        auto params = node(NodeKind::Params);
        if (!check(TokenKind::RParen)) {
            for (;;) {
                string pn = t.lexeme;
                int pl = t.line, pc = t.col;
                expect(TokenKind::Identifier, "parameter name");
                expect(TokenKind::Colon, "':' in parameter");
                auto P = node(NodeKind::Param, pn, pl, pc);
                P->add(parseType());
                params->add(std::move(P));
                if (!accept(TokenKind::Comma)) break;
            }
        }
        expect(TokenKind::RParen, "')'");
        H->add(std::move(params));
        if (accept(TokenKind::Colon)) {
            H->add(parseType());
        }
        return H;
    }

    unique_ptr<AST> parseRoutineBody() {
        if (check(TokenKind::KW_is)) {
            Token tok = t;
            advance();
            auto B = nodeAt(NodeKind::RoutineBodyBlock, tok);
            B->add(parseBody());
            expect(TokenKind::KW_end, "'end' of routine body");
            return B;
        }
        if (check(TokenKind::Arrow)) {
            Token tok = t;
            advance();
            auto E = nodeAt(NodeKind::RoutineBodyExpr, tok);
            E->add(parseExpression());
            return E;
        }
        throwErr("expected 'is' or '=>' in routine body");
        return nullptr;
    }

    unique_ptr<AST> parseBody() {
        auto B = node(NodeKind::Body);
        while (!check(TokenKind::KW_end) && !check(TokenKind::EndOfFile)) {
            if (isStartOfSimpleDecl())
                B->add(parseSimpleDecl());
            else if (isStartOfStatement())
                B->add(parseStatement());
            else
                break;
        }
        return B;
    }

    bool isStartOfStatement() {
        return check(TokenKind::Identifier) || check(TokenKind::KW_while) || check(TokenKind::KW_for) ||
               check(TokenKind::KW_if) || check(TokenKind::KW_print) || check(TokenKind::KW_return);
    }

    unique_ptr<AST> parseStatement() {
        if (check(TokenKind::KW_while)) return parseWhile();
        if (check(TokenKind::KW_for)) return parseFor();
        if (check(TokenKind::KW_if)) return parseIf();
        if (check(TokenKind::KW_print)) return parsePrint();
        if (check(TokenKind::KW_return)) return parseReturn();
        return parseModifiablePrimaryOrCall(true);
    }

    unique_ptr<AST> parseWhile() {
        Token wTok = t;
        expect(TokenKind::KW_while, "'while'");
        auto W = nodeAt(NodeKind::While, wTok, "while");
        W->add(parseExpression());
        expect(TokenKind::KW_loop, "'loop'");
        W->add(parseBody());
        expect(TokenKind::KW_end, "'end'");
        return W;
    }

    unique_ptr<AST> parseFor() {
        Token fTok = t;
        expect(TokenKind::KW_for, "'for'");
        Token nameTok = t;
        expect(TokenKind::Identifier, "loop variable");
        expect(TokenKind::KW_in, "'in'");
        auto F = nodeAt(NodeKind::For, nameTok, nameTok.lexeme);
        auto first = parseExpression();
        if (accept(TokenKind::Range)) {
            auto last = parseExpression();
            auto R = node(NodeKind::Expr, "..");
            R->add(std::move(first));
            R->add(std::move(last));
            F->add(std::move(R));
        } else {
            F->add(std::move(first));
        }
        if (accept(TokenKind::KW_reverse)) {
            F->add(node(NodeKind::Name, "reverse"));
        }
        expect(TokenKind::KW_loop, "'loop'");
        F->add(parseBody());
        expect(TokenKind::KW_end, "'end'");
        return F;
    }

    unique_ptr<AST> parseIf() {
        Token ifTok = t;
        expect(TokenKind::KW_if, "'if'");
        auto I = nodeAt(NodeKind::If, ifTok, "if");
        I->add(parseExpression());
        expect(TokenKind::KW_then, "'then'");
        I->add(parseBody());
        if (accept(TokenKind::KW_else)) {
            I->add(parseBody());
        }
        expect(TokenKind::KW_end, "'end'");
        return I;
    }

    unique_ptr<AST> parsePrint() {
        Token pTok = t;
        expect(TokenKind::KW_print, "'print'");
        auto P = nodeAt(NodeKind::Print, pTok, "print");
        P->add(parseExpression());
        while (accept(TokenKind::Comma))
            P->add(parseExpression());
        return P;
    }

    unique_ptr<AST> parseReturn() {
        Token rTok = t;
        expect(TokenKind::KW_return, "'return'");
        auto R = nodeAt(NodeKind::ReturnStmt, rTok, "return");
        if (!isStmtTerminator()) {
            R->add(parseExpression());
        }
        return R;
    }

    bool isStmtTerminator() {
        return t.kind == TokenKind::Separator || t.kind == TokenKind::KW_end || 
               t.kind == TokenKind::KW_else || t.kind == TokenKind::KW_loop || 
               t.kind == TokenKind::RParen || t.kind == TokenKind::RBracket || 
               t.kind == TokenKind::EndOfFile;
    }

    unique_ptr<AST> parseModifiablePrimaryOrCall(bool asStmt) {
        auto lhs = parseModifiablePrimaryCore();
        if (check(TokenKind::Assign)) {
            Token assignTok = t;
            advance();
            auto A = nodeAt(NodeKind::Assign, assignTok);
            A->add(std::move(lhs));
            A->add(parseExpression());
            return A;
        }
        if (lhs->kind == NodeKind::ModPrimary && lhs->kids.size() == 1 &&
            lhs->kids[0]->kind == NodeKind::Name && check(TokenKind::LParen)) {
            auto C = node(NodeKind::Call, lhs->kids[0]->text, lhs->kids[0]->line, lhs->kids[0]->col);
            advance();
            if (!check(TokenKind::RParen)) {
                C->add(parseExpression());
                while (accept(TokenKind::Comma))
                    C->add(parseExpression());
            }
            expect(TokenKind::RParen, "')' after arguments");
            return C;
        }
        if (asStmt) {
            throwErr("expected assignment ':=' or routine call");
        }
        return lhs;
    }

    unique_ptr<AST> parseModifiablePrimaryCore() {
        string name = t.lexeme;
        int l = t.line, c = t.col;
        expect(TokenKind::Identifier, "identifier");
        auto MP = node(NodeKind::ModPrimary);
        auto base = node(NodeKind::Name, name, l, c);
        MP->add(std::move(base));
        for (;;) {
            if (accept(TokenKind::Dot)) {
                string m = t.lexeme;
                int ml = t.line, mc = t.col;
                expect(TokenKind::Identifier, "member name");
                MP->add(node(NodeKind::Member, m, ml, mc));
            } else if (check(TokenKind::LBracket)) {
                Token lb = t;
                advance();
                auto idx = nodeAt(NodeKind::Index, lb);
                idx->add(parseExpression());
                expect(TokenKind::RBracket, "']'");
                MP->add(std::move(idx));
            } else if (accept(TokenKind::Apostrophe)) {
                string attr = t.lexeme;
                int al = t.line, ac = t.col;
                expect(TokenKind::Identifier, "attribute name");
                MP->add(node(NodeKind::Member, attr, al, ac));
            } else
                break;
        }
        return MP;
    }

    unique_ptr<AST> parseExpression() {
        auto left = parseRelation();
        while (check(TokenKind::KW_and) || check(TokenKind::KW_or) || check(TokenKind::KW_xor)) {
            string op = t.lexeme;
            int l = t.line, c = t.col;
            advance();
            auto E = node(NodeKind::Expr, op, l, c);
            E->add(std::move(left));
            E->add(parseRelation());
            left = std::move(E);
        }
        return left;
    }
    
    unique_ptr<AST> parseRelation() {
        auto left = parseSimple();
        if (check(TokenKind::LT) || check(TokenKind::LE) || check(TokenKind::GT) ||
            check(TokenKind::GE) || check(TokenKind::EQ) || check(TokenKind::NE)) {
            string op = t.lexeme;
            int l = t.line, c = t.col;
            advance();
            auto R = node(NodeKind::Rel, op, l, c);
            R->add(std::move(left));
            R->add(parseSimple());
            return R;
        }
        return left;
    }
    
    unique_ptr<AST> parseSimple() {
        auto left = parseFactor();
        while (check(TokenKind::Star) || check(TokenKind::Slash) || check(TokenKind::Percent)) {
            string op = t.lexeme;
            int l = t.line, c = t.col;
            advance();
            auto S = node(NodeKind::Simple, op, l, c);
            S->add(std::move(left));
            S->add(parseFactor());
            left = std::move(S);
        }
        return left;
    }
    
    unique_ptr<AST> parseFactor() {
        auto left = parseSummand();
        while (check(TokenKind::Plus) || check(TokenKind::Minus)) {
            string op = t.lexeme;
            int l = t.line, c = t.col;
            advance();
            auto F = node(NodeKind::Factor, op, l, c);
            F->add(std::move(left));
            F->add(parseSummand());
            left = std::move(F);
        }
        return left;
    }
    
    unique_ptr<AST> parseSummand() {
        if (accept(TokenKind::LParen)) {
            auto E = parseExpression();
            expect(TokenKind::RParen, "')'");
            return E;
        }
        if (check(TokenKind::KW_not)) {
            Token nt = t;
            advance();
            auto N = nodeAt(NodeKind::Primary, nt, "not");
            N->add(parseSummand());
            return N;
        }
        return parsePrimary();
    }

    unique_ptr<AST> parsePrimary() {
        bool havePrefix = false;
        string prefix;
        if (check(TokenKind::Plus) || check(TokenKind::Minus)) {
            havePrefix = true;
            prefix = t.lexeme;
            advance();
        }
        if (check(TokenKind::Integer)) {
            string v = t.lexeme;
            int l = t.line, c = t.col;
            advance();
            return node(NodeKind::Primary, (havePrefix ? prefix + v : v), l, c);
        }
        if (check(TokenKind::Real)) {
            string v = t.lexeme;
            int l = t.line, c = t.col;
            advance();
            return node(NodeKind::Primary, (havePrefix ? prefix + v : v), l, c);
        }
        if (check(TokenKind::Boolean)) {
            string v = t.lexeme;
            int l = t.line, c = t.col;
            advance();
            return node(NodeKind::Primary, v, l, c);
        }
        if (check(TokenKind::LParen)) {
            advance();
            auto E = parseExpression();
            expect(TokenKind::RParen, "')'");
            return E;
        }
        if (havePrefix)
            throwErr("prefix sign may be used only before numeric literal");

        if (check(TokenKind::Identifier)) {
            auto lhs = parseModifiablePrimaryCore();
            if (lhs->kind == NodeKind::ModPrimary && lhs->kids.size() == 1 &&
                lhs->kids[0]->kind == NodeKind::Name && check(TokenKind::LParen)) {
                auto C = node(NodeKind::Call, lhs->kids[0]->text, lhs->kids[0]->line, lhs->kids[0]->col);
                advance();
                if (!check(TokenKind::RParen)) {
                    C->add(parseExpression());
                    while (accept(TokenKind::Comma))
                        C->add(parseExpression());
                }
                expect(TokenKind::RParen, "')'");
                return C;
            }
            return lhs;
        }
        throwErr("expected primary");
        return nullptr;
    }

    unique_ptr<AST> node(NodeKind k, string text = "", int l = -1, int c = -1) {
        if (!buildAST) return std::make_unique<AST>(k);
        int ln = (l == -1 ? t.line : l);
        int cn = (c == -1 ? t.col : c);
        return std::make_unique<AST>(k, std::move(text), ln, cn);
    }
    
    unique_ptr<AST> nodeAt(NodeKind k, const Token &tok, string text = "") {
        return std::make_unique<AST>(k, std::move(text), tok.line, tok.col);
    }
};

int main(int argc, char** argv) {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    
    if (argc < 2) {
        cerr << "Usage: " << argv[0] << " <source.pi> [output.wat]\n";
        return 1;
    }
    
    ifstream in(argv[1], ios::binary);
    if (!in) {
        cerr << "Cannot open file: " << argv[1] << "\n";
        return 1;
    }
    
    string src((istreambuf_iterator<char>(in)), istreambuf_iterator<char>());
    
    try {
        // Parse
        Lexer lex(std::move(src));
        Parser p(lex, true);
        auto tree = p.parseProgram();
        
        if (!p.errors.empty()) {
            cerr << "\nSyntax errors:\n";
            for (auto& e: p.errors) cerr << "  " << e << "\n";
            return 2;
        }
        
        // Generate WASM
        WASMCompiler compiler;
        string watCode = compiler.compile(tree.get());
        
        // Output
        string outputFile = (argc >= 3) ? argv[2] : "output.wat";
        ofstream out(outputFile);
        if (!out) {
            cerr << "Cannot write to file: " << outputFile << "\n";
            return 1;
        }
        out << watCode;
        cout << "Generated WASM code written to " << outputFile << "\n";
        
        return 0;
        
    } catch (const ParseError& e) {
        cerr << "\n" << e.what() << "\n";
        return 2;
    } catch (const exception& e) {
        cerr << "\nError: " << e.what() << "\n";
        return 3;
    }
}

