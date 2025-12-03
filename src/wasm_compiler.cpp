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
                int commentStartLine = line, commentStartCol = col;
                advance(); advance();
                bool terminated = false;
                while (!eof())
                {
                    if (peek() == '*' && peek(1) == '/') { 
                        advance(); advance(); 
                        terminated = true;
                        break; 
                    }
                    advance();
                }
                if (!terminated) {
                    // Unterminated block comment - set error
                    pendingErrorToken = make(TokenKind::Error, "unterminated block comment", commentStartLine, commentStartCol);
                    hasPendingError = true;
                    return;
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


class WASMCompiler {
private:
    ostringstream wat;
    int labelCounter = 0;
    int localCounter = 0;
    int memoryOffset = 2048;
    
    struct VarInfo {
        string name;
        string type;
        int localIndex = -1;
        bool isGlobal = false;
        bool isArray = false;
        int arraySize = -1;
        string elemType;
        int blockLevel = 0;
    };
    
    unordered_map<string, VarInfo> globalVars;
    vector<unordered_map<string, vector<VarInfo>>> localScopes;
    unordered_map<string, int> routineParams;
    unordered_map<AST*, string> currentVarDeclToUniqueName;
    int currentBlockLevel = 0;
    
    struct TypeDef {
        NodeKind kind;
        string baseType;
        AST* typeNode;
    };
    unordered_map<string, TypeDef> typeTable;
    AST* rootAST = nullptr;
    
    // Helper to get parameter types from function declaration
    vector<string> getFunctionParamTypes(const string& funcName) {
        vector<string> paramTypes;
        if (!rootAST) return paramTypes;
        
        for (auto& child : rootAST->kids) {
            if (child->kind == NodeKind::RoutineDecl && child->kids.size() > 0) {
                AST* header = child->kids[0].get();
                if (header->kind == NodeKind::RoutineHeader && header->text == funcName) {
                    if (header->kids.size() > 0 && header->kids[0]->kind == NodeKind::Params) {
                        AST* params = header->kids[0].get();
                        for (auto& param : params->kids) {
                            if (param->kind == NodeKind::Param && param->kids.size() > 0) {
                                AST* typeNode = param->kids[0].get();
                                if (typeNode->kind == NodeKind::PrimType) {
                                    paramTypes.push_back(typeNode->text);
                                } else {
                                    paramTypes.push_back("integer"); // default
                                }
                            }
                        }
                    }
                    break;
                }
            }
        }
        return paramTypes;
    }
    
    // Helper to check if function has return type
    bool hasFunctionReturnType(const string& funcName) {
        if (!rootAST) return false;
        
        for (auto& child : rootAST->kids) {
            if (child->kind == NodeKind::RoutineDecl && child->kids.size() > 0) {
                AST* header = child->kids[0].get();
                if (header->kind == NodeKind::RoutineHeader && header->text == funcName) {
                    // Check if there's a return type (it's after Params, before body)
                    for (size_t i = 0; i < header->kids.size(); i++) {
                        AST* kid = header->kids[i].get();
                        if (kid->kind == NodeKind::PrimType || kid->kind == NodeKind::UserType) {
                            return true;
                        }
                        if (kid->kind == NodeKind::RoutineBodyBlock || kid->kind == NodeKind::RoutineBodyExpr) {
                            break;
                        }
                    }
                    break;
                }
            }
        }
        return false;
    }
    
    string getWASMType(const string& piType) {
        if (piType == "integer") return "i32";
        if (piType == "real") return "f64";
        if (piType == "boolean") return "i32";
        return "i32";
    }
    
    string getPITypeFromVarInfo(VarInfo* var, AST* varDeclNode = nullptr) {
        if (!var) return "integer";
        
        if (varDeclNode && varDeclNode->kind == NodeKind::VarDecl && varDeclNode->kids.size() > 0) {
            AST* typeNode = varDeclNode->kids[0].get();
            if (typeNode->kind == NodeKind::PrimType) {
                return typeNode->text;
            } else if (typeNode->kind == NodeKind::UserType) {
                if (typeTable.count(typeNode->text)) {
                    TypeDef& td = typeTable[typeNode->text];
                    if (td.kind == NodeKind::PrimType && td.typeNode) {
                        return td.typeNode->text;
                    }
                }
            }
        }
        
        if (var->type == "f64") return "real";
        if (var->type == "i32") return "integer";
        return "integer";
    }
    
    AST* findVarDeclInAST(const string& varName, AST* root) {
        if (!root) return nullptr;
        if (root->kind == NodeKind::VarDecl && root->text == varName) {
            return root;
        }
        for (auto& child : root->kids) {
            AST* result = findVarDeclInAST(varName, child.get());
            if (result) return result;
        }
        return nullptr;
    }
    
    // Get size of any type (primitive, record, or array)
    int getTypeSize(AST* typeNode) {
        if (!typeNode) return 4;
        
        if (typeNode->kind == NodeKind::PrimType) {
            string piType = typeNode->text;
            if (piType == "integer" || piType == "boolean") return 4;
            if (piType == "real") return 8;
            return 4;
        }
        
        if (typeNode->kind == NodeKind::UserType) {
            string typeName = typeNode->text;
            if (typeTable.count(typeName)) {
                TypeDef& td = typeTable[typeName];
                return getTypeSize(td.typeNode);
            }
            // Look up in AST
            if (rootAST) {
                for (auto& child : rootAST->kids) {
                    if (child->kind == NodeKind::TypeDecl && child->text == typeName && child->kids.size() > 0) {
                        return getTypeSize(child->kids[0].get());
                    }
                }
            }
            return 4;
        }
        
        if (typeNode->kind == NodeKind::RecordType) {
            int size = 0;
            for (auto& child : typeNode->kids) {
                if (child->kind == NodeKind::VarDecl && child->kids.size() > 0) {
                    size += getTypeSize(child->kids[0].get());
                }
            }
            return size > 0 ? size : 4;
        }
        
        if (typeNode->kind == NodeKind::ArrayType) {
            // Arrays store size + elements, but we just return pointer size for local vars
            // The actual allocation handles the full size
            int arraySize = 0;
            int elemSize = 4;
            if (typeNode->kids.size() > 0) {
                // First child might be size
                if (typeNode->kids[0]->kind == NodeKind::Primary) {
                    arraySize = stoi(typeNode->kids[0]->text);
                }
                // Last child is element type
                if (typeNode->kids.size() > 1) {
                    elemSize = getTypeSize(typeNode->kids.back().get());
                } else if (typeNode->kids.size() == 1 && typeNode->kids[0]->kind != NodeKind::Primary) {
                    elemSize = getTypeSize(typeNode->kids[0].get());
                }
            }
            return 4 + arraySize * elemSize; // size field + elements
        }
        
        return 4;
    }
    
    // Get size of a named type
    int getTypeSizeByName(const string& typeName) {
        if (typeName == "integer" || typeName == "boolean") return 4;
        if (typeName == "real") return 8;
        
        if (typeTable.count(typeName)) {
            TypeDef& td = typeTable[typeName];
            return getTypeSize(td.typeNode);
        }
        
        if (rootAST) {
            for (auto& child : rootAST->kids) {
                if (child->kind == NodeKind::TypeDecl && child->text == typeName && child->kids.size() > 0) {
                    return getTypeSize(child->kids[0].get());
                }
            }
        }
        return 4;
    }
    
    // Get the field type name for a record field
    string getRecordFieldTypeName(const string& recordTypeName, const string& fieldName) {
        if (rootAST) {
            for (auto& child : rootAST->kids) {
                if (child->kind == NodeKind::TypeDecl && child->text == recordTypeName && child->kids.size() > 0) {
                    AST* typeNode = child->kids[0].get();
                    if (typeNode && typeNode->kind == NodeKind::RecordType) {
                        for (auto& field : typeNode->kids) {
                            if (field->kind == NodeKind::VarDecl && field->text == fieldName && field->kids.size() > 0) {
                                AST* fieldTypeNode = field->kids[0].get();
                                if (fieldTypeNode->kind == NodeKind::PrimType) {
                                    return fieldTypeNode->text;
                                } else if (fieldTypeNode->kind == NodeKind::UserType) {
                                    return fieldTypeNode->text;
                                }
                            }
                        }
                    }
                }
            }
        }
        return "unknown";
    }

    int getRecordFieldOffset(const string& recordTypeName, const string& fieldName, AST* root) {
        if (!root) return -1;
        
        if (root->kind == NodeKind::TypeDecl && root->text == recordTypeName && root->kids.size() > 0) {
            AST* typeNode = root->kids[0].get();
            if (typeNode && typeNode->kind == NodeKind::RecordType) {
                int offset = 0;
                for (auto& child : typeNode->kids) {
                    if (child->kind == NodeKind::VarDecl) {
                        if (child->text == fieldName) {
                            return offset;
                        }
                        if (child->kids.size() > 0) {
                            offset += getTypeSize(child->kids[0].get());
                        }
                    }
                }
            }
        }
        
        for (auto& child : root->kids) {
            int result = getRecordFieldOffset(recordTypeName, fieldName, child.get());
            if (result >= 0) return result;
        }
        return -1;
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
    
    void buildTypeTable(AST* root) {
        if (!root) return;
        if (root->kind == NodeKind::TypeDecl && !root->kids.empty()) {
            TypeDef td;
            td.typeNode = root->kids[0].get();
            td.kind = td.typeNode->kind;
            typeTable[root->text] = td;
        }
        for (auto& child : root->kids) {
            buildTypeTable(child.get());
        }
    }
    
    string resolveUserType(const string& typeName) {
        if (typeTable.count(typeName)) {
            TypeDef& td = typeTable[typeName];
            if (td.kind == NodeKind::PrimType) {
                return getWASMType(td.typeNode->text);
            } else if (td.kind == NodeKind::UserType) {
                return resolveUserType(td.typeNode->text);
            } else if (td.kind == NodeKind::ArrayType) {
                return "i32";
            } else if (td.kind == NodeKind::RecordType) {
                return "i32";
            }
        }
        return "i32";
    }
    
public:
    string compile(AST* root) {
        wat.str("");
        wat.clear();
        rootAST = root;
        memoryOffset = 2048;
        
        emit("(module");
        emit("  (import \"wasi_snapshot_preview1\" \"fd_write\"");
        emit("    (func $fd_write (param i32 i32 i32 i32) (result i32)))");
        emit("  (memory 1)");
        emit("  (export \"memory\" (memory 0))");
        emit("");
        
        buildTypeTable(root);
        // Helper function to print i32 (converts number to string)

        emit("  (func $print_i32 (param $value i32)");
        emit("    (local $n i32) (local $neg i32) (local $len i32)");
        emit("    (local $buf_in i32) (local $buf_out i32) (local $d i32)");
        emit("    (local $i i32)");
        emit("    (local.set $buf_in  (i32.const 1024))");
        emit("    (local.set $buf_out (i32.const 1536))");
        emit("    (local.set $n   (local.get $value))");
        emit("    (local.set $neg (i32.const 0))");
        emit("    (local.set $len (i32.const 0))");
        emit("    (if (i32.lt_s (local.get $n) (i32.const 0))");
        emit("      (then");
        emit("        (local.set $neg (i32.const 1))");
        emit("        (local.set $n (i32.sub (i32.const 0) (local.get $n)))");
        emit("      )");
        emit("    )");
        emit("    (if (i32.eqz (local.get $n))");
        emit("      (then");
        emit("        (i32.store8 (i32.add (local.get $buf_in) (local.get $len)) (i32.const 48))");
        emit("        (local.set $len (i32.const 1))");
        emit("      )");
        emit("      (else");
        emit("        (loop $L");
        emit("          (local.set $d (i32.rem_u (local.get $n) (i32.const 10)))");
        emit("          (i32.store8");
        emit("            (i32.add (local.get $buf_in) (local.get $len))");
        emit("            (i32.add (i32.const 48) (local.get $d)))");
        emit("          (local.set $len (i32.add (local.get $len) (i32.const 1)))");
        emit("          (local.set $n (i32.div_u (local.get $n) (i32.const 10)))");
        emit("          (br_if $L (i32.gt_u (local.get $n) (i32.const 0)))");
        emit("        )");
        emit("      )");
        emit("    )");
        emit("    (local.set $i (i32.const 0))");
        emit("    (loop $R");
        emit("      (i32.store8");
        emit("        (i32.add (local.get $buf_out) (local.get $i))");
        emit("        (i32.load8_u");
        emit("          (i32.add (local.get $buf_in)");
        emit("                   (i32.sub (local.get $len)");
        emit("                            (i32.add (local.get $i) (i32.const 1))))))");
        emit("      (local.set $i (i32.add (local.get $i) (i32.const 1)))");
        emit("      (br_if $R (i32.lt_u (local.get $i) (local.get $len)))");
        emit("    )");
        emit("    (if (local.get $neg)");
        emit("      (then");
        emit("        (local.set $i (local.get $len))");
        emit("        (loop $S");
        emit("          (i32.store8");
        emit("            (i32.add (local.get $buf_out) (i32.add (local.get $i) (i32.const 1)))");
        emit("            (i32.load8_u (i32.add (local.get $buf_out) (local.get $i))))");
        emit("          (local.set $i (i32.sub (local.get $i) (i32.const 1)))");
        emit("          (br_if $S (i32.gt_s (local.get $i) (i32.const -1)))");
        emit("        )");
        emit("        (i32.store8 (local.get $buf_out) (i32.const 45))");
        emit("        (local.set $len (i32.add (local.get $len) (i32.const 1)))");
        emit("      )");
        emit("    )");
        emit("    (i32.store8 (i32.add (local.get $buf_out) (local.get $len)) (i32.const 10))");
        emit("    (local.set $len (i32.add (local.get $len) (i32.const 1)))");
        emit("    (i32.store (i32.const 0) (local.get $buf_out))");
        emit("    (i32.store (i32.const 4) (local.get $len))");
        emit("    (call $fd_write (i32.const 1) (i32.const 0) (i32.const 1) (i32.const 8))");
        emit("    drop");
        emit("  )");

        // Process global declarations and routines
        localScopes.push_back({});
        
        for (auto& child : root->kids) {
            if (child->kind == NodeKind::VarDecl) {
                compileVarDecl(child.get(), 2);
            } else if (child->kind == NodeKind::RoutineDecl) {
                compileRoutine(child.get(), 2);
            }
        }
        
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
                VarInfo* var = nullptr;
                for (int i = localScopes.size() - 1; i >= 0; i--) {
                    if (localScopes[i].count(varName)) {
                        vector<VarInfo>& candidates = localScopes[i][varName];
                        for (auto& candidate : candidates) {
                            if (candidate.blockLevel <= currentBlockLevel) {
                                if (!var || candidate.blockLevel > var->blockLevel) {
                                    var = &candidate;
                                }
                            }
                        }
                        if (var) break;
                    }
                }
                if (!var && globalVars.count(varName)) {
                    var = &globalVars[varName];
                }
                if (var) {
                    if (var->isGlobal) {
                        emitIndent(indent, "global.get $" + varName);
                    } else {
                        emitIndent(indent, "local.get $" + var->name);
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
                        emitIndent(indent, opType == "f64" ? "f64.div" : "i32.div_s");
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
                    emitIndent(indent, opType == "f64" ? "f64.lt" : "i32.lt_s");
                } else if (expr->text == "<=") {
                    emitIndent(indent, opType == "f64" ? "f64.le" : "i32.le_s");
                } else if (expr->text == ">") {
                    emitIndent(indent, opType == "f64" ? "f64.gt" : "i32.gt_s");
                } else if (expr->text == ">=") {
                    emitIndent(indent, opType == "f64" ? "f64.ge" : "i32.ge_s");
                } else if (expr->text == "=") {
                    emitIndent(indent, opType + ".eq");
                } else if (expr->text == "/=") {
                    emitIndent(indent, opType + ".ne");
                }
                return "i32";
            }
            case NodeKind::Call: {
                string funcName = expr->text;
                vector<string> paramTypes = getFunctionParamTypes(funcName);
                
                for (size_t i = 0; i < expr->kids.size(); i++) {
                    string argType = compileExpression(expr->kids[i].get(), indent);
                    string expectedType = (i < paramTypes.size()) ? paramTypes[i] : "integer";
                    
                    // Convert argument type if needed (same rules as assignment)
                    if (expectedType == "integer" && argType == "f64") {
                        // real -> integer (rounding)
                        emitIndent(indent, "f64.nearest");
                        emitIndent(indent, "i32.trunc_f64_s");
                    } else if (expectedType == "integer" && argType == "i32") {
                        // Check if source is real (we need to infer from expression)
                        // For now, assume it's already integer
                    } else if (expectedType == "real" && argType == "i32") {
                        // integer -> real
                        emitIndent(indent, "f64.convert_i32_s");
                    } else if (expectedType == "boolean" && argType == "i32") {
                        // integer -> boolean (runtime check will happen in assignment)
                        // No conversion needed at call site, it's handled in assignment
                    }
                }
                emitIndent(indent, "call $" + funcName);
                return "i32";
            }
            case NodeKind::ModPrimary: {
                if (expr->kids.size() > 0 && expr->kids[0]->kind == NodeKind::Name) {
                    string varName = expr->kids[0]->text;
                    VarInfo* var = nullptr;
                    AST* varDeclNode = nullptr;
                    for (int i = localScopes.size() - 1; i >= 0; i--) {
                        if (localScopes[i].count(varName)) {
                            vector<VarInfo>& candidates = localScopes[i][varName];
                            // Find the variable with highest blockLevel <= currentBlockLevel
                            for (auto& candidate : candidates) {
                                if (candidate.blockLevel <= currentBlockLevel) {
                                    if (!var || candidate.blockLevel > var->blockLevel) {
                                        var = &candidate;
                                    }
                                }
                            }
                        }
                    }
                    if (!var && globalVars.count(varName)) {
                        var = &globalVars[varName];
                    }
                    
                    if (var && rootAST) {
                        varDeclNode = findVarDeclInAST(varName, rootAST);
                    }
                    
                    if (var) {
                        if (var->isGlobal) {
                            emitIndent(indent, "global.get $" + varName);
                        } else {
                            emitIndent(indent, "local.get $" + var->name);
                        }
                        
                        // Track current type for nested access
                        string currentTypeName = "unknown";
                        if (varDeclNode && varDeclNode->kids.size() > 0) {
                            AST* typeNode = varDeclNode->kids[0].get();
                            if (typeNode->kind == NodeKind::UserType) {
                                currentTypeName = typeNode->text;
                            } else if (typeNode->kind == NodeKind::RecordType) {
                                currentTypeName = "record";
                            } else if (typeNode->kind == NodeKind::ArrayType) {
                                currentTypeName = "array";
                            }
                        }
                        
                        // For variables that are arrays, set up type info
                        if (var && var->isArray && varDeclNode) {
                            AST* typeNode = varDeclNode->kids[0].get();
                            if (typeNode && typeNode->kind == NodeKind::UserType) {
                                currentTypeName = typeNode->text;
                            }
                        }
                        
                        for (size_t i = 1; i < expr->kids.size(); i++) {
                            if (expr->kids[i]->kind == NodeKind::Index) {
                                // For fixed-size arrays: base+4 is first element
                                // Stack: [base_address]
                                
                                // Get element size based on current type
                                int elemSize = 4;
                                string elemTypeName = "integer";
                                if (currentTypeName != "unknown" && typeTable.count(currentTypeName)) {
                                    TypeDef& td = typeTable[currentTypeName];
                                    if (td.kind == NodeKind::ArrayType && td.typeNode) {
                                        // Get element type from array type
                                        AST* arrType = td.typeNode;
                                        // Last child should be element type
                                        if (arrType->kids.size() > 0) {
                                            AST* lastChild = arrType->kids.back().get();
                                            if (lastChild->kind == NodeKind::PrimType) {
                                                elemTypeName = lastChild->text;
                                                if (lastChild->text == "real") elemSize = 8;
                                            } else if (lastChild->kind == NodeKind::UserType) {
                                                elemTypeName = lastChild->text;
                                                elemSize = getTypeSizeByName(lastChild->text);
                                            }
                                        }
                                    }
                                }
                                
                                compileExpression(expr->kids[i]->kids[0].get(), indent);
                                emitIndent(indent, "i32.const 1");
                                emitIndent(indent, "i32.sub");  // Convert 1-based to 0-based
                                emitIndent(indent, "i32.const " + to_string(elemSize));
                                emitIndent(indent, "i32.mul");  // offset = index * elemSize
                                emitIndent(indent, "i32.const 4");
                                emitIndent(indent, "i32.add");  // base + 4 (skip size field)
                                emitIndent(indent, "i32.add");  // base + 4 + offset
                                
                                // Check if there are more accessors after this
                                bool isLastAccessor = (i == expr->kids.size() - 1);
                                
                                // Check if element type is primitive (need to load) or complex (don't load yet)
                                bool elemIsPrimitive = (elemTypeName == "integer" || elemTypeName == "real" || elemTypeName == "boolean");
                                
                                if (isLastAccessor && elemIsPrimitive) {
                                    emitIndent(indent, "i32.load");
                                }
                                // If not last or element is complex, don't load - just have address on stack
                                
                                // Update current type for next iteration
                                currentTypeName = elemTypeName;
                            } else if (expr->kids[i]->kind == NodeKind::Member) {
                                string fieldName = expr->kids[i]->text;
                                
                                // Get field offset using current type
                                int fieldOffset = -1;
                                if (rootAST && currentTypeName != "unknown" && currentTypeName != "record") {
                                    fieldOffset = getRecordFieldOffset(currentTypeName, fieldName, rootAST);
                                }
                                
                                // Check if this is the last accessor (we need to load) or not (just offset)
                                bool isLastAccessor = (i == expr->kids.size() - 1);
                                
                                if (fieldOffset >= 0) {
                                    emitIndent(indent, "i32.const " + to_string(fieldOffset));
                                    emitIndent(indent, "i32.add");
                                    if (isLastAccessor) {
                                        emitIndent(indent, "i32.load");
                                    }
                                    // Don't load if not the last - just compute the address
                                    // Update current type for the next iteration
                                    currentTypeName = getRecordFieldTypeName(currentTypeName, fieldName);
                                } else {
                                    // Fallback for unknown offset
                                    if (isLastAccessor) {
                                        emitIndent(indent, "i32.load");
                                    }
                                }
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
                string rhsType = compileExpression(rhs, indent);
                
                if (lhs->kind == NodeKind::ModPrimary && lhs->kids.size() > 0) {
                    AST* nameNode = lhs->kids[0].get();
                    if (nameNode->kind == NodeKind::Name) {
                        string varName = nameNode->text;
                        VarInfo* var = nullptr;
                        AST* varDeclNode = nullptr;
                        for (int i = localScopes.size() - 1; i >= 0; i--) {
                            if (localScopes[i].count(varName)) {
                                vector<VarInfo>& candidates = localScopes[i][varName];
                                for (auto& candidate : candidates) {
                                    if (candidate.blockLevel <= currentBlockLevel) {
                                        if (!var || candidate.blockLevel > var->blockLevel) {
                                            var = &candidate;
                                        }
                                    }
                                }
                                if (var) break;
                            }
                        }
                        if (!var && globalVars.count(varName)) {
                            var = &globalVars[varName];
                        }
                        
                        if (var && rootAST) {
                            varDeclNode = findVarDeclInAST(varName, rootAST);
                        }
                        
                        if (var) {
                            string lhsPIType = getPITypeFromVarInfo(var, varDeclNode);
                            string rhsPIType = (rhsType == "f64") ? "real" : 
                                              (rhsType == "i32") ? "integer" : "integer";
                            
                            if (lhsPIType == "integer" && rhsPIType == "real") {
                                emitIndent(indent, "f64.nearest");
                                emitIndent(indent, "i32.trunc_f64_s");
                            } else if (lhsPIType == "boolean" && rhsPIType == "integer") {
                                emitIndent(indent, "local.tee $__temp_check");
                                emitIndent(indent, "i32.const 1");
                                emitIndent(indent, "i32.gt_u");
                                emitIndent(indent, "(if (then unreachable))");
                                emitIndent(indent, "local.get $__temp_check");
                            } else if (lhsPIType == "real" && rhsPIType == "integer") {
                                emitIndent(indent, "f64.convert_i32_s");
                            } else if (lhsPIType == "real" && rhsPIType == "boolean") {
                                emitIndent(indent, "f64.convert_i32_s");
                            } else if (lhsPIType == "integer" && rhsPIType == "boolean") {
                            }
                            
                            if (lhs->kids.size() > 1 && (lhs->kids[1]->kind == NodeKind::Index || lhs->kids[1]->kind == NodeKind::Member)) {
                                // Nested access assignment (array index or member access, possibly multiple levels)
                                // First, save the RHS value
                                emitIndent(indent, "local.tee $__temp_check");
                                
                                // Get base address
                                compileExpression(nameNode, indent);
                                
                                // Track current type for nested access
                                string currentTypeName = "unknown";
                                if (varDeclNode && varDeclNode->kids.size() > 0) {
                                    AST* typeNode = varDeclNode->kids[0].get();
                                    if (typeNode->kind == NodeKind::UserType) {
                                        currentTypeName = typeNode->text;
                                    } else if (typeNode->kind == NodeKind::RecordType) {
                                        currentTypeName = "record";
                                    } else if (typeNode->kind == NodeKind::ArrayType) {
                                        currentTypeName = "array";
                                    }
                                }
                                
                                // For variables that are arrays, set up type info
                                if (var && var->isArray && varDeclNode) {
                                    AST* typeNode = varDeclNode->kids[0].get();
                                    if (typeNode && typeNode->kind == NodeKind::UserType) {
                                        currentTypeName = typeNode->text;
                                    }
                                }
                                
                                // Process all member/index accesses
                                for (size_t mi = 1; mi < lhs->kids.size(); mi++) {
                                    if (lhs->kids[mi]->kind == NodeKind::Member) {
                                        string fieldName = lhs->kids[mi]->text;
                                        int fieldOffset = -1;
                                        
                                        if (rootAST && currentTypeName != "unknown" && currentTypeName != "record") {
                                            fieldOffset = getRecordFieldOffset(currentTypeName, fieldName, rootAST);
                                        }
                                        
                                        if (fieldOffset >= 0) {
                                            emitIndent(indent, "i32.const " + to_string(fieldOffset));
                                            emitIndent(indent, "i32.add");
                                        }
                                        
                                        // Update current type for next iteration
                                        currentTypeName = getRecordFieldTypeName(currentTypeName, fieldName);
                                    } else if (lhs->kids[mi]->kind == NodeKind::Index) {
                                        // Array index within nested access
                                        // Get element size based on current type
                                        int elemSize = 4;
                                        string elemTypeName = "integer";
                                        if (currentTypeName != "unknown" && typeTable.count(currentTypeName)) {
                                            TypeDef& td = typeTable[currentTypeName];
                                            if (td.kind == NodeKind::ArrayType && td.typeNode) {
                                                AST* arrType = td.typeNode;
                                                if (arrType->kids.size() > 0) {
                                                    AST* lastChild = arrType->kids.back().get();
                                                    if (lastChild->kind == NodeKind::PrimType) {
                                                        elemTypeName = lastChild->text;
                                                        if (lastChild->text == "real") elemSize = 8;
                                                    } else if (lastChild->kind == NodeKind::UserType) {
                                                        elemTypeName = lastChild->text;
                                                        elemSize = getTypeSizeByName(lastChild->text);
                                                    }
                                                }
                                            }
                                        }
                                        
                                        compileExpression(lhs->kids[mi]->kids[0].get(), indent);
                                        emitIndent(indent, "i32.const 1");
                                        emitIndent(indent, "i32.sub");
                                        emitIndent(indent, "i32.const " + to_string(elemSize));
                                        emitIndent(indent, "i32.mul");
                                        emitIndent(indent, "i32.const 4");
                                        emitIndent(indent, "i32.add");
                                        emitIndent(indent, "i32.add");
                                        
                                        // Update current type for next iteration
                                        currentTypeName = elemTypeName;
                                    }
                                }
                                
                                // Store the value
                                emitIndent(indent, "local.get $__temp_check");
                                emitIndent(indent, "i32.store");
                            } else {
                                if (var->isGlobal) {
                                    emitIndent(indent, "global.set $" + varName);
                                } else {
                                    emitIndent(indent, "local.set $" + var->name);
                                }
                            }
                        }
                    }
                }
                break;
            }
            case NodeKind::Print: {
                for (auto& expr : stmt->kids) {
                    string exprType = compileExpression(expr.get(), indent);
                    if (exprType == "f64") {
                        emitIndent(indent, "call $print_f64");
                    } else {
                        // i32 or boolean (both are i32 in WASM)
                    emitIndent(indent, "call $print_i32");
                    }
                }
                break;
            }
            case NodeKind::If: {
                compileExpression(stmt->kids[0].get(), indent);
                emitIndent(indent, "(if");
                
                if (stmt->kids.size() > 1 && stmt->kids[1]->kind == NodeKind::Body) {
                    emitIndent(indent + 1, "(then");
                    localScopes.push_back({});
                    currentBlockLevel++;
                    for (auto& child : stmt->kids[1]->kids) {
                        compileStatement(child.get(), indent + 2);
                    }
                    currentBlockLevel--;
                    localScopes.pop_back();
                    emitIndent(indent + 1, ")");
                }
                
                if (stmt->kids.size() > 2 && stmt->kids[2]->kind == NodeKind::Body) {
                    emitIndent(indent + 1, "(else");
                    localScopes.push_back({});
                    currentBlockLevel++;
                    for (auto& child : stmt->kids[2]->kids) {
                        compileStatement(child.get(), indent + 2);
                    }
                    currentBlockLevel--;
                    localScopes.pop_back();
                    emitIndent(indent + 1, ")");
                }
                
                emitIndent(indent, ")");
                break;
            }
            case NodeKind::While: {
                emitIndent(indent, "(block $end");
                emitIndent(indent + 1, "(loop $loop");
                
                compileExpression(stmt->kids[0].get(), indent + 2);
                emitIndent(indent + 2, "i32.eqz");
                emitIndent(indent + 2, "br_if $end");
                
                // Body
                if (stmt->kids.size() > 1 && stmt->kids[1]->kind == NodeKind::Body) {
                    for (auto& child : stmt->kids[1]->kids) {
                        compileStatement(child.get(), indent + 2);
                    }
                }
                
                emitIndent(indent + 2, "br $loop");
                emitIndent(indent + 1, ")");
                emitIndent(indent, ")");
                break;
            }
            case NodeKind::For: {
                if (stmt->kids.size() < 2) break;
                
                string loopVarName = stmt->text;
                bool isReverse = false;
                
                for (size_t i = 1; i < stmt->kids.size(); i++) {
                    if (stmt->kids[i]->kind == NodeKind::Name && stmt->kids[i]->text == "reverse") {
                        isReverse = true;
                        break;
                    }
                }
                
                AST* rangeNode = stmt->kids[0].get();
                
                if (rangeNode->kind == NodeKind::Expr && rangeNode->text == "..") {
                    // Range with two expressions - structural loop
                    if (rangeNode->kids.size() >= 2) {
                        string startLocal = "$start_" + loopVarName;
                        string endLocal = "$end_" + loopVarName;
                        
                        compileExpression(rangeNode->kids[0].get(), indent);
                        emitIndent(indent, "local.set " + startLocal);
                        
                            compileExpression(rangeNode->kids[1].get(), indent);
                        emitIndent(indent, "local.set " + endLocal);
                        
                        if (isReverse) {
                            emitIndent(indent, "local.get " + endLocal);
                        } else {
                            emitIndent(indent, "local.get " + startLocal);
                        }
                        emitIndent(indent, "local.set $" + loopVarName);
                        
                        emitIndent(indent, "(block $end");
                        emitIndent(indent + 1, "(loop $loop");
                        
                        emitIndent(indent + 2, "local.get $" + loopVarName);
                        if (isReverse) {
                            emitIndent(indent + 2, "local.get " + startLocal);
                            emitIndent(indent + 2, "i32.lt_s");
                        } else {
                            emitIndent(indent + 2, "local.get " + endLocal);
                            emitIndent(indent + 2, "i32.gt_s");
                        }
                        emitIndent(indent + 2, "br_if $end");
                        
                        AST* bodyNode = nullptr;
                        for (size_t i = 1; i < stmt->kids.size(); i++) {
                            if (stmt->kids[i]->kind == NodeKind::Body) {
                                bodyNode = stmt->kids[i].get();
                                break;
                            }
                        }
                        if (bodyNode) {
                            for (auto& child : bodyNode->kids) {
                                compileStatement(child.get(), indent + 2);
                            }
                        }
                        
                        emitIndent(indent + 2, "local.get $" + loopVarName);
                        if (isReverse) {
                            emitIndent(indent + 2, "i32.const 1");
                            emitIndent(indent + 2, "i32.sub");
                        } else {
                            emitIndent(indent + 2, "i32.const 1");
                            emitIndent(indent + 2, "i32.add");
                        }
                        emitIndent(indent + 2, "local.set $" + loopVarName);
                        emitIndent(indent + 2, "br $loop");
                        emitIndent(indent + 1, ")");
                        emitIndent(indent, ")");
                    }
                } else {
                    string arrBase = "$arr_base_" + loopVarName;
                    string arrSize = "$arr_size_" + loopVarName;
                    string idx = "$idx_" + loopVarName;
                    
                    compileExpression(rangeNode, indent);
                    emitIndent(indent, "local.set " + arrBase);
                    
                    // For fixed-size arrays, base points to size field
                    // For dynamic arrays, size is stored at base-4
                    // Try to read from base first (fixed-size), if that fails, use base-4
                    emitIndent(indent, "local.get " + arrBase);
                    emitIndent(indent, "i32.load");  // Read size from base (fixed-size arrays)
                    emitIndent(indent, "local.set " + arrSize);
                    
                    emitIndent(indent, "i32.const 0");
                    emitIndent(indent, "local.set " + idx);
                    
                    emitIndent(indent, "(block $end");
                    emitIndent(indent + 1, "(loop $loop");
                    
                    emitIndent(indent + 2, "local.get " + idx);
                    emitIndent(indent + 2, "local.get " + arrSize);
                    emitIndent(indent + 2, "i32.ge_u");
                    emitIndent(indent + 2, "br_if $end");
                    
                    emitIndent(indent + 2, "local.get " + arrBase);
                    emitIndent(indent + 2, "i32.const 4");
                    emitIndent(indent + 2, "i32.add");  // base + 4 (skip size field)
                    emitIndent(indent + 2, "local.get " + idx);
                    emitIndent(indent + 2, "i32.const 4");
                    emitIndent(indent + 2, "i32.mul");  // offset = idx * 4
                    emitIndent(indent + 2, "i32.add");  // base + 4 + offset
                    emitIndent(indent + 2, "i32.load");
                    emitIndent(indent + 2, "local.set $" + loopVarName);
                    
                    AST* bodyNode = nullptr;
                    for (size_t i = 1; i < stmt->kids.size(); i++) {
                        if (stmt->kids[i]->kind == NodeKind::Body) {
                            bodyNode = stmt->kids[i].get();
                            break;
                        }
                    }
                    if (bodyNode) {
                        for (auto& child : bodyNode->kids) {
                            compileStatement(child.get(), indent + 2);
                        }
                    }
                    
                    emitIndent(indent + 2, "local.get " + idx);
                    emitIndent(indent + 2, "i32.const 1");
                    emitIndent(indent + 2, "i32.add");
                    emitIndent(indent + 2, "local.set " + idx);
                    emitIndent(indent + 2, "br $loop");
                    emitIndent(indent + 1, ")");
                    emitIndent(indent, ")");
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
                string funcName = stmt->text;
                compileExpression(stmt, indent);
                // Only drop if function returns a value
                if (hasFunctionReturnType(funcName)) {
                    emitIndent(indent, "drop");
                }
                break;
            }
            case NodeKind::Body: {
                // Nested Body (e.g., from if simplification) - increment block level
                currentBlockLevel++;
                for (auto& child : stmt->kids) {
                    compileStatement(child.get(), indent);
                }
                currentBlockLevel--;
                break;
            }
            case NodeKind::VarDecl: {
                string varName = stmt->text;
                AST* varDeclNode = stmt;
                string actualVarName = currentVarDeclToUniqueName.count(varDeclNode) ? currentVarDeclToUniqueName[varDeclNode] : varName;
                VarInfo* var = nullptr;
                for (int i = localScopes.size() - 1; i >= 0; i--) {
                    if (localScopes[i].count(varName)) {
                        vector<VarInfo>& candidates = localScopes[i][varName];
                        // Find the variable matching the actualVarName
                        for (auto& candidate : candidates) {
                            if (candidate.name == actualVarName) {
                                var = &candidate;
                                break;
                            }
                        }
                        if (var) break;
                    }
                }
                if (var && stmt->kids.size() > 1) {
                    compileExpression(stmt->kids[1].get(), indent);
                    emitIndent(indent, "local.set $" + actualVarName);
                }
                break;
            }
            default:
                break;
        }
    }
    
    VarInfo collectVarInfo(AST* varDecl) {
        VarInfo var;
        if (varDecl->kind != NodeKind::VarDecl) return var;
        
        string varName = varDecl->text;
        var.name = varName;
        
        if (varDecl->kids.size() > 0) {
            AST* typeNode = varDecl->kids[0].get();
            if (typeNode->kind == NodeKind::PrimType) {
                var.type = getWASMType(typeNode->text);
            } else if (typeNode->kind == NodeKind::ArrayType) {
                var.isArray = true;
                if (typeNode->kids.size() >= 1) {
                    AST* sizeNode = typeNode->kids[0].get();
                    if (sizeNode && sizeNode->kind == NodeKind::Primary) {
                        try {
                            var.arraySize = stoi(sizeNode->text);
                        } catch (...) {
                            var.arraySize = -1;
                        }
                    }
                }
                if (typeNode->kids.size() > 1) {
                    AST* elemType = typeNode->kids[1].get();
                    if (elemType->kind == NodeKind::PrimType) {
                        var.elemType = getWASMType(elemType->text);
                    }
                }
                var.type = "i32";
            } else if (typeNode->kind == NodeKind::UserType) {
                string typeName = typeNode->text;
                // Check if user type is an array type
                if (typeTable.count(typeName)) {
                    TypeDef& td = typeTable[typeName];
                    if (td.kind == NodeKind::ArrayType) {
                        var.isArray = true;
                        AST* arrayTypeNode = td.typeNode;
                        if (arrayTypeNode->kids.size() >= 1) {
                            AST* sizeNode = arrayTypeNode->kids[0].get();
                            if (sizeNode && sizeNode->kind == NodeKind::Primary) {
                                try {
                                    var.arraySize = stoi(sizeNode->text);
                                } catch (...) {
                                    var.arraySize = -1;
                                }
                            }
                        }
                        if (arrayTypeNode->kids.size() > 1) {
                            AST* elemType = arrayTypeNode->kids[1].get();
                            if (elemType->kind == NodeKind::PrimType) {
                                var.elemType = getWASMType(elemType->text);
                            } else if (elemType->kind == NodeKind::UserType) {
                                var.elemType = resolveUserType(elemType->text);
                            }
                        }
                        var.type = "i32";
                    } else {
                        string resolvedType = resolveUserType(typeName);
                        var.type = resolvedType;
                    }
                } else {
                    string resolvedType = resolveUserType(typeName);
                    var.type = resolvedType;
                }
            } else if (typeNode->kind == NodeKind::RecordType) {
                var.type = "i32";
            }
        }
        
        if (var.type.empty()) {
            var.type = "i32";
        }
        
        return var;
    }
    
    void compileVarDecl(AST* varDecl, int indent) {
        if (varDecl->kind != NodeKind::VarDecl) return;
        
        string varName = varDecl->text;
        VarInfo var = collectVarInfo(varDecl);
        
        bool isGlobal = (localScopes.size() == 1);
        
        if (isGlobal) {
            var.isGlobal = true;
            globalVars[varName] = var;
            emitIndent(indent, "(global $" + varName + " (mut " + var.type + ")");
            if (varDecl->kids.size() > 1) {
                compileExpression(varDecl->kids[1].get(), indent + 1);
            } else {
                emitIndent(indent + 1, var.type + ".const 0");
            }
            emitIndent(indent, ")");
        } else {
            var.isGlobal = false;
            var.localIndex = localCounter++;
            localScopes.back()[varName].push_back(var);
        }
    }
    
    void collectLocalVars(AST* body, vector<pair<AST*, int>>& varDeclsWithLevel, int blockLevel = 0) {
        if (!body) return;
        if (body->kind == NodeKind::Body) {
            for (auto& child : body->kids) {
                if (child->kind == NodeKind::VarDecl) {
                    varDeclsWithLevel.push_back({child.get(), blockLevel});
                } else if (child->kind == NodeKind::Body) {
                    // Nested Body (e.g., from if simplification) - increment block level
                    collectLocalVars(child.get(), varDeclsWithLevel, blockLevel + 1);
                } else if (child->kind == NodeKind::If || child->kind == NodeKind::While) {
                    for (size_t i = 1; i < child->kids.size(); i++) {
                        if (child->kids[i]->kind == NodeKind::Body) {
                            collectLocalVars(child->kids[i].get(), varDeclsWithLevel, blockLevel + 1);
                        }
                    }
                }
            }
        }
    }
    
    string getUniqueVarName(const string& baseName, int blockLevel) {
        if (blockLevel == 0) return baseName;
        return baseName + "_" + to_string(blockLevel);
    }
    
    void collectForLoopVars(AST* body, unordered_set<string>& forLoopVars) {
        if (!body) return;
        if (body->kind == NodeKind::Body) {
            for (auto& child : body->kids) {
                if (child->kind == NodeKind::For) {
                    forLoopVars.insert(child->text);
                    for (size_t i = 1; i < child->kids.size(); i++) {
                        if (child->kids[i]->kind == NodeKind::Body) {
                            collectForLoopVars(child->kids[i].get(), forLoopVars);
                        }
                    }
                } else if (child->kind == NodeKind::Body) {
                    collectForLoopVars(child.get(), forLoopVars);
                }
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
        
        memoryOffset = 2048;
        currentBlockLevel = 0;
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
                    localScopes.back()[paramName].push_back(var);
                }
            }
        }
        
        string returnType = "";
        size_t bodyIndex = 1;
        if (header->kids.size() > 1 && header->kids[1]->kind == NodeKind::PrimType) {
            returnType = getWASMType(header->kids[1]->text);
            emitIndent(indent + 1, "(result " + returnType + ")");
            bodyIndex = 2;
        }
        vector<pair<AST*, int>> localVarDeclsWithLevel;
        unordered_set<string> forLoopVars;
        if (header->kids.size() > bodyIndex) {
            AST* body = header->kids[bodyIndex].get();
            if (body->kind == NodeKind::RoutineBodyBlock) {
                if (body->kids.size() > 0 && body->kids[0]->kind == NodeKind::Body) {
                    collectLocalVars(body->kids[0].get(), localVarDeclsWithLevel);
                    collectForLoopVars(body->kids[0].get(), forLoopVars);
                }
            }
        }
        
        unordered_map<string, int> varNameCounters;
        currentVarDeclToUniqueName.clear();
        for (auto& [varDecl, blockLevel] : localVarDeclsWithLevel) {
            if (varDecl) {
                string baseName = varDecl->text;
                int counter = 0;
                if (varNameCounters.count(baseName)) {
                    counter = varNameCounters[baseName];
                }
                varNameCounters[baseName] = counter + 1;
                string uniqueName = (counter == 0) ? baseName : (baseName + "_" + to_string(counter));
                currentVarDeclToUniqueName[varDecl] = uniqueName;
                VarInfo var = collectVarInfo(varDecl);
                var.name = uniqueName;
                var.blockLevel = blockLevel;
                var.isGlobal = false;
                var.localIndex = localCounter++;
                localScopes.back()[baseName].push_back(var);
                emitIndent(indent + 1, "(local $" + uniqueName + " " + var.type + ")");
            }
        }
        
        for (const string& loopVar : forLoopVars) {
            VarInfo var;
            var.name = loopVar;
            var.type = "i32";
            var.isGlobal = false;
            var.localIndex = localCounter++;
            localScopes.back()[loopVar].push_back(var);
            emitIndent(indent + 1, "(local $" + loopVar + " i32)");
            
            string startLocal = "start_" + loopVar;
            string endLocal = "end_" + loopVar;
            var.name = startLocal;
            var.localIndex = localCounter++;
            localScopes.back()[startLocal].push_back(var);
            emitIndent(indent + 1, "(local $" + startLocal + " i32)");
            var.name = endLocal;
            var.localIndex = localCounter++;
            localScopes.back()[endLocal].push_back(var);
            emitIndent(indent + 1, "(local $" + endLocal + " i32)");
            
            string arrBase = "arr_base_" + loopVar;
            string arrSize = "arr_size_" + loopVar;
            string idx = "idx_" + loopVar;
            var.name = arrBase;
            var.localIndex = localCounter++;
            localScopes.back()[arrBase].push_back(var);
            emitIndent(indent + 1, "(local $" + arrBase + " i32)");
            var.name = arrSize;
            var.localIndex = localCounter++;
            localScopes.back()[arrSize].push_back(var);
            emitIndent(indent + 1, "(local $" + arrSize + " i32)");
            var.name = idx;
            var.localIndex = localCounter++;
            localScopes.back()[idx].push_back(var);
            emitIndent(indent + 1, "(local $" + idx + " i32)");
        }
        
        VarInfo tempVar;
        tempVar.name = "__temp_check";
        tempVar.type = "i32";
        tempVar.isGlobal = false;
        tempVar.localIndex = localCounter++;
        localScopes.back()["__temp_check"].push_back(tempVar);
        emitIndent(indent + 1, "(local $__temp_check i32)");
        
        if (header->kids.size() > bodyIndex) {
            AST* body = header->kids[bodyIndex].get();
            if (body->kind == NodeKind::RoutineBodyBlock) {
                if (body->kids.size() > 0 && body->kids[0]->kind == NodeKind::Body) {
                    for (auto& stmt : body->kids[0]->kids) {
                        if (stmt->kind == NodeKind::VarDecl) {
                            string varName = stmt->text;
                            VarInfo* var = nullptr;
                            AST* varDeclNode = stmt.get();
                            string actualVarName = currentVarDeclToUniqueName.count(varDeclNode) ? currentVarDeclToUniqueName[varDeclNode] : varName;
                            if (localScopes.back().count(varName)) {
                                vector<VarInfo>& candidates = localScopes.back()[varName];
                                for (auto& candidate : candidates) {
                                    if (candidate.name == actualVarName) {
                                        var = &candidate;
                                        break;
                                    }
                                }
                            }
                            
                            if (var && var->isArray && var->arraySize > 0) {
                                int elemSize = (var->elemType == "f64") ? 8 : 4;
                                int totalSize = 4 + (var->arraySize * elemSize);
                                // Store array size at base address
                                emitIndent(indent + 1, "i32.const " + to_string(memoryOffset));
                                emitIndent(indent + 1, "i32.const " + to_string(var->arraySize));
                                emitIndent(indent + 1, "i32.store");
                                // Set base address (points to size field, elements start at base+4)
                                emitIndent(indent + 1, "i32.const " + to_string(memoryOffset));
                                emitIndent(indent + 1, "local.set $" + actualVarName);
                                
                                memoryOffset += totalSize;
                                memoryOffset = (memoryOffset + 7) & ~7;
                            } else if (var && varDeclNode && varDeclNode->kids.size() > 0) {
                                AST* typeNode = varDeclNode->kids[0].get();
                                bool isRecord = false;
                                AST* recordTypeNode = nullptr;
                                
                                if (typeNode->kind == NodeKind::RecordType) {
                                    isRecord = true;
                                    recordTypeNode = typeNode;
                                } else if (typeNode->kind == NodeKind::UserType && 
                                         typeTable.count(typeNode->text) &&
                                         typeTable[typeNode->text].kind == NodeKind::RecordType) {
                                    isRecord = true;
                                    TypeDef& td = typeTable[typeNode->text];
                                    if (td.typeNode) recordTypeNode = td.typeNode;
                                }
                                
                                if (isRecord && recordTypeNode) {
                                    // Use getTypeSize to properly calculate record size including nested records
                                    int recordSize = getTypeSize(recordTypeNode);
                                    
                                    if (recordSize > 0) {
                                        emitIndent(indent + 1, "i32.const " + to_string(memoryOffset));
                                        emitIndent(indent + 1, "local.set $" + actualVarName);
                                        memoryOffset += recordSize;
                                        memoryOffset = (memoryOffset + 7) & ~7;
                                    }
                                } else if (stmt->kids.size() > 1) {
                                    compileExpression(stmt->kids[1].get(), indent + 1);
                                    emitIndent(indent + 1, "local.set $" + actualVarName);
                                }
                            } else if (stmt->kids.size() > 1) {
                                compileExpression(stmt->kids[1].get(), indent + 1);
                                emitIndent(indent + 1, "local.set $" + actualVarName);
                            }
                        } else {
                            compileStatement(stmt.get(), indent + 1);
                        }
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
        } else {
        }
        
        localScopes.pop_back();
        emitIndent(indent, ")");
    }
};

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


class SemanticAnalyzer {
private:
    vector<string> errors;
    vector<string> warnings;
    
    struct Symbol {
        string name;
        string type;  // "integer", "real", "boolean", "routine", etc.
        int line, col;
        bool used = false;
        bool isRoutine = false;
        vector<string> paramTypes;  
        string returnType;          
    };
    
    vector<unordered_map<string, Symbol>> scopes;  // stack of visibility
    int insideRoutineDepth = 0;  // depth of nested routines
    
    // table of symbols between traversals
    unordered_map<string, bool> globalUsage;
    
    // Track for loop variables that are read-only
    unordered_set<string> forLoopVariables;  // variables declared in for loops
    
    struct TypeInfo {
        string kind = "unknown";
        int arrayLen = -1;
        string elemKind = "";
    };
    
    unordered_map<string, TypeInfo> typeDefs;
    unordered_map<string, vector<TypeInfo>> funcParamTypesTI;
    unordered_map<string, TypeInfo> funcReturnTI;
    vector<string> routineNameStack;
    AST* rootAST = nullptr;
    
    // Track forward declarations and full definitions
    unordered_map<string, bool> forwardDeclarations;  // routine name -> has body
    unordered_map<string, bool> fullDefinitions;      // routine name -> has full definition
    
    // Stats of optimizing
    int constantFoldingCount = 0;
    int unreachableCodeCount = 0;
    int ifSimplificationCount = 0;
    int unusedVarCount = 0;
    
public:
    // Main method of analysis
    bool analyze(AST* root) {
        
        // Phase 1: Checks (without modifying AST)
        scopes.clear();
        scopes.push_back({});  // global scope
        insideRoutineDepth = 0;
        forLoopVariables.clear();  // Clear for loop variables tracking
        forwardDeclarations.clear();
        fullDefinitions.clear();
        
        rootAST = root;
        buildTypeTable(root);
        checkDeclarationsAndUsage(root);
        checkSizelessArrays(root);
        
        // Check: all forward declarations must have corresponding full definitions
        for (const auto& fwd : forwardDeclarations) {
            if (!fullDefinitions.count(fwd.first)) {
                addError(1, 1, "forward declaration of routine '" + fwd.first + 
                    "' has no corresponding full definition");
            }
        } 

        if (!errors.empty()) {
            cerr << "\nSemantic errors:" << endl;
            for (const auto& err : errors) {
                cerr << "  " << err << endl;
            }
            return false;
        }
        cout << "\nSemantic checks passed" << endl;
        
        // Phase 2: Optimizations (modify AST)
        
        optimizeConstantFolding(root);
        optimizeUnreachableCode(root);
        optimizeIfSimplification(root);
        optimizeUnusedVariables(root);
        
        printOptimizationStats();
        
        if (!warnings.empty()) {
            cout << "\nWarnings:" << endl;
            for (const auto& warn : warnings) {
                cout << "  " << warn << endl;
            }
        }
        return true;
        
    }

    
private:

    TypeInfo typeFromTypeNode(AST* t) {
        TypeInfo ti;
        if (!t) return ti;

        switch (t->kind) {
            case NodeKind::PrimType:
                ti.kind = t->text; // "integer"/"real"/"boolean"
                return ti;

            case NodeKind::UserType: {
                ti.kind = t->text;
                return ti;
            }

            case NodeKind::ArrayType: {
                ti.kind = "array";
                if (t->kids.size() == 2) {
                    AST* sz = t->kids[0].get();
                    AST* el = t->kids[1].get();
                    if (sz && sz->kind == NodeKind::Primary && !sz->text.empty()) {
                        bool neg = (sz->text[0] == '-' || sz->text[0] == '+');
                        bool allDigits = true;
                        for (size_t i = neg ? 1 : 0; i < sz->text.size(); ++i)
                            if (!isdigit(sz->text[i])) { allDigits = false; break; }
                        if (allDigits) ti.arrayLen = stoi(sz->text);
                    }
                    TypeInfo elem = typeFromTypeNode(el);
                    ti.elemKind = elem.kind;
                } else if (t->kids.size() == 1) {
                    AST* el = t->kids[0].get();
                    TypeInfo elem = typeFromTypeNode(el);
                    ti.elemKind = elem.kind;
                    ti.arrayLen = -1;
                }
                return ti;
            }

            case NodeKind::RecordType:
                ti.kind = "record";
                return ti;

            default:
                return ti;
        }
    }

    void buildTypeTable(AST* n) {
        if (!n) return;
        if (n->kind == NodeKind::TypeDecl && !n->kids.empty()) {
            TypeInfo ti = typeFromTypeNode(n->kids[0].get());
            typeDefs[n->text] = ti;
        }
        for (auto& ch : n->kids) buildTypeTable(ch.get());
    }

    bool typesEqual(const TypeInfo& a, const TypeInfo& b) {
        if (a.kind != b.kind) return false;
        if (a.kind == "array") {
            if (!(a.arrayLen == -1 || b.arrayLen == -1 || a.arrayLen == b.arrayLen)) return false;
            return a.elemKind == b.elemKind;
        }
        return true;
    }
    
    bool isCompatibleType(const TypeInfo& target, const TypeInfo& source) {
        if (typesEqual(target, source)) return true;
        
        // integer := real (rounding to nearest)
        if (target.kind == "integer") {
            if (source.kind == "real" || source.kind == "boolean") {
                return true;
            }
        }
        
        // real := integer (direct value copying)
        if (target.kind == "real") {
            if (source.kind == "integer" || source.kind == "boolean") {
                return true;
            }
        }
        
        // boolean := integer (only 1->true, 0->false, but we allow it at compile time)
        if (target.kind == "boolean") {
            if (source.kind == "integer") {
                return true;
            }
        }
        
        // For arrays: check element type compatibility recursively
        if (target.kind == "array" && source.kind == "array") {
            TypeInfo targetElem, sourceElem;
            targetElem.kind = target.elemKind;
            sourceElem.kind = source.elemKind;
            
            auto targetElemIt = typeDefs.find(targetElem.kind);
            if (targetElemIt != typeDefs.end()) {
                targetElem = targetElemIt->second;
            }
            auto sourceElemIt = typeDefs.find(sourceElem.kind);
            if (sourceElemIt != typeDefs.end()) {
                sourceElem = sourceElemIt->second;
            }
            
            bool lengthCompatible = (target.arrayLen == -1 || source.arrayLen == -1 || 
                                    target.arrayLen == source.arrayLen);
            
            if (lengthCompatible) {
                return isCompatibleType(targetElem, sourceElem);
            }
        }
        
        // For user-defined types: expand and check recursively
        if (target.kind != "unknown" && source.kind != "unknown" && 
            target.kind != "array" && target.kind != "record" &&
            source.kind != "array" && source.kind != "record" &&
            target.kind != "integer" && target.kind != "real" && target.kind != "boolean" &&
            source.kind != "integer" && source.kind != "real" && source.kind != "boolean") {
            auto targetIt = typeDefs.find(target.kind);
            auto sourceIt = typeDefs.find(source.kind);
            
            if (targetIt != typeDefs.end() && sourceIt != typeDefs.end()) {
                return isCompatibleType(targetIt->second, sourceIt->second);
            }
        }
        
        // For records: check field-by-field compatibility recursively
        if (target.kind != "unknown" && source.kind != "unknown" && rootAST) {
            AST* targetTypeDecl = nullptr;
            AST* sourceTypeDecl = nullptr;
            
            if (target.kind != "array" && target.kind != "record" && 
                target.kind != "integer" && target.kind != "real" && target.kind != "boolean") {
                targetTypeDecl = findTypeDecl(target.kind, rootAST);
            }
            
            if (source.kind != "array" && source.kind != "record" &&
                source.kind != "integer" && source.kind != "real" && source.kind != "boolean") {
                sourceTypeDecl = findTypeDecl(source.kind, rootAST);
            }
            
            if (targetTypeDecl && sourceTypeDecl) {
                TypeInfo targetTI = typeFromTypeNode(targetTypeDecl->kids[0].get());
                TypeInfo sourceTI = typeFromTypeNode(sourceTypeDecl->kids[0].get());
                
                if (targetTI.kind == "record" && sourceTI.kind == "record") {
                    return areRecordsCompatible(targetTypeDecl, sourceTypeDecl, rootAST);
                }
            }
        }
        
        return false;
    }
    
    AST* findTypeDecl(const string& typeName, AST* root) {
        if (!root) return nullptr;
        if (root->kind == NodeKind::TypeDecl && root->text == typeName) {
            return root;
        }
        for (auto& child : root->kids) {
            AST* result = findTypeDecl(typeName, child.get());
            if (result) return result;
        }
        return nullptr;
    }
    
    bool areRecordsCompatible(AST* targetRecordDecl, AST* sourceRecordDecl, AST*) {
        if (!targetRecordDecl || !sourceRecordDecl) return false;
        
        AST* targetRecordType = targetRecordDecl->kids.empty() ? nullptr : targetRecordDecl->kids[0].get();
        AST* sourceRecordType = sourceRecordDecl->kids.empty() ? nullptr : sourceRecordDecl->kids[0].get();
        
        if (!targetRecordType || targetRecordType->kind != NodeKind::RecordType) return false;
        if (!sourceRecordType || sourceRecordType->kind != NodeKind::RecordType) return false;
        
        unordered_map<string, TypeInfo> targetFields;
        for (auto& child : targetRecordType->kids) {
            if (child->kind == NodeKind::VarDecl && !child->kids.empty()) {
                targetFields[child->text] = typeFromTypeNode(child->kids[0].get());
            }
        }
        
        unordered_map<string, TypeInfo> sourceFields;
        for (auto& child : sourceRecordType->kids) {
            if (child->kind == NodeKind::VarDecl && !child->kids.empty()) {
                sourceFields[child->text] = typeFromTypeNode(child->kids[0].get());
            }
        }
        
        for (const auto& targetField : targetFields) {
            auto sourceIt = sourceFields.find(targetField.first);
            if (sourceIt == sourceFields.end()) {
                return false;
            }
            if (!isCompatibleType(targetField.second, sourceIt->second)) {
                return false;
            }
        }
        
        return true;
    }
    
    TypeInfo getRecordFieldType(const string& recordTypeName, const string& fieldName, AST* root) {
        TypeInfo ti;
        if (!root) return ti;
        
        if (root->kind == NodeKind::TypeDecl && root->text == recordTypeName && !root->kids.empty()) {
            AST* typeNode = root->kids[0].get();
            if (typeNode && typeNode->kind == NodeKind::RecordType) {
                for (auto& child : typeNode->kids) {
                    if (child->kind == NodeKind::VarDecl && child->text == fieldName && !child->kids.empty()) {
                        return typeFromTypeNode(child->kids[0].get());
                    }
                }
            }
        }
        
        for (auto& child : root->kids) {
            TypeInfo result = getRecordFieldType(recordTypeName, fieldName, child.get());
            if (result.kind != "unknown") return result;
        }
        
        return ti;
    }
    
    TypeInfo getRecordFieldTypeFromVarDecl(AST* varDecl, const string& fieldName) {
        TypeInfo ti;
        if (!varDecl || varDecl->kind != NodeKind::VarDecl) return ti;
        
        if (varDecl->kids.size() > 0) {
            AST* typeNode = varDecl->kids[0].get();
            if (typeNode && typeNode->kind == NodeKind::RecordType) {
                for (auto& child : typeNode->kids) {
                    if (child->kind == NodeKind::VarDecl && child->text == fieldName && !child->kids.empty()) {
                        return typeFromTypeNode(child->kids[0].get());
                    }
                }
            }
        }
        
        return ti;
    }
    
    AST* findVarDecl(const string& varName, AST* root) {
        if (!root) return nullptr;
        
        if (root->kind == NodeKind::VarDecl && root->text == varName) {
            return root;
        }
        
        for (auto& child : root->kids) {
            AST* result = findVarDecl(varName, child.get());
            if (result) return result;
        }
        
        return nullptr;
    }
    
    TypeInfo inferExprType(AST* e) {
        TypeInfo ti;
        if (!e) return ti;

        switch (e->kind) {
            case NodeKind::Primary: {
                if (e->text == "true" || e->text == "false") { ti.kind = "boolean"; return ti; }
                if (isInteger(e->text)) { ti.kind = "integer"; return ti; }
                {
                    bool hasDot = false, ok = true;
                    size_t i = (e->text.size() && (e->text[0]=='+'||e->text[0]=='-')) ? 1 : 0;
                    for (; i < e->text.size(); ++i) {
                        if (e->text[i]=='.') { if (hasDot) { ok=false; break; } hasDot=true; }
                        else if (!isdigit(e->text[i])) { ok=false; break; }
                    }
                    if (ok && hasDot) { ti.kind = "real"; return ti; }
                }
                return ti;
            }

            case NodeKind::Name: {
                for (int i = (int)scopes.size()-1; i>=0; --i) {
                    auto it = scopes[i].find(e->text);
                    if (it != scopes[i].end()) {
                        if (it->second.isRoutine) {
                            ti.kind = it->second.returnType.empty() ? "unknown" : it->second.returnType;
                            return ti;
                        }
                        ti.kind = it->second.type;
                        auto td = typeDefs.find(ti.kind);
                        if (td != typeDefs.end() && td->second.kind == "array") {
                            ti = td->second;
                        }
                        return ti;
                    }
                }
                return ti;
            }

            case NodeKind::Call: {
                for (int i = (int)scopes.size()-1; i>=0; --i) {
                    auto it = scopes[i].find(e->text);
                    if (it != scopes[i].end() && it->second.isRoutine) {
                        ti.kind = it->second.returnType.empty() ? "unknown" : it->second.returnType;
                        return ti;
                    }
                }
                return ti;
            }

            case NodeKind::ModPrimary: {
                TypeInfo cur;
                if (!e->kids.empty() && e->kids[0]->kind == NodeKind::Name) {
                    cur = inferExprType(e->kids[0].get());
                } else {
                    cur.kind = "unknown";
                }
                for (size_t i = 1; i < e->kids.size(); ++i) {
                    if (e->kids[i]->kind == NodeKind::Index) {
                        if (cur.kind == "array") {
                            AST* idxExpr = e->kids[i]->kids.empty() ? nullptr : e->kids[i]->kids[0].get();
                            if (idxExpr && idxExpr->kind == NodeKind::Primary && isInteger(idxExpr->text) && cur.arrayLen > 0) {
                                long long idx = stoll(idxExpr->text);
                                if (idx < 1 || idx > cur.arrayLen) {
                                    addError(e->kids[i]->line, e->kids[i]->col,
                                        "index " + to_string(idx) + " is out of bounds 1.." + to_string(cur.arrayLen));
                                }
                            }
                            TypeInfo elemType;
                            elemType.kind = cur.elemKind.empty() ? "unknown" : cur.elemKind;
                            auto td = typeDefs.find(elemType.kind);
                            if (td != typeDefs.end() && td->second.kind == "array") {
                                elemType = td->second;
                            }
                            cur = elemType;
                        } else {
                            cur.kind = "unknown";
                        }
                    } else if (e->kids[i]->kind == NodeKind::Member) {
                        if (cur.kind != "unknown" && rootAST) {
                            string recordTypeName = cur.kind;
                            string fieldName = e->kids[i]->text;
                            TypeInfo fieldType;
                            
                            if (recordTypeName == "record") {
                                if (e->kids[0]->kind == NodeKind::Name) {
                                    AST* varDecl = findVarDecl(e->kids[0]->text, rootAST);
                                    if (varDecl) {
                                        fieldType = getRecordFieldTypeFromVarDecl(varDecl, fieldName);
                                    }
                                }
                            } else {
                                fieldType = getRecordFieldType(recordTypeName, fieldName, rootAST);
                                if (fieldType.kind == "unknown") {
                                    auto td = typeDefs.find(recordTypeName);
                                    if (td != typeDefs.end() && td->second.kind == "record") {
                                        fieldType = getRecordFieldType(recordTypeName, fieldName, rootAST);
                                    }
                                }
                            }
                            
                            if (fieldType.kind != "unknown") {
                                cur = fieldType;
                            } else {
                                cur.kind = "unknown";
                            }
                        } else {
                            cur.kind = "unknown";
                        }
                    }
                }
                return cur;
            }

            case NodeKind::Factor:
            case NodeKind::Simple: {
                if (e->kids.size()==2) {
                    TypeInfo a = inferExprType(e->kids[0].get());
                    TypeInfo b = inferExprType(e->kids[1].get());
                    if (a.kind=="real" || b.kind=="real") { ti.kind="real"; }
                    else if (a.kind=="integer" && b.kind=="integer") { ti.kind="integer"; }
                    else { ti.kind="unknown"; }
                    return ti;
                }
                return ti;
            }

            case NodeKind::Rel:
            case NodeKind::Expr: {
                ti.kind = "boolean";
                return ti;
            }

            default:
                return ti;
        }
    }

    void checkDeclarationsAndUsage(AST* node) {
        if (!node) return;
        
        if (node->kind == NodeKind::RoutineDecl) {
            // Check: nested routines are not allowed (only global routines)
            if (insideRoutineDepth > 0) {
                AST* header = node->kids.size() > 0 && node->kids[0]->kind == NodeKind::RoutineHeader 
                    ? node->kids[0].get() : nullptr;
                if (header) {
                    addError(header->line, header->col,
                        "nested routines are not allowed; only global routines are supported");
                }
            }
            
            if (node->kids.size() > 0 && node->kids[0]->kind == NodeKind::RoutineHeader) {
                AST* header = node->kids[0].get();
                string routineName = header->text;
                
                Symbol sym;
                sym.name = routineName;
                sym.type = "routine";
                sym.line = header->line;
                sym.col = header->col;
                sym.isRoutine = true;
                
                if (header->kids.size() > 0 && header->kids[0]->kind == NodeKind::Params) {
                    for (auto& param : header->kids[0]->kids) {
                        if (param->kind == NodeKind::Param && param->kids.size() > 0) {
                            sym.paramTypes.push_back(getTypeString(param->kids[0].get()));
                        }
                    }
                }
                
                if (header->kids.size() > 1 && header->kids[1]->kind != NodeKind::RoutineBodyBlock 
                    && header->kids[1]->kind != NodeKind::RoutineBodyExpr) {
                    sym.returnType = getTypeString(header->kids[1].get());
                }
                
                // Check if this is a forward declaration (no body) or full definition
                bool hasBody = false;
                for (size_t i = 1; i < header->kids.size(); i++) {
                    if (header->kids[i]->kind == NodeKind::RoutineBodyBlock || 
                        header->kids[i]->kind == NodeKind::RoutineBodyExpr) {
                        hasBody = true;
                        break;
                    }
                }
                
                // Check for duplicate declarations
                if (scopes.back().count(routineName)) {
                    // Check if it's a forward declaration being completed
                    if (forwardDeclarations.count(routineName) && hasBody) {
                        // Verify signature matches
                        Symbol& existing = scopes.back()[routineName];
                        if (existing.paramTypes.size() != sym.paramTypes.size() ||
                            existing.returnType != sym.returnType) {
                            addError(header->line, header->col,
                                "forward declaration signature mismatch for routine '" + routineName + "'");
                        } else {
                            // Check parameter types match
                            for (size_t i = 0; i < sym.paramTypes.size(); i++) {
                                if (existing.paramTypes[i] != sym.paramTypes[i]) {
                                    addError(header->line, header->col,
                                        "forward declaration parameter type mismatch for routine '" + routineName + "'");
                                    break;
                                }
                            }
                        }
                        fullDefinitions[routineName] = true;
                    } else {
                        addError(header->line, header->col,
                            "duplicate declaration of routine '" + routineName + "'");
                    }
                } else {
                    scopes.back()[routineName] = sym;
                    if (!hasBody) {
                        forwardDeclarations[routineName] = true;
                    } else {
                        fullDefinitions[routineName] = true;
                    }
                }
                
                vector<TypeInfo> ptiList;
                if (header->kids.size() > 0 && header->kids[0]->kind == NodeKind::Params) {
                    for (auto& param : header->kids[0]->kids) {
                        if (param->kind == NodeKind::Param && !param->kids.empty()) {
                            ptiList.push_back(typeFromTypeNode(param->kids[0].get()));
                        }
                    }
                }
                funcParamTypesTI[routineName] = std::move(ptiList);
                
                if (!sym.returnType.empty()) {
                    for (auto& ch : header->kids)
                        if (ch && (ch->kind==NodeKind::PrimType||ch->kind==NodeKind::UserType
                                || ch->kind==NodeKind::ArrayType||ch->kind==NodeKind::RecordType)) {
                            funcReturnTI[routineName] = typeFromTypeNode(ch.get());
                            break;
                        }
                }
            }
            
            scopes.push_back({});
            insideRoutineDepth++;
            
            if (node->kids.size() > 0 && node->kids[0]->kind == NodeKind::RoutineHeader) {
                routineNameStack.push_back(node->kids[0]->text);
            }
            
            if (node->kids.size() > 0 && node->kids[0]->kind == NodeKind::RoutineHeader) {
                AST* header = node->kids[0].get();
                if (header->kids.size() > 0 && header->kids[0]->kind == NodeKind::Params) {
                    for (auto& param : header->kids[0]->kids) {
                        if (param->kind == NodeKind::Param) {
                            Symbol pSym;
                            pSym.name = param->text;
                            pSym.type = param->kids.size() > 0 ? getTypeString(param->kids[0].get()) : "unknown";
                            pSym.line = param->line;
                            pSym.col = param->col;
                            pSym.used = true;  
                            scopes.back()[param->text] = pSym;
                        }
                    }
                }
            }
            
            for (auto& kid : node->kids) {
                checkDeclarationsAndUsage(kid.get());
            }
            
            AST *header = nullptr;
            if (!node->kids.empty() && node->kids[0]->kind == NodeKind::RoutineHeader)
            {
                header = node->kids[0].get();
            }

            auto isTypeNode = [](AST *n)
            {
                if (!n)
                    return false;
                switch (n->kind)
                {
                case NodeKind::PrimType:
                case NodeKind::UserType:
                case NodeKind::ArrayType:
                case NodeKind::RecordType:
                    return true;
                default:
                    return false;
                }
            };

            auto findHeaderChild = [&](NodeKind k) -> AST *
            {
                if (!header)
                    return nullptr;
                for (auto &ch : header->kids)
                    if (ch->kind == k)
                        return ch.get();
                return nullptr;
            };

            bool hasReturnType = false;
            if (header)
            {
                for (auto &ch : header->kids)
                    if (isTypeNode(ch.get()))
                    {
                        hasReturnType = true;
                        break;
                    }
            }

            AST *bodyExpr = findHeaderChild(NodeKind::RoutineBodyExpr);
            AST *bodyBlock = findHeaderChild(NodeKind::RoutineBodyBlock);

            if (hasReturnType)
            {
                bool ok = (bodyExpr != nullptr)                       
                          || (bodyBlock && hasReturnStmt(bodyBlock)); 

                if (!ok && header)
                {
                    addError(header->line, header->col,
                             "function '" + header->text + "' must return a value");
                }
            }

            scopes.pop_back();
            insideRoutineDepth--;
            if (!routineNameStack.empty()) routineNameStack.pop_back();
            return;
        }
        
        if (node->kind == NodeKind::VarDecl) {
            Symbol sym;
            sym.name = node->text;
            sym.type = node->kids.size() > 0 ? getTypeString(node->kids[0].get()) : "unknown";
            sym.line = node->line;
            sym.col = node->col;
            
            scopes.back()[node->text] = sym;
            
            for (auto& kid : node->kids) {
                checkDeclarationsAndUsage(kid.get());
            }
            
            if (node->kids.size() >= 2) {
                AST* typeNode = node->kids[0].get();
                AST* initExpr = node->kids[1].get();
                
                bool isTypeNode = (typeNode->kind == NodeKind::PrimType || 
                                  typeNode->kind == NodeKind::UserType ||
                                  typeNode->kind == NodeKind::ArrayType ||
                                  typeNode->kind == NodeKind::RecordType);
                
                if (isTypeNode && initExpr) {
                    TypeInfo declaredType = typeFromTypeNode(typeNode);
                    TypeInfo initType = inferExprType(initExpr);
                    
                    if (declaredType.kind != "unknown" && initType.kind != "unknown" && 
                        !isCompatibleType(declaredType, initType)) {
                        addError(initExpr->line, initExpr->col,
                            "type mismatch in variable initialization: declared type is '" +
                            (declaredType.kind=="array" ? ("array(" + declaredType.elemKind + (declaredType.arrayLen>0?(","+to_string(declaredType.arrayLen)):"") + ")") : declaredType.kind) +
                            "', initializer type is '" +
                            (initType.kind=="array" ? ("array(" + initType.elemKind + (initType.arrayLen>0?(","+to_string(initType.arrayLen)):"") + ")") : initType.kind) +
                            "'");
                    }
                }
            }
            return;
        }
        
        if (node->kind == NodeKind::For) {
            scopes.push_back({});
            
            Symbol loopVar;
            loopVar.name = node->text;
            loopVar.type = "integer";
            loopVar.line = node->line;
            loopVar.col = node->col;
            loopVar.used = true;  
            scopes.back()[node->text] = loopVar;
            
            // Mark loop variable as read-only
            forLoopVariables.insert(node->text);
            
            for (auto& kid : node->kids) {
                checkDeclarationsAndUsage(kid.get());
            }
            
            // Remove loop variable from read-only set after loop scope
            forLoopVariables.erase(node->text);
            scopes.pop_back();
            return;
        }
        
        if (node->kind == NodeKind::ReturnStmt) {
            if (insideRoutineDepth == 0) {
                addError(node->line, node->col, 
                    "return statement can only be used inside a routine");
            }
            
            if (!routineNameStack.empty()) {
                string cur = routineNameStack.back();
                auto rit = funcReturnTI.find(cur);
                if (rit != funcReturnTI.end()) {
                    const TypeInfo& retFormal = rit->second;
                    if (!node->kids.empty()) {
                        TypeInfo retActual = inferExprType(node->kids[0].get());
                        if (retFormal.kind != "unknown" && retActual.kind != "unknown" &&
                            !isCompatibleType(retFormal, retActual)) {
                            addError(node->kids[0]->line, node->kids[0]->col,
                                "return type mismatch: expected '" +
                                (retFormal.kind=="array" ? ("array(" + retFormal.elemKind + (retFormal.arrayLen>0?(","+to_string(retFormal.arrayLen)):"") + ")") : retFormal.kind) +
                                "', got '" +
                                (retActual.kind=="array" ? ("array(" + retActual.elemKind + (retActual.arrayLen>0?(","+to_string(retActual.arrayLen)):"") + ")") : retActual.kind) +
                                "'");
                        }
                    }
                }
            }
        }
        
        if (node->kind == NodeKind::Name) {
            if (node->text == "reverse")
            {
                return;
            }
            string varName = node->text;
            
            bool found = false;
            for (int i = scopes.size() - 1; i >= 0; i--) {
                if (scopes[i].count(varName)) {
                    scopes[i][varName].used = true;
                    globalUsage[varName] = true;  
                    found = true;
                    break;
                }
            }
            
            if (!found) {
                addError(node->line, node->col, 
                    "variable '" + varName + "' used before declaration");
            }
        }
        
        if (node->kind == NodeKind::Call) {
            string funcName = node->text;
            
            bool found = false;
            for (int i = scopes.size() - 1; i >= 0; i--) {
                if (scopes[i].count(funcName)) {
                    scopes[i][funcName].used = true;
                    
                    Symbol& funcSym = scopes[i][funcName];
                    if (funcSym.isRoutine) {
                        size_t expectedArgs = funcSym.paramTypes.size();
                        size_t actualArgs = node->kids.size();
                        
                        if (actualArgs != expectedArgs) {
                            addError(node->line, node->col,
                                "function '" + funcName + "' expects " + 
                                to_string(expectedArgs) + " arguments, but " +
                                to_string(actualArgs) + " provided");
                        }
                        
                        auto tiIt = funcParamTypesTI.find(funcName);
                        if (tiIt != funcParamTypesTI.end()) {
                            const auto& formals = tiIt->second;
                            if (formals.size() == node->kids.size()) {
                                for (size_t k = 0; k < formals.size(); ++k) {
                                    TypeInfo actual = inferExprType(node->kids[k].get());
                                    const TypeInfo& formal = formals[k];
                                    if (!isCompatibleType(formal, actual)) {
                                        addError(node->kids[k]->line, node->kids[k]->col,
                                            "type mismatch for argument " + to_string(k+1) +
                                            " in call to '" + funcName + "': expected '" +
                                            (formal.kind=="array" ? ("array(" + formal.elemKind + (formal.arrayLen>0?(","+to_string(formal.arrayLen)):"") + ")") : formal.kind) +
                                            "', got '" +
                                            (actual.kind=="array" ? ("array(" + actual.elemKind + (actual.arrayLen>0?(","+to_string(actual.arrayLen)):"") + ")") : actual.kind) +
                                            "'");
                                    }
                                }
                            }
                        }
                    }
                    
                    found = true;
                    break;
                }
            }
            
            if (!found) {
                addError(node->line, node->col,
                    "function '" + funcName + "' used before declaration");
            }
        }
        
        if (node->kind == NodeKind::ModPrimary) {
            inferExprType(node);
        }
        
        if (node->kind == NodeKind::Assign) {
            if (node->kids.size() > 0) {
                AST* lhs = node->kids[0].get();
                if (lhs->kind == NodeKind::ModPrimary && lhs->kids.size() > 0) {
                    if (lhs->kids[0]->kind == NodeKind::Name) {
                        string varName = lhs->kids[0]->text;
                        
                        // Check if trying to assign to for loop variable (read-only)
                        if (forLoopVariables.count(varName)) {
                            addError(lhs->kids[0]->line, lhs->kids[0]->col,
                                "loop variable '" + varName + "' is read-only");
                            return;  // Don't continue checking this assignment
                        }
                        
                        bool found = false;
                        for (int i = scopes.size() - 1; i >= 0; i--) {
                            if (scopes[i].count(varName)) {
                                scopes[i][varName].used = true;
                                found = true;
                                break;
                            }
                        }
                        
                        if (!found) {
                            addError(lhs->kids[0]->line, lhs->kids[0]->col,
                                "variable '" + varName + "' used before declaration");
                        }
                    }
                }
            }
            
            if (node->kids.size() > 1) {
                checkDeclarationsAndUsage(node->kids[1].get());
            }
            
            if (node->kids.size() > 1) {
                TypeInfo lt = inferExprType(node->kids[0].get());
                TypeInfo rt = inferExprType(node->kids[1].get());
                if (lt.kind != "unknown" && rt.kind != "unknown" && !isCompatibleType(lt, rt)) {
                    addError(node->line, node->col,
                        "type mismatch in assignment: left is '" +
                        (lt.kind=="array" ? ("array(" + lt.elemKind + (lt.arrayLen>0?(","+to_string(lt.arrayLen)):"") + ")") : lt.kind) +
                        "', right is '" +
                        (rt.kind=="array" ? ("array(" + rt.elemKind + (rt.arrayLen>0?(","+to_string(rt.arrayLen)):"") + ")") : rt.kind) +
                        "'");
                }
            }
            return;
        }
        
        // Check While loop: condition must be boolean
        if (node->kind == NodeKind::While && node->kids.size() > 0) {
            TypeInfo condType = inferExprType(node->kids[0].get());
            if (condType.kind != "unknown" && condType.kind != "boolean") {
                addError(node->kids[0]->line, node->kids[0]->col,
                    "while loop condition must be boolean, got '" + condType.kind + "'");
            }
        }
        
        // Check If statement: condition must be boolean
        if (node->kind == NodeKind::If && node->kids.size() > 0) {
            TypeInfo condType = inferExprType(node->kids[0].get());
            if (condType.kind != "unknown" && condType.kind != "boolean") {
                addError(node->kids[0]->line, node->kids[0]->col,
                    "if statement condition must be boolean, got '" + condType.kind + "'");
            }
        }
        
        // Check For loop: range expressions must be integer
        if (node->kind == NodeKind::For && node->kids.size() > 0) {
            AST* rangeNode = node->kids[0].get();
            if (rangeNode->kind == NodeKind::Expr && rangeNode->text == "..") {
                // Range with two expressions
                if (rangeNode->kids.size() >= 2) {
                    TypeInfo startType = inferExprType(rangeNode->kids[0].get());
                    TypeInfo endType = inferExprType(rangeNode->kids[1].get());
                    if (startType.kind != "unknown" && startType.kind != "integer") {
                        addError(rangeNode->kids[0]->line, rangeNode->kids[0]->col,
                            "for loop range start must be integer, got '" + startType.kind + "'");
                    }
                    if (endType.kind != "unknown" && endType.kind != "integer") {
                        addError(rangeNode->kids[1]->line, rangeNode->kids[1]->col,
                            "for loop range end must be integer, got '" + endType.kind + "'");
                    }
                }
            }
        }
        
        for (auto& kid : node->kids) {
            checkDeclarationsAndUsage(kid.get());
        }
    }
    
    void checkSizelessArrays(AST *node, bool inParams = false)
    {
        if (!node)
            return;

        if (node->kind == NodeKind::ArrayType)
        {
            bool sizeless = (node->kids.size() == 1);
            if (sizeless && !inParams)
            {
                addError(node->line, node->col,
                         "sizeless array 'array[]' is allowed only in routine parameter types");
            }
        }

        if (node->kind == NodeKind::Params)
        {
            for (auto &ch : node->kids)
                checkSizelessArrays(ch.get(), true);
            return;
        }

        for (auto &ch : node->kids)
            checkSizelessArrays(ch.get(), inParams);
    }

    static bool hasReturnStmt(AST *n)
    {
        if (!n)
            return false;
        if (n->kind == NodeKind::ReturnStmt)
            return true;
        for (auto &c : n->kids)
            if (hasReturnStmt(c.get()))
                return true;
        return false;
    }
    
    void optimizeConstantFolding(AST* node) {
        if (!node) return;
        
        for (auto& kid : node->kids) {
            optimizeConstantFolding(kid.get());
        }
        
        if (node->kind == NodeKind::Factor || node->kind == NodeKind::Simple || 
            node->kind == NodeKind::Rel || node->kind == NodeKind::Expr) {
            
            if (node->kids.size() == 2) {
                AST* left = node->kids[0].get();
                AST* right = node->kids[1].get();
                
                if (left->kind == NodeKind::Primary && right->kind == NodeKind::Primary) {
                    bool leftIsInt = isInteger(left->text);
                    bool rightIsInt = isInteger(right->text);
                    bool leftIsBool = (left->text == "true" || left->text == "false");
                    bool rightIsBool = (right->text == "true" || right->text == "false");
                    
                    if (leftIsInt && rightIsInt) {
                        long long lval = stoll(left->text);
                        long long rval = stoll(right->text);
                        long long result = 0;
                        bool computed = false;
                        
                        if (node->text == "+") { result = lval + rval; computed = true; }
                        else if (node->text == "-") { result = lval - rval; computed = true; }
                        else if (node->text == "*") { result = lval * rval; computed = true; }
                        else if (node->text == "/" && rval != 0) { result = lval / rval; computed = true; }
                        else if (node->text == "%" && rval != 0) { result = lval % rval; computed = true; }
                        
                        if (computed) {
                            node->kind = NodeKind::Primary;
                            node->text = to_string(result);
                            node->kids.clear();
                            constantFoldingCount++;
                            return;
                        }
                        
                        bool boolResult = false;
                        bool isBoolOp = false;
                        
                        if (node->text == "<") { boolResult = lval < rval; isBoolOp = true; }
                        else if (node->text == "<=") { boolResult = lval <= rval; isBoolOp = true; }
                        else if (node->text == ">") { boolResult = lval > rval; isBoolOp = true; }
                        else if (node->text == ">=") { boolResult = lval >= rval; isBoolOp = true; }
                        else if (node->text == "=") { boolResult = lval == rval; isBoolOp = true; }
                        else if (node->text == "/=") { boolResult = lval != rval; isBoolOp = true; }
                        
                        if (isBoolOp) {
                            node->kind = NodeKind::Primary;
                            node->text = boolResult ? "true" : "false";
                            node->kids.clear();
                            constantFoldingCount++;
                            return;
                        }
                    }
                    
                    if (leftIsBool && rightIsBool) {
                        bool lval = (left->text == "true");
                        bool rval = (right->text == "true");
                        bool result = false;
                        bool computed = false;
                        
                        if (node->text == "and") { result = lval && rval; computed = true; }
                        else if (node->text == "or") { result = lval || rval; computed = true; }
                        else if (node->text == "xor") { result = lval != rval; computed = true; }
                        
                        if (computed) {
                            node->kind = NodeKind::Primary;
                            node->text = result ? "true" : "false";
                            node->kids.clear();
                            constantFoldingCount++;
                            return;
                        }
                    }
                }
            }
        }
        
        if (node->kind == NodeKind::Primary && node->text == "not" && node->kids.size() == 1) {
            AST* child = node->kids[0].get();
            if (child->kind == NodeKind::Primary) {
                if (child->text == "true") {
                    node->text = "false";
                    node->kids.clear();
                    constantFoldingCount++;
                } else if (child->text == "false") {
                    node->text = "true";
                    node->kids.clear();
                    constantFoldingCount++;
                }
            }
        }
    }
    
    void optimizeUnreachableCode(AST* node) {
        if (!node) return;
        
        if (node->kind == NodeKind::Body || node->kind == NodeKind::Program) {
            bool foundReturn = false;
            vector<size_t> toDelete;
            
            for (size_t i = 0; i < node->kids.size(); i++) {
                if (foundReturn) {
                    toDelete.push_back(i);
                    unreachableCodeCount++;
                } else if (node->kids[i]->kind == NodeKind::ReturnStmt) {
                    foundReturn = true;
                }
            }
            
            for (int i = toDelete.size() - 1; i >= 0; i--) {
                node->kids.erase(node->kids.begin() + toDelete[i]);
            }
        }
        
        for (auto& kid : node->kids) {
            optimizeUnreachableCode(kid.get());
        }
    }
    
    void optimizeIfSimplification(AST* node) {
        if (!node) return;
        
        for (auto& kid : node->kids) {
            optimizeIfSimplification(kid.get());
        }
        
        if (node->kind == NodeKind::If && node->kids.size() >= 2) {
            AST* condition = node->kids[0].get();
            
            if (condition->kind == NodeKind::Primary) {
                if (condition->text == "true") {
                    AST* thenBody = node->kids[1].get();
                    
                    vector<unique_ptr<AST>> newKids;
                    for (auto& stmt : thenBody->kids) {
                        newKids.push_back(std::move(stmt));
                    }
                    
                    node->kids = std::move(newKids);
                    node->kind = NodeKind::Body;
                    node->text = "";
                    
                    ifSimplificationCount++;
                } else if (condition->text == "false") {
                    if (node->kids.size() >= 3) {
                        AST* elseBody = node->kids[2].get();
                        
                        vector<unique_ptr<AST>> newKids;
                        for (auto& stmt : elseBody->kids) {
                            newKids.push_back(std::move(stmt));
                        }
                        
                        node->kids = std::move(newKids);
                        node->kind = NodeKind::Body;
                        node->text = "";
                    } else {
                        node->kids.clear();
                        node->kind = NodeKind::Body;
                        node->text = "";
                    }
                    
                    ifSimplificationCount++;
                }
            }
        }
    }
    
    void optimizeUnusedVariables(AST* node) {
        if (!node) return;
        
        removeUnusedVars(node, 0);
    }
    
    void removeUnusedVars(AST* node, int scopeLevel) {
        if (!node) return;
        
        int nextLevel = scopeLevel;
        if (node->kind == NodeKind::RoutineDecl || node->kind == NodeKind::For) {
            nextLevel = scopeLevel + 1;
        }
        
        if (node->kind == NodeKind::Body || node->kind == NodeKind::Program) {
            vector<size_t> toDelete;
            
            for (size_t i = 0; i < node->kids.size(); i++) {
                if (node->kids[i]->kind == NodeKind::VarDecl) {
                    string varName = node->kids[i]->text;
                    
                    bool used = globalUsage.count(varName) && globalUsage[varName];
                    if (!used) {
                        toDelete.push_back(i);
                        unusedVarCount++;
                        addWarning(node->kids[i]->line, node->kids[i]->col,
                            "unused variable '" + varName + "' removed");
                    }
                }
            }
            
            for (int i = toDelete.size() - 1; i >= 0; i--) {
                node->kids.erase(node->kids.begin() + toDelete[i]);
            }
        }
        
        for (auto& kid : node->kids) {
            removeUnusedVars(kid.get(), nextLevel);
        }
    }
    
    string getTypeString(AST* typeNode) {
        if (!typeNode) return "unknown";
        if (typeNode->kind == NodeKind::PrimType) return typeNode->text;
        if (typeNode->kind == NodeKind::UserType) return typeNode->text;
        if (typeNode->kind == NodeKind::ArrayType) return "array";
        if (typeNode->kind == NodeKind::RecordType) return "record";
        return "unknown";
    }
    
    bool isInteger(const string& s) {
        if (s.empty()) return false;
        size_t start = 0;
        if (s[0] == '-' || s[0] == '+') start = 1;
        if (start >= s.size()) return false;
        for (size_t i = start; i < s.size(); i++) {
            if (!isdigit(s[i])) return false;
        }
        return true;
    }
    
    void addError(int line, int col, const string& msg) {
        ostringstream os;
        os << "Error at " << line << ":" << col << ": " << msg;
        errors.push_back(os.str());
    }
    
    void addWarning(int line, int col, const string& msg) {
        ostringstream os;
        os << "Warning at " << line << ":" << col << ": " << msg;
        warnings.push_back(os.str());
    }
    
    void printOptimizationStats() {
        cout << "\nConstant folding: " << constantFoldingCount << " changed" << endl;
        cout << "Unreachable code: " << unreachableCodeCount << " changed" << endl;
        cout << "If simplification: " << ifSimplificationCount << " changed" << endl;
        cout << "Unused variables: " << unusedVarCount << " changed" << endl;

        int total = constantFoldingCount + unreachableCodeCount + ifSimplificationCount + unusedVarCount;
        cout << "Total optimizations: " << total << endl;
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
        
        cout << "Syntax checks passed" << endl;
        
        // Semantic analysis
        SemanticAnalyzer analyzer;
        bool isValid = analyzer.analyze(tree.get());
        if (!isValid) {
            cerr << "\nSemantic analysis failed. Compilation aborted.\n";
            return 3;
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

