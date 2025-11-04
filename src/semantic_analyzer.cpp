// semantic_analyzer.cpp — семантический анализатор для Project I
// Реализует проверки и оптимизации AST
// Build: g++ -std=c++20 -O2 src/lexer.cpp src/parser.cpp src/semantic_analyzer.cpp -o build/semantic_analyzer
// Run: ./build/semantic_analyzer tests/001_expressions.pi

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

using namespace std;

// ==================== Импорт структур из parser.cpp ====================
enum class TokenKind {
    Identifier, Integer, Real, Boolean,
    KW_var, KW_type, KW_integer, KW_real, KW_boolean,
    KW_record, KW_array, KW_is, KW_end, KW_while, KW_loop,
    KW_for, KW_in, KW_reverse, KW_if, KW_then, KW_else,
    KW_print, KW_routine, KW_and, KW_or, KW_xor, KW_not,
    KW_return,
    LParen, RParen, LBracket, RBracket,
    Comma, Colon, Dot, Range, Assign, Arrow,
    Plus, Minus, Star, Slash, Percent,
    LT, LE, GT, GE, EQ, NE,
    Apostrophe,
    Separator,
    EndOfFile, Error
};

struct Token {
    TokenKind kind;
    string    lexeme;
    int       line = 1;
    int       col  = 1;
    long long intValue = 0;
    long double realValue = 0.0L;
    bool      boolValue = false;
};

struct Lexer {
    string src;
    size_t i = 0;
    int line = 1, col = 1;
    bool   hasPendingError = false;
    Token  pendingErrorToken;

    explicit Lexer(string s): src(std::move(s)) {}

    bool eof() const { return i >= src.size(); }
    char peek(size_t k=0) const { return (i+k<src.size()) ? src[i+k] : '\0'; }

    char advance() {
        if (eof()) return '\0';
        char c = src[i++];
        if (c == '\n') { line++; col = 1; }
        else           { col++; }
        return c;
    }
    bool match(char expected) {
        if (peek() == expected) { advance(); return true; }
        return false;
    }

    static bool isIdentStart(char c) {
        return std::isalpha(static_cast<unsigned char>(c)) || c=='_';
    }
    static bool isIdentCont(char c) {
        return std::isalnum(static_cast<unsigned char>(c)) || c=='_';
    }

    void skipWhitespaceAndComments() {
        for (;;) {
            while (true) {
                char c = peek();
                if (c==' ' || c=='\t' || c=='\r') { advance(); }
                else break;
            }
            if (peek()=='/' && peek(1)=='/') {
                while (peek()!='\n' && !eof()) advance();
                continue;
            }
            if (peek()=='/' && peek(1)=='*') {
                int start_line = line, start_col = col;
                advance(); advance();
                bool closed = false;
                while (!eof()) {
                    if (peek()=='*' && peek(1)=='/') {
                        advance(); advance();
                        closed = true;
                        break;
                    }
                    advance();
                }
                if (!closed) {
                    hasPendingError = true;
                    pendingErrorToken.kind  = TokenKind::Error;
                    pendingErrorToken.lexeme= "unterminated block comment";
                    pendingErrorToken.line  = start_line;
                    pendingErrorToken.col   = start_col;
                    return;
                }
                continue;
            }
            break;
        }
    }

    static TokenKind keywordKind(const string& s, bool& isBoolLit, bool& boolVal) {
        static const unordered_map<string, TokenKind> kw = {
            {"var",TokenKind::KW_var},{"type",TokenKind::KW_type},
            {"integer",TokenKind::KW_integer},{"real",TokenKind::KW_real},
            {"boolean",TokenKind::KW_boolean},{"record",TokenKind::KW_record},
            {"array",TokenKind::KW_array},{"is",TokenKind::KW_is},
            {"end",TokenKind::KW_end},{"while",TokenKind::KW_while},
            {"loop",TokenKind::KW_loop},{"for",TokenKind::KW_for},
            {"in",TokenKind::KW_in},{"reverse",TokenKind::KW_reverse},
            {"if",TokenKind::KW_if},{"then",TokenKind::KW_then},
            {"else",TokenKind::KW_else},{"print",TokenKind::KW_print},
            {"routine",TokenKind::KW_routine},
            {"and",TokenKind::KW_and},{"or",TokenKind::KW_or},
            {"xor",TokenKind::KW_xor},{"not",TokenKind::KW_not},
            {"return",TokenKind::KW_return},
        };
        isBoolLit = false; boolVal = false;
        if (s=="true")  { isBoolLit=true; boolVal=true;  return TokenKind::Boolean; }
        if (s=="false") { isBoolLit=true; boolVal=false; return TokenKind::Boolean; }
        auto it = kw.find(s);
        return (it!=kw.end()) ? it->second : TokenKind::Identifier;
    }

    Token make(TokenKind k, const string& lex, int l, int c) {
        Token t; t.kind=k; t.lexeme=lex; t.line=l; t.col=c; return t;
    }

    Token next() {
        if (hasPendingError) {
            hasPendingError = false;
            return pendingErrorToken;
        }

        skipWhitespaceAndComments();

        if (hasPendingError) {
            hasPendingError = false;
            return pendingErrorToken;
        }

        int tl = line, tc = col;
        if (eof()) return make(TokenKind::EndOfFile, "<eof>", tl, tc);

        char c = advance();
        
        if (c == '\n') {
            return make(TokenKind::Separator, "\\n", tl, tc);
        }
        if (isIdentStart(c)) {
            string s(1,c);
            while (isIdentCont(peek())) s.push_back(advance());
            bool isBool=false, bval=false;
            TokenKind k = keywordKind(s, isBool, bval);
            Token t = make(k, s, tl, tc);
            if (k==TokenKind::Boolean) t.boolValue = bval;
            return t;
        }

        if (std::isdigit(static_cast<unsigned char>(c))) {
            string s(1,c);
            while (std::isdigit(static_cast<unsigned char>(peek()))) s.push_back(advance());

            if (peek()=='.' && peek(1)!='.' && std::isdigit(static_cast<unsigned char>(peek(1)))) {
                s.push_back(advance());
                while (std::isdigit(static_cast<unsigned char>(peek()))) s.push_back(advance());
                Token t = make(TokenKind::Real, s, tl, tc);
                t.realValue = std::strtold(s.c_str(), nullptr);
                return t;
            } else {
                Token t = make(TokenKind::Integer, s, tl, tc);
                t.intValue = std::stoll(s);
                return t;
            }
        }

        switch (c) {
            case ':': if (match('=')) return make(TokenKind::Assign, ":=", tl, tc);
                      return make(TokenKind::Colon, ":", tl, tc);
            case '.': if (match('.')) return make(TokenKind::Range, "..", tl, tc);
                      return make(TokenKind::Dot, ".", tl, tc);
            case '=': if (match('>')) return make(TokenKind::Arrow, "=>", tl, tc);
                      return make(TokenKind::EQ, "=", tl, tc);
            case '<': if (match('=')) return make(TokenKind::LE, "<=", tl, tc);
                      return make(TokenKind::LT, "<", tl, tc);
            case '>': if (match('=')) return make(TokenKind::GE, ">=", tl, tc);
                      return make(TokenKind::GT, ">", tl, tc);
            case '/': if (match('=')) return make(TokenKind::NE, "/=", tl, tc);
                      return make(TokenKind::Slash, "/", tl, tc);
            case '\'': return make(TokenKind::Apostrophe, "'", tl, tc);
        }
        switch (c) {
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

        return make(TokenKind::Error, string(1,c), tl, tc);
    }
};

string kindName(TokenKind k) {
    switch (k) {
#define C(x) case TokenKind::x: return #x;
        C(Identifier) C(Integer) C(Real) C(Boolean)
        C(KW_var) C(KW_type) C(KW_integer) C(KW_real) C(KW_boolean)
        C(KW_record) C(KW_array) C(KW_is) C(KW_end) C(KW_while) C(KW_loop)
        C(KW_for) C(KW_in) C(KW_reverse) C(KW_if) C(KW_then) C(KW_else)
        C(KW_print) C(KW_routine) C(KW_and) C(KW_or) C(KW_xor) C(KW_not)
        C(LParen) C(RParen) C(LBracket) C(RBracket)
        C(Comma) C(Colon) C(Dot) C(Range) C(Assign) C(Arrow)
        C(Plus) C(Minus) C(Star) C(Slash) C(Percent)
        C(LT) C(LE) C(GT) C(GE) C(EQ) C(NE)
        C(Apostrophe)
        C(Separator)
        C(EndOfFile) C(Error) C(KW_return)
#undef C
    }
    return "?";
}

struct ParseError: std::runtime_error {
    using std::runtime_error::runtime_error;
};

enum class NodeKind {
    Program, VarDecl, TypeDecl, RecordType, ArrayType, UserType, PrimType,
    RoutineDecl, RoutineHeader, RoutineBodyBlock, RoutineBodyExpr, Params, Param,
    Body, StmtList, Assign, While, For, If, Print, Call, ReturnStmt,
    Expr, Rel, Simple, Factor, Summand, Primary, ModPrimary, Name, Index, Member
};

struct AST {
    NodeKind kind;
    string   text;
    vector<unique_ptr<AST>> kids;
    int line=1, col=1;
    
    // Дополнительные поля для оптимизации
    bool markedForDeletion = false;
    
    AST(NodeKind k, string t="", int l=1, int c=1): kind(k), text(std::move(t)), line(l), col(c) {}
    AST* add(unique_ptr<AST> ch){ kids.emplace_back(std::move(ch)); return kids.back().get(); }
};

struct Parser {
    Lexer& L;
    Token  t;
    bool   buildAST = true;
    vector<string> errors;

    explicit Parser(Lexer& lex, bool ast=true): L(lex), buildAST(ast) { advance(); }

    [[noreturn]] void throwErr(const string& msg) {
        std::ostringstream os; os << "Syntax error at " << t.line << ":" << t.col << ": " << msg
                                  << " (got " << kindName(t.kind) << " \"" << t.lexeme << "\")";
        throw ParseError(os.str());
    }
    void error(const string& msg) {
        std::ostringstream os; os << "Syntax error at " << t.line << ":" << t.col << ": " << msg
                                  << " (got " << kindName(t.kind) << " \"" << t.lexeme << "\")";
        errors.push_back(os.str());
        while (t.kind!=TokenKind::EndOfFile &&
               t.kind!=TokenKind::Separator &&
               t.kind!=TokenKind::KW_end &&
               t.kind!=TokenKind::RParen && t.kind!=TokenKind::RBracket)
        {
            advance();
        }
        if (t.kind==TokenKind::Separator) advance();
    }
    void advance() { t = L.next(); skipSeparators(); }
    bool check(TokenKind k) const { return t.kind==k; }
    bool accept(TokenKind k) { if (t.kind==k){ advance(); return true; } return false; }
    void expect(TokenKind k, const char* what) {
        if (!accept(k)) throwErr(string("expected ")+what);
    }
    void skipSeparators() {
        while (t.kind==TokenKind::Separator) t = L.next();
    }

    unique_ptr<AST> parseProgram() {
        auto root = node(NodeKind::Program);
        while (!check(TokenKind::EndOfFile)) {
            if (isStartOfSimpleDecl()) {
                root->add(parseSimpleDecl());
            } else if (check(TokenKind::KW_routine)) {
                root->add(parseRoutineDecl());
            } else if (isStartOfStatement()) {
                root->add(parseStatement());
            } else {
                error("expected declaration (var/type/routine) or statement");
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
            string name = t.lexeme; int l=t.line, c=t.col;
            expect(TokenKind::Identifier, "identifier after 'var'");
            auto nodeVar = node(NodeKind::VarDecl, name, l, c);
            if (accept(TokenKind::Colon)) {
                nodeVar->add(parseType());
                if (accept(TokenKind::Assign)) {
                    nodeVar->add(parseExpression());
                } else if (accept(TokenKind::KW_is)) {
                    nodeVar->add(parseExpression());
                }
            } else if (accept(TokenKind::Assign)) {
                nodeVar->add(parseExpression());
            } else if (accept(TokenKind::KW_is)) {
                nodeVar->add(parseExpression());
            } else {
                throwErr("expected ':', ':=' or 'is' in variable declaration");
            }
            return nodeVar;
        }
        if (accept(TokenKind::KW_type)) {
            string name = t.lexeme; int l=t.line, c=t.col;
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
            string w = t.lexeme; int l=t.line,c=t.col; advance();
            auto p = node(NodeKind::PrimType, w, l, c);
            return p;
        }
        if (check(TokenKind::KW_array) || check(TokenKind::KW_record)) {
            return parseUserType();
        }
        if (check(TokenKind::Identifier)) {
            string name=t.lexeme; int l=t.line,c=t.col; advance();
            return node(NodeKind::UserType, name, l, c);
        }
        throwErr("expected type");
        return nullptr;
    }

    unique_ptr<AST> parseUserType() {
        if (accept(TokenKind::KW_record)) {
            auto rec = node(NodeKind::RecordType);
            while (!check(TokenKind::KW_end) && !check(TokenKind::EndOfFile)) {
                if (!check(TokenKind::KW_var)) {
                    throwErr("in record: expected 'var' declaration");
                }
                rec->add(parseSimpleDecl());
            }
            expect(TokenKind::KW_end, "end");
            return rec;
        }
        if (accept(TokenKind::KW_array)) {
            auto arr = node(NodeKind::ArrayType);
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
        auto hdr = parseRoutineHeader();
        if (check(TokenKind::KW_is) || check(TokenKind::Arrow)) {
            hdr->add(parseRoutineBody());
        }
        return attach(NodeKind::RoutineDecl, std::move(hdr));
    }

    unique_ptr<AST> parseRoutineHeader() {
        int l=t.line,c=t.col;
        expect(TokenKind::KW_routine, "'routine'");
        string name=t.lexeme; int nl=t.line,nc=t.col;
        expect(TokenKind::Identifier, "routine name");
        auto H = node(NodeKind::RoutineHeader, name, nl,nc);
        expect(TokenKind::LParen, "'('");
        auto params = node(NodeKind::Params);
        if (!check(TokenKind::RParen)) {
            for (;;) {
                string pn=t.lexeme; int pl=t.line,pc=t.col;
                expect(TokenKind::Identifier, "parameter name");
                expect(TokenKind::Colon, "':' in parameter");
                auto P = node(NodeKind::Param, pn, pl,pc);
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
        if (accept(TokenKind::KW_is)) {
            auto B = node(NodeKind::RoutineBodyBlock);
            B->add(parseBody());
            expect(TokenKind::KW_end, "'end' of routine body");
            return B;
        }
        if (accept(TokenKind::Arrow)) {
            auto E = node(NodeKind::RoutineBodyExpr);
            E->add(parseExpression());
            return E;
        }
        throwErr("expected 'is' or '=>' in routine body");
        return nullptr;
    }

    unique_ptr<AST> parseBody() {
        auto B = node(NodeKind::Body);
        while (!check(TokenKind::KW_end) && !check(TokenKind::EndOfFile)) {
            if (isStartOfSimpleDecl()) B->add(parseSimpleDecl());
            else if (isStartOfStatement()) B->add(parseStatement());
            else break;
        }
        return B;
    }

    bool isStartOfStatement() {
        return check(TokenKind::Identifier) || check(TokenKind::KW_while) || check(TokenKind::KW_for) ||
               check(TokenKind::KW_if) || check(TokenKind::KW_print) || check(TokenKind::KW_return);
    }

    unique_ptr<AST> parseStatement() {
        if (check(TokenKind::KW_while)) return parseWhile();
        if (check(TokenKind::KW_for))   return parseFor();
        if (check(TokenKind::KW_if))    return parseIf();
        if (check(TokenKind::KW_print)) return parsePrint();
        if (check(TokenKind::KW_return)) return parseReturn();
        auto mp = parseModifiablePrimaryOrCall(/*asStmt=*/true);
        return mp;
    }

    unique_ptr<AST> parseWhile() {
        int l=t.line,c=t.col;
        expect(TokenKind::KW_while, "'while'");
        auto W = node(NodeKind::While, "while", l,c);
        W->add(parseExpression());
        expect(TokenKind::KW_loop, "'loop'");
        W->add(parseBody());
        expect(TokenKind::KW_end, "'end'");
        return W;
    }

    unique_ptr<AST> parseFor() {
        int l=t.line,c=t.col;
        expect(TokenKind::KW_for, "'for'");
        string var=t.lexeme; int vl=t.line,vc=t.col;
        expect(TokenKind::Identifier,"loop variable");
        expect(TokenKind::KW_in,"'in'");
        auto F = node(NodeKind::For, var, vl,vc);
        auto first = parseExpression();
        if (accept(TokenKind::Range)) {
            auto last = parseExpression();
            auto R = node(NodeKind::Expr, "..");
            R->add(std::move(first)); R->add(std::move(last));
            F->add(std::move(R));
        } else {
            F->add(std::move(first));
        }
        bool rev=false;
        if (accept(TokenKind::KW_reverse)) { rev=true; }
        if (rev) F->add(node(NodeKind::Name,"reverse"));
        expect(TokenKind::KW_loop,"'loop'");
        F->add(parseBody());
        expect(TokenKind::KW_end,"'end'");
        return F;
    }

    unique_ptr<AST> parseIf() {
        int l=t.line,c=t.col;
        expect(TokenKind::KW_if,"'if'");
        auto I = node(NodeKind::If,"if",l,c);
        I->add(parseExpression());
        expect(TokenKind::KW_then,"'then'");
        I->add(parseBody());
        if (accept(TokenKind::KW_else)) {
            I->add(parseBody());
        }
        expect(TokenKind::KW_end,"'end'");
        return I;
    }

    unique_ptr<AST> parsePrint() {
        int l=t.line,c=t.col;
        expect(TokenKind::KW_print,"'print'");
        auto P = node(NodeKind::Print,"print",l,c);
        P->add(parseExpression());
        while (accept(TokenKind::Comma)) P->add(parseExpression());
        return P;
    }

    unique_ptr<AST> parseReturn() {
        int l=t.line,c=t.col;
        expect(TokenKind::KW_return,"'return'");
        auto R = node(NodeKind::ReturnStmt,"return",l,c);
        if (!isStmtTerminator()) {
            R->add(parseExpression());
        }
        return R;
    }

    bool isStmtTerminator() {
        return t.kind==TokenKind::Separator || t.kind==TokenKind::KW_end || t.kind==TokenKind::KW_else
            || t.kind==TokenKind::KW_loop || t.kind==TokenKind::RParen || t.kind==TokenKind::RBracket
            || t.kind==TokenKind::EndOfFile;
    }

    unique_ptr<AST> parseModifiablePrimaryOrCall(bool asStmt) {
        auto lhs = parseModifiablePrimaryCore();
        if (accept(TokenKind::Assign)) {
            auto A = node(NodeKind::Assign);
            A->add(std::move(lhs));
            A->add(parseExpression());
            return A;
        }
        if (lhs->kind==NodeKind::ModPrimary && lhs->kids.size()==1 &&
            lhs->kids[0]->kind==NodeKind::Name && check(TokenKind::LParen))
        {
            auto C = node(NodeKind::Call, lhs->kids[0]->text, lhs->kids[0]->line, lhs->kids[0]->col);
            advance();
            if (!check(TokenKind::RParen)) {
                C->add(parseExpression());
                while (accept(TokenKind::Comma)) C->add(parseExpression());
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
        string name=t.lexeme; int l=t.line,c=t.col;
        expect(TokenKind::Identifier,"identifier");
        auto MP = node(NodeKind::ModPrimary);
        auto base = node(NodeKind::Name, name,l,c);
        MP->add(std::move(base));
        for (;;) {
            if (accept(TokenKind::Dot)) {
                string m=t.lexeme; int ml=t.line,mc=t.col;
                expect(TokenKind::Identifier,"member name");
                MP->add(node(NodeKind::Member,m,ml,mc));
            } else if (accept(TokenKind::LBracket)) {
                auto idx = node(NodeKind::Index);
                idx->add(parseExpression());
                expect(TokenKind::RBracket, "']'");
                MP->add(std::move(idx));
            } else if (accept(TokenKind::Apostrophe)) {
                string attr=t.lexeme; int al=t.line,ac=t.col;
                expect(TokenKind::Identifier,"attribute name");
                MP->add(node(NodeKind::Member,attr,al,ac));
            } else break;
        }
        return MP;
    }

    unique_ptr<AST> parseExpression() {
        auto left = parseRelation();
        while (check(TokenKind::KW_and) || check(TokenKind::KW_or) || check(TokenKind::KW_xor)) {
            string op=t.lexeme; int l=t.line,c=t.col; advance();
            auto E = node(NodeKind::Expr, op, l,c);
            E->add(std::move(left));
            E->add(parseRelation());
            left = std::move(E);
        }
        return left;
    }
    unique_ptr<AST> parseRelation() {
        auto left = parseSimple();
        if (check(TokenKind::LT) || check(TokenKind::LE) || check(TokenKind::GT) ||
            check(TokenKind::GE) || check(TokenKind::EQ) || check(TokenKind::NE))
        {
            string op=t.lexeme; int l=t.line,c=t.col; advance();
            auto R = node(NodeKind::Rel, op, l,c);
            R->add(std::move(left));
            R->add(parseSimple());
            return R;
        }
        return left;
    }
    unique_ptr<AST> parseSimple() {
        auto left = parseFactor();
        while (check(TokenKind::Star) || check(TokenKind::Slash) || check(TokenKind::Percent)) {
            string op=t.lexeme; int l=t.line,c=t.col; advance();
            auto S = node(NodeKind::Simple, op, l,c);
            S->add(std::move(left));
            S->add(parseFactor());
            left = std::move(S);
        }
        return left;
    }
    unique_ptr<AST> parseFactor() {
        auto left = parseSummand();
        while (check(TokenKind::Plus) || check(TokenKind::Minus)) {
            string op=t.lexeme; int l=t.line,c=t.col; advance();
            auto F = node(NodeKind::Factor, op, l,c);
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
        if (accept(TokenKind::KW_not)) {
            auto N = node(NodeKind::Primary, "not", t.line, t.col);
            N->add(parseSummand());
            return N;
        }
        return parsePrimary();
    }

    unique_ptr<AST> parsePrimary() {
        bool havePrefix=false;
        string prefix;
        int pl=0,pc=0;
        if (check(TokenKind::Plus) || check(TokenKind::Minus)) {
            havePrefix=true; prefix=t.lexeme; pl=t.line; pc=t.col; advance();
        }
        if (check(TokenKind::Integer)) {
            string v=t.lexeme; int l=t.line,c=t.col; advance();
            auto P = node(NodeKind::Primary, (havePrefix? prefix+v : v), l,c);
            return P;
        }
        if (check(TokenKind::Real)) {
            string v=t.lexeme; int l=t.line,c=t.col; advance();
            auto P = node(NodeKind::Primary, (havePrefix? prefix+v : v), l,c);
            return P;
        }
        if (check(TokenKind::Boolean)) {
            string v=t.lexeme; int l=t.line,c=t.col; advance();
            auto P = node(NodeKind::Primary, v, l,c);
            return P;
        }
        if (check(TokenKind::LParen)) {
            advance();
            auto E = parseExpression();
            expect(TokenKind::RParen, "')'");
            return E;
        }
        if (havePrefix) throwErr("prefix sign may be used only before numeric literal");

        if (check(TokenKind::Identifier)) {
            auto lhs = parseModifiablePrimaryCore();
            if (lhs->kind==NodeKind::ModPrimary && lhs->kids.size()==1 &&
                lhs->kids[0]->kind==NodeKind::Name && check(TokenKind::LParen))
            {
                auto C = node(NodeKind::Call, lhs->kids[0]->text, lhs->kids[0]->line, lhs->kids[0]->col);
                advance();
                if (!check(TokenKind::RParen)) {
                    C->add(parseExpression());
                    while (accept(TokenKind::Comma)) C->add(parseExpression());
                }
                expect(TokenKind::RParen, "')'");
                return C;
            }
            return lhs;
        }
        throwErr("expected primary");
        return nullptr;
    }

    unique_ptr<AST> node(NodeKind k, string text="", int l=-1, int c=-1) {
        if (!buildAST) return std::make_unique<AST>(k);
        int ln = (l==-1? t.line : l);
        int cn = (c==-1? t.col : c);
        return std::make_unique<AST>(k, std::move(text), ln, cn);
    }
    unique_ptr<AST> attach(NodeKind k, unique_ptr<AST> child) {
        auto n = node(k);
        n->add(std::move(child));
        return n;
    }
};

// ==================== Семантический Анализатор ====================

class SemanticAnalyzer {
private:
    vector<string> errors;
    vector<string> warnings;
    
    // Таблицы символов
    struct Symbol {
        string name;
        string type;  // "integer", "real", "boolean", "routine", etc.
        int line, col;
        bool used = false;
        bool isRoutine = false;
        vector<string> paramTypes;  // для функций
        string returnType;          // для функций
    };
    
    vector<unordered_map<string, Symbol>> scopes;  // стек областей видимости
    int insideRoutineDepth = 0;  // глубина вложенности в функции
    
    // Глобальная мапа использования переменных (сохраняется между фазами)
    unordered_map<string, bool> globalUsage;
    
    // Статистика оптимизаций
    int constantFoldingCount = 0;
    int unreachableCodeCount = 0;
    int ifSimplificationCount = 0;
    int unusedVarCount = 0;
    
public:
    // Главный метод анализа
    void analyze(AST* root) {
        cout << "\n=== Семантический анализ ===" << endl;
        
        // Фаза 1: Проверки (не модифицируют AST)
        cout << "\nФаза 1: Семантические проверки" << endl;
        scopes.clear();
        scopes.push_back({});  // глобальная область видимости
        insideRoutineDepth = 0;
        
        checkDeclarationsAndUsage(root);
        
        if (!errors.empty()) {
            cout << "\n❌ Найдены семантические ошибки:" << endl;
            for (const auto& err : errors) {
                cout << "  " << err << endl;
            }
            return;
        }
        
        cout << "✓ Все декларации корректны" << endl;
        cout << "✓ Все переменные объявлены до использования" << endl;
        cout << "✓ return используется только внутри функций" << endl;
        
        // Фаза 2: Оптимизации (модифицируют AST)
        cout << "\nФаза 2: Оптимизации AST" << endl;
        
        optimizeConstantFolding(root);
        optimizeUnreachableCode(root);
        optimizeIfSimplification(root);
        optimizeUnusedVariables(root);
        
        // Вывод статистики
        printOptimizationStats();
        
        if (!warnings.empty()) {
            cout << "\n⚠️  Предупреждения:" << endl;
            for (const auto& warn : warnings) {
                cout << "  " << warn << endl;
            }
        }
        
        cout << "\n✅ Семантический анализ завершён успешно" << endl;
    }
    
private:
    // ============ ПРОВЕРКА 1: Декларации до использования ============
    void checkDeclarationsAndUsage(AST* node) {
        if (!node) return;
        
        if (node->kind == NodeKind::RoutineDecl) {
            // Регистрируем функцию
            if (node->kids.size() > 0 && node->kids[0]->kind == NodeKind::RoutineHeader) {
                AST* header = node->kids[0].get();
                string routineName = header->text;
                
                Symbol sym;
                sym.name = routineName;
                sym.type = "routine";
                sym.line = header->line;
                sym.col = header->col;
                sym.isRoutine = true;
                
                // Извлекаем типы параметров
                if (header->kids.size() > 0 && header->kids[0]->kind == NodeKind::Params) {
                    for (auto& param : header->kids[0]->kids) {
                        if (param->kind == NodeKind::Param && param->kids.size() > 0) {
                            sym.paramTypes.push_back(getTypeString(param->kids[0].get()));
                        }
                    }
                }
                
                // Извлекаем тип возврата
                if (header->kids.size() > 1 && header->kids[1]->kind != NodeKind::RoutineBodyBlock 
                    && header->kids[1]->kind != NodeKind::RoutineBodyExpr) {
                    sym.returnType = getTypeString(header->kids[1].get());
                }
                
                scopes.back()[routineName] = sym;
            }
            
            // Входим в область видимости функции
            scopes.push_back({});
            insideRoutineDepth++;
            
            // Добавляем параметры в локальную область
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
                            pSym.used = true;  // параметры считаем используемыми
                            scopes.back()[param->text] = pSym;
                        }
                    }
                }
            }
            
            for (auto& kid : node->kids) {
                checkDeclarationsAndUsage(kid.get());
            }
            
            scopes.pop_back();
            insideRoutineDepth--;
            return;
        }
        
        if (node->kind == NodeKind::VarDecl) {
            // Регистрируем переменную
            Symbol sym;
            sym.name = node->text;
            sym.type = node->kids.size() > 0 ? getTypeString(node->kids[0].get()) : "unknown";
            sym.line = node->line;
            sym.col = node->col;
            
            scopes.back()[node->text] = sym;
            
            // Проверяем инициализацию
            for (auto& kid : node->kids) {
                checkDeclarationsAndUsage(kid.get());
            }
            return;
        }
        
        if (node->kind == NodeKind::For) {
            // for создаёт новую область видимости для переменной цикла
            scopes.push_back({});
            
            Symbol loopVar;
            loopVar.name = node->text;
            loopVar.type = "integer";
            loopVar.line = node->line;
            loopVar.col = node->col;
            loopVar.used = true;  // переменная цикла считается используемой
            scopes.back()[node->text] = loopVar;
            
            for (auto& kid : node->kids) {
                checkDeclarationsAndUsage(kid.get());
            }
            
            scopes.pop_back();
            return;
        }
        
        // ПРОВЕРКА 2: return только внутри функций
        if (node->kind == NodeKind::ReturnStmt) {
            if (insideRoutineDepth == 0) {
                addError(node->line, node->col, 
                    "return statement can only be used inside a routine");
            }
        }
        
        // Проверка использования переменной
        if (node->kind == NodeKind::Name) {
            string varName = node->text;
            
            // Ищем переменную в областях видимости (от текущей к глобальной)
            bool found = false;
            for (int i = scopes.size() - 1; i >= 0; i--) {
                if (scopes[i].count(varName)) {
                    scopes[i][varName].used = true;
                    globalUsage[varName] = true;  // Глобально помечаем как использованную
                    found = true;
                    break;
                }
            }
            
            if (!found) {
                addError(node->line, node->col, 
                    "variable '" + varName + "' used before declaration");
            }
        }
        
        // Проверка вызова функции
        if (node->kind == NodeKind::Call) {
            string funcName = node->text;
            
            bool found = false;
            for (int i = scopes.size() - 1; i >= 0; i--) {
                if (scopes[i].count(funcName)) {
                    scopes[i][funcName].used = true;
                    
                    // ПРОВЕРКА 3: Проверка типов аргументов (простая версия)
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
        
        if (node->kind == NodeKind::Assign) {
            // Проверяем левую часть присваивания
            if (node->kids.size() > 0) {
                AST* lhs = node->kids[0].get();
                if (lhs->kind == NodeKind::ModPrimary && lhs->kids.size() > 0) {
                    if (lhs->kids[0]->kind == NodeKind::Name) {
                        string varName = lhs->kids[0]->text;
                        
                        // Проверяем, что переменная объявлена
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
            
            // Проверяем правую часть
            if (node->kids.size() > 1) {
                checkDeclarationsAndUsage(node->kids[1].get());
            }
            return;
        }
        
        // Рекурсивно обрабатываем детей
        for (auto& kid : node->kids) {
            checkDeclarationsAndUsage(kid.get());
        }
    }
    
    // ============ ОПТИМИЗАЦИЯ 1: Свёртка констант ============
    void optimizeConstantFolding(AST* node) {
        if (!node) return;
        
        // Сначала оптимизируем детей
        for (auto& kid : node->kids) {
            optimizeConstantFolding(kid.get());
        }
        
        // Затем пытаемся свернуть текущий узел
        if (node->kind == NodeKind::Factor || node->kind == NodeKind::Simple || 
            node->kind == NodeKind::Rel || node->kind == NodeKind::Expr) {
            
            if (node->kids.size() == 2) {
                AST* left = node->kids[0].get();
                AST* right = node->kids[1].get();
                
                // Проверяем, что оба операнда - числовые константы
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
                        
                        // Операции сравнения
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
                    
                    // Логические операции
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
        
        // Упрощение унарных операций
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
    
    // ============ ОПТИМИЗАЦИЯ 2: Удаление недостижимого кода ============
    void optimizeUnreachableCode(AST* node) {
        if (!node) return;
        
        if (node->kind == NodeKind::Body || node->kind == NodeKind::Program) {
            bool foundReturn = false;
            vector<size_t> toDelete;
            
            for (size_t i = 0; i < node->kids.size(); i++) {
                if (foundReturn) {
                    // Всё после return - недостижимый код
                    toDelete.push_back(i);
                    unreachableCodeCount++;
                } else if (node->kids[i]->kind == NodeKind::ReturnStmt) {
                    foundReturn = true;
                }
            }
            
            // Удаляем недостижимый код (в обратном порядке)
            for (int i = toDelete.size() - 1; i >= 0; i--) {
                node->kids.erase(node->kids.begin() + toDelete[i]);
            }
        }
        
        // Рекурсивно обрабатываем детей
        for (auto& kid : node->kids) {
            optimizeUnreachableCode(kid.get());
        }
    }
    
    // ============ ОПТИМИЗАЦИЯ 3: Упрощение if ============
    void optimizeIfSimplification(AST* node) {
        if (!node) return;
        
        // Сначала оптимизируем детей
        for (auto& kid : node->kids) {
            optimizeIfSimplification(kid.get());
        }
        
        if (node->kind == NodeKind::If && node->kids.size() >= 2) {
            AST* condition = node->kids[0].get();
            
            // Проверяем, является ли условие константой
            if (condition->kind == NodeKind::Primary) {
                if (condition->text == "true") {
                    // if (true) -> заменяем на тело then-ветки
                    AST* thenBody = node->kids[1].get();
                    
                    // Копируем детей then-ветки на место if
                    vector<unique_ptr<AST>> newKids;
                    for (auto& stmt : thenBody->kids) {
                        newKids.push_back(std::move(stmt));
                    }
                    
                    node->kids = std::move(newKids);
                    node->kind = NodeKind::Body;
                    node->text = "";
                    
                    ifSimplificationCount++;
                } else if (condition->text == "false") {
                    // if (false) -> заменяем на тело else-ветки (если есть) или удаляем
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
                        // Нет else-ветки, просто очищаем
                        node->kids.clear();
                        node->kind = NodeKind::Body;
                        node->text = "";
                    }
                    
                    ifSimplificationCount++;
                }
            }
        }
    }
    
    // ============ ОПТИМИЗАЦИЯ 4: Удаление неиспользуемых переменных ============
    void optimizeUnusedVariables(AST* node) {
        if (!node) return;
        
        // Используем информацию, уже собранную в checkDeclarationsAndUsage
        removeUnusedVars(node, 0);
    }
    
    void removeUnusedVars(AST* node, int scopeLevel) {
        if (!node) return;
        
        // Для функций создаём новый уровень области видимости
        int nextLevel = scopeLevel;
        if (node->kind == NodeKind::RoutineDecl || node->kind == NodeKind::For) {
            nextLevel = scopeLevel + 1;
        }
        
        if (node->kind == NodeKind::Body || node->kind == NodeKind::Program) {
            vector<size_t> toDelete;
            
            for (size_t i = 0; i < node->kids.size(); i++) {
                if (node->kids[i]->kind == NodeKind::VarDecl) {
                    string varName = node->kids[i]->text;
                    
                    // Проверяем использование в глобальной мапе
                    bool used = globalUsage.count(varName) && globalUsage[varName];
                    
                    if (!used) {
                        toDelete.push_back(i);
                        unusedVarCount++;
                        addWarning(node->kids[i]->line, node->kids[i]->col,
                            "unused variable '" + varName + "' removed");
                    }
                }
            }
            
            // Удаляем неиспользуемые переменные (в обратном порядке)
            for (int i = toDelete.size() - 1; i >= 0; i--) {
                node->kids.erase(node->kids.begin() + toDelete[i]);
            }
        }
        
        for (auto& kid : node->kids) {
            removeUnusedVars(kid.get(), nextLevel);
        }
    }
    
    // ============ Вспомогательные методы ============
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
        cout << "\n📊 Статистика оптимизаций:" << endl;
        cout << "  • Свёртка констант: " << constantFoldingCount << " операций" << endl;
        cout << "  • Удалено недостижимого кода: " << unreachableCodeCount << " блоков" << endl;
        cout << "  • Упрощение if: " << ifSimplificationCount << " конструкций" << endl;
        cout << "  • Удалено неиспользуемых переменных: " << unusedVarCount << endl;
        
        int total = constantFoldingCount + unreachableCodeCount + ifSimplificationCount + unusedVarCount;
        cout << "  Всего оптимизаций: " << total << endl;
    }
};

// ==================== Main Driver ====================
int main(int argc, char** argv) {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    if (argc < 2) {
        cerr << "Usage: " << argv[0] << " <source.pi>\n";
        return 1;
    }
    
    ifstream in(argv[1], ios::binary);
    if (!in) { 
        cerr << "Cannot open file: " << argv[1] << "\n"; 
        return 1; 
    }
    
    string src((istreambuf_iterator<char>(in)), istreambuf_iterator<char>());
    
    try {
        cout << "Анализируется файл: " << argv[1] << endl;
        
        // Парсинг
        Lexer lex(std::move(src));
        Parser p(lex, /*buildAST*/true);
        auto tree = p.parseProgram();
        
        if (!p.errors.empty()) {
            cerr << "\n❌ Синтаксические ошибки:\n";
            for (auto& e: p.errors) cerr << "  " << e << "\n";
            return 2;
        }
        
        cout << "✓ Синтаксический анализ пройден" << endl;
        
        // Семантический анализ
        SemanticAnalyzer analyzer;
        analyzer.analyze(tree.get());
        
        return 0;
        
    } catch (const ParseError& e) {
        cerr << "\n❌ " << e.what() << "\n";
        return 2;
    } catch (const exception& e) {
        cerr << "\n❌ Ошибка: " << e.what() << "\n";
        return 3;
    }
}

