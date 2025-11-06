

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

enum class TokenKind
{
    Identifier,
    Integer,
    Real,
    Boolean,
    KW_var,
    KW_type,
    KW_integer,
    KW_real,
    KW_boolean,
    KW_record,
    KW_array,
    KW_is,
    KW_end,
    KW_while,
    KW_loop,
    KW_for,
    KW_in,
    KW_reverse,
    KW_if,
    KW_then,
    KW_else,
    KW_print,
    KW_routine,
    KW_and,
    KW_or,
    KW_xor,
    KW_not,
    KW_return,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Dot,
    Range,
    Assign,
    Arrow,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    LT,
    LE,
    GT,
    GE,
    EQ,
    NE,
    Apostrophe,
    Separator,
    EndOfFile,
    Error
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
        if (eof())
            return '\0';
        char c = src[i++];
        if (c == '\n')
        {
            line++;
            col = 1;
        }
        else
        {
            col++;
        }
        return c;
    }
    bool match(char expected)
    {
        if (peek() == expected)
        {
            advance();
            return true;
        }
        return false;
    }

    static bool isIdentStart(char c)
    {
        return std::isalpha(static_cast<unsigned char>(c)) || c == '_';
    }
    static bool isIdentCont(char c)
    {
        return std::isalnum(static_cast<unsigned char>(c)) || c == '_';
    }

    void skipWhitespaceAndComments()
    {
        for (;;)
        {
            while (true)
            {
                char c = peek();
                if (c == ' ' || c == '\t' || c == '\r')
                {
                    advance();
                }
                else
                    break;
            }
            if (peek() == '/' && peek(1) == '/')
            {
                while (peek() != '\n' && !eof())
                    advance();
                continue;
            }
            if (peek() == '/' && peek(1) == '*')
            {
                int start_line = line, start_col = col;
                advance();
                advance();
                bool closed = false;
                while (!eof())
                {
                    if (peek() == '*' && peek(1) == '/')
                    {
                        advance();
                        advance();
                        closed = true;
                        break;
                    }
                    advance();
                }
                if (!closed)
                {
                    hasPendingError = true;
                    pendingErrorToken.kind = TokenKind::Error;
                    pendingErrorToken.lexeme = "unterminated block comment";
                    pendingErrorToken.line = start_line;
                    pendingErrorToken.col = start_col;
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
            {"var", TokenKind::KW_var},
            {"type", TokenKind::KW_type},
            {"integer", TokenKind::KW_integer},
            {"real", TokenKind::KW_real},
            {"boolean", TokenKind::KW_boolean},
            {"record", TokenKind::KW_record},
            {"array", TokenKind::KW_array},
            {"is", TokenKind::KW_is},
            {"end", TokenKind::KW_end},
            {"while", TokenKind::KW_while},
            {"loop", TokenKind::KW_loop},
            {"for", TokenKind::KW_for},
            {"in", TokenKind::KW_in},
            {"reverse", TokenKind::KW_reverse},
            {"if", TokenKind::KW_if},
            {"then", TokenKind::KW_then},
            {"else", TokenKind::KW_else},
            {"print", TokenKind::KW_print},
            {"routine", TokenKind::KW_routine},
            {"and", TokenKind::KW_and},
            {"or", TokenKind::KW_or},
            {"xor", TokenKind::KW_xor},
            {"not", TokenKind::KW_not},
            {"return", TokenKind::KW_return},
        };
        isBoolLit = false;
        boolVal = false;
        if (s == "true")
        {
            isBoolLit = true;
            boolVal = true;
            return TokenKind::Boolean;
        }
        if (s == "false")
        {
            isBoolLit = true;
            boolVal = false;
            return TokenKind::Boolean;
        }
        auto it = kw.find(s);
        return (it != kw.end()) ? it->second : TokenKind::Identifier;
    }

    Token make(TokenKind k, const string &lex, int l, int c)
    {
        Token t;
        t.kind = k;
        t.lexeme = lex;
        t.line = l;
        t.col = c;
        return t;
    }

    Token next()
    {
        if (hasPendingError)
        {
            hasPendingError = false;
            return pendingErrorToken;
        }

        skipWhitespaceAndComments();

        if (hasPendingError)
        {
            hasPendingError = false;
            return pendingErrorToken;
        }

        int tl = line, tc = col;
        if (eof())
            return make(TokenKind::EndOfFile, "<eof>", tl, tc);

        char c = advance();

        if (c == '\n')
        {
            return make(TokenKind::Separator, "\\n", tl, tc);
        }
        if (isIdentStart(c))
        {
            string s(1, c);
            while (isIdentCont(peek()))
                s.push_back(advance());
            bool isBool = false, bval = false;
            TokenKind k = keywordKind(s, isBool, bval);
            Token t = make(k, s, tl, tc);
            if (k == TokenKind::Boolean)
                t.boolValue = bval;
            return t;
        }

        if (std::isdigit(static_cast<unsigned char>(c)))
        {
            string s(1, c);
            while (std::isdigit(static_cast<unsigned char>(peek())))
                s.push_back(advance());

            if (peek() == '.' && peek(1) != '.' && std::isdigit(static_cast<unsigned char>(peek(1))))
            {
                s.push_back(advance()); // '.'
                while (std::isdigit(static_cast<unsigned char>(peek())))
                    s.push_back(advance());
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
        case ':':
            if (match('='))
                return make(TokenKind::Assign, ":=", tl, tc);
            return make(TokenKind::Colon, ":", tl, tc);
        case '.':
            if (match('.'))
                return make(TokenKind::Range, "..", tl, tc);
            return make(TokenKind::Dot, ".", tl, tc);
        case '=':
            if (match('>'))
                return make(TokenKind::Arrow, "=>", tl, tc);
            return make(TokenKind::EQ, "=", tl, tc);
        case '<':
            if (match('='))
                return make(TokenKind::LE, "<=", tl, tc);
            return make(TokenKind::LT, "<", tl, tc);
        case '>':
            if (match('='))
                return make(TokenKind::GE, ">=", tl, tc);
            return make(TokenKind::GT, ">", tl, tc);
        case '/':
            if (match('='))
                return make(TokenKind::NE, "/=", tl, tc);
            return make(TokenKind::Slash, "/", tl, tc);
        case '\'':
            return make(TokenKind::Apostrophe, "'", tl, tc);
        }
        switch (c)
        {
        case '+':
            return make(TokenKind::Plus, "+", tl, tc);
        case '-':
            return make(TokenKind::Minus, "-", tl, tc);
        case '*':
            return make(TokenKind::Star, "*", tl, tc);
        case '%':
            return make(TokenKind::Percent, "%", tl, tc);
        case '(':
            return make(TokenKind::LParen, "(", tl, tc);
        case ')':
            return make(TokenKind::RParen, ")", tl, tc);
        case '[':
            return make(TokenKind::LBracket, "[", tl, tc);
        case ']':
            return make(TokenKind::RBracket, "]", tl, tc);
        case ',':
            return make(TokenKind::Comma, ",", tl, tc);
        case ';':
            return make(TokenKind::Separator, ";", tl, tc);
        }

        return make(TokenKind::Error, string(1, c), tl, tc);
    }
};

string tokenKindName(TokenKind k)
{
    switch (k)
    {
#define C(x)           \
    case TokenKind::x: \
        return #x;
        C(Identifier)
        C(Integer)
        C(Real)
        C(Boolean)
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

struct ParseError : std::runtime_error
{
    using std::runtime_error::runtime_error;
};

enum class NodeKind
{
    Program,
    VarDecl,
    TypeDecl,
    RecordType,
    ArrayType,
    UserType,
    PrimType,
    RoutineDecl,
    RoutineHeader,
    RoutineBodyBlock,
    RoutineBodyExpr,
    Params,
    Param,
    Body,
    StmtList,
    Assign,
    While,
    For,
    If,
    Print,
    Call,
    ReturnStmt,
    Expr,
    Rel,
    Simple,
    Factor,
    Summand,
    Primary,
    ModPrimary,
    Name,
    Index,
    Member
};

struct AST
{
    NodeKind kind;
    string text;
    vector<unique_ptr<AST>> kids;
    int line = 1, col = 1;
    AST(NodeKind k, string t = "", int l = 1, int c = 1) : kind(k), text(std::move(t)), line(l), col(c) {}
    AST *add(unique_ptr<AST> ch)
    {
        kids.emplace_back(std::move(ch));
        return kids.back().get();
    }
};

// helper to create node with exact token position
unique_ptr<AST> nodeAt(NodeKind k, const Token &tok, string text = "")
{
    return std::make_unique<AST>(k, std::move(text), tok.line, tok.col);
}

struct Parser
{
    Lexer &L;
    Token t; // current token
    bool buildAST = true;
    vector<string> errors;

    explicit Parser(Lexer &lex, bool ast = true) : L(lex), buildAST(ast) { advance(); }

    [[noreturn]] void throwErr(const string &msg)
    {
        std::ostringstream os;
        os << "Syntax error at " << t.line << ":" << t.col << ": " << msg
           << " (got " << tokenKindName(t.kind) << " \"" << t.lexeme << "\")";
        throw ParseError(os.str());
    }
    void error(const string &msg)
    {
        std::ostringstream os;
        os << "Syntax error at " << t.line << ":" << t.col << ": " << msg
           << " (got " << tokenKindName(t.kind) << " \"" << t.lexeme << "\")";
        errors.push_back(os.str());
        while (t.kind != TokenKind::EndOfFile &&
               t.kind != TokenKind::Separator &&
               t.kind != TokenKind::KW_end &&
               t.kind != TokenKind::RParen && t.kind != TokenKind::RBracket)
        {
            advance();
        }
        if (t.kind == TokenKind::Separator)
            advance();
    }
    // take next token from lexer
    void advance()
    {
        t = L.next();
        skipSeparators();
    }
    bool check(TokenKind k) const { return t.kind == k; }
    bool accept(TokenKind k)
    {
        if (t.kind == k)
        {
            advance();
            return true;
        }
        return false;
    }
    void expect(TokenKind k, const char *what)
    {
        if (!accept(k))
            throwErr(string("expected ") + what);
    }
    void skipSeparators()
    {
        while (t.kind == TokenKind::Separator)
            t = L.next();
    }

    // ---- entry
    unique_ptr<AST> parseProgram()
    {
        auto root = node(NodeKind::Program);
        while (!check(TokenKind::EndOfFile))
        {
            if (isStartOfSimpleDecl())
            {
                root->add(parseSimpleDecl());
            }
            else if (check(TokenKind::KW_routine))
            {
                root->add(parseRoutineDecl());
            }
            else
            {
                error("expected declaration (var/type/routine)");
                if (check(TokenKind::EndOfFile))
                    break;
                advance();
            }
        }
        return root;
    }

    // --- declarations
    bool isStartOfSimpleDecl()
    {
        return check(TokenKind::KW_var) || check(TokenKind::KW_type);
    }

    unique_ptr<AST> parseSimpleDecl()
    {
        if (accept(TokenKind::KW_var))
        {
            // var Identifier : Type [ is Expression ]
            // | var Identifier is Expression
            string name = t.lexeme;
            int l = t.line, c = t.col;
            expect(TokenKind::Identifier, "identifier after 'var'");
            auto nodeVar = node(NodeKind::VarDecl, name, l, c);
            if (accept(TokenKind::Colon))
            {
                nodeVar->add(parseType());
                if (accept(TokenKind::KW_is))
                {
                    nodeVar->add(parseExpression());
                }
            }
            else if (accept(TokenKind::KW_is))
            {
                nodeVar->add(parseExpression());
            }
            else
            {
                throwErr("expected ':' or 'is' in variable declaration");
            }
            return nodeVar;
        }
        if (accept(TokenKind::KW_type))
        {
            // TypeDeclaration : type Identifier is Type
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

    unique_ptr<AST> parseType()
    {
        // Type : PrimitiveType | UserType | Identifier
        if (check(TokenKind::KW_integer) || check(TokenKind::KW_real) || check(TokenKind::KW_boolean))
        {
            string w = t.lexeme;
            int l = t.line, c = t.col;
            advance();
            auto p = node(NodeKind::PrimType, w, l, c);
            return p;
        }
        if (check(TokenKind::KW_array) || check(TokenKind::KW_record))
        {
            return parseUserType();
        }
        if (check(TokenKind::Identifier))
        {
            string name = t.lexeme;
            int l = t.line, c = t.col;
            advance();
            return node(NodeKind::UserType, name, l, c);
        }
        throwErr("expected type");
        return nullptr;
    }

    unique_ptr<AST> parseUserType()
    {
        if (check(TokenKind::KW_record))
        {
            // RecordType : record { VariableDeclaration } end
            Token recTok = t;
            advance();
            auto rec = nodeAt(NodeKind::RecordType, recTok);
            while (!check(TokenKind::KW_end) && !check(TokenKind::EndOfFile))
            {
                // inside record: only variable declarations
                if (!check(TokenKind::KW_var))
                {
                    throwErr("in record: expected 'var' declaration");
                }
                rec->add(parseSimpleDecl());
            }
            expect(TokenKind::KW_end, "end");
            return rec;
        }
        if (check(TokenKind::KW_array))
        {
            // ArrayType : array [ [ Expression ] ] Type
            Token arrTok = t;
            advance();
            auto arr = nodeAt(NodeKind::ArrayType, arrTok);
            expect(TokenKind::LBracket, "'[' after 'array'");
            if (check(TokenKind::RBracket))
            {
                advance(); // sizeless []
            }
            else
            {
                // [ Expression ]
                arr->add(parseExpression());
                expect(TokenKind::RBracket, "']' after array size");
            }
            arr->add(parseType());
            return arr;
        }
        throwErr("expected 'record' or 'array'");
        return nullptr;
    }

    // --- routines
    unique_ptr<AST> parseRoutineDecl()
    {
        // RoutineDeclaration : RoutineHeader [ RoutineBody ]
        if (!check(TokenKind::KW_routine))
            throwErr("expected 'routine'");
        Token routTok = t;
        auto hdr = parseRoutineHeader(); // consumes 'routine' and header
        if (check(TokenKind::KW_is) || check(TokenKind::Arrow))
        {
            hdr->add(parseRoutineBody());
        }
        auto decl = nodeAt(NodeKind::RoutineDecl, routTok);
        decl->add(std::move(hdr));
        return decl;
    }

    unique_ptr<AST> parseRoutineHeader()
    {
        // routine Identifier ( Parameters ) [ : Type ]
        Token routineTok = t;
        expect(TokenKind::KW_routine, "'routine'");
        Token nameTok = t;
        expect(TokenKind::Identifier, "routine name");
        auto H = nodeAt(NodeKind::RoutineHeader, nameTok, nameTok.lexeme);
        expect(TokenKind::LParen, "'('");
        auto params = node(NodeKind::Params);
        if (!check(TokenKind::RParen))
        {
            for (;;)
            {
                string pn = t.lexeme;
                int pl = t.line, pc = t.col;
                expect(TokenKind::Identifier, "parameter name");
                expect(TokenKind::Colon, "':' in parameter");
                auto P = node(NodeKind::Param, pn, pl, pc);
                P->add(parseType());
                params->add(std::move(P));
                if (!accept(TokenKind::Comma))
                    break;
            }
        }
        expect(TokenKind::RParen, "')'");
        H->add(std::move(params));
        if (accept(TokenKind::Colon))
        {
            H->add(parseType()); // return type
        }
        return H;
    }

    unique_ptr<AST> parseRoutineBody()
    {
        // RoutineBody : is Body end | => Expression
        if (check(TokenKind::KW_is))
        {
            Token tok = t;
            advance(); // 'is'
            auto B = nodeAt(NodeKind::RoutineBodyBlock, tok);
            B->add(parseBody());
            expect(TokenKind::KW_end, "'end' of routine body");
            return B;
        }
        if (check(TokenKind::Arrow))
        {
            Token tok = t;
            advance(); // '=>'
            auto E = nodeAt(NodeKind::RoutineBodyExpr, tok);
            E->add(parseExpression());
            return E;
        }
        throwErr("expected 'is' or '=>' in routine body");
        return nullptr;
    }

    unique_ptr<AST> parseBody()
    {
        auto B = node(NodeKind::Body);
        while (!check(TokenKind::KW_end) && !check(TokenKind::EndOfFile))
        {
            if (isStartOfSimpleDecl())
                B->add(parseSimpleDecl());
            else if (isStartOfStatement())
                B->add(parseStatement());
            else
                break;
        }
        return B;
    }

    // --- statements
    bool isStartOfStatement()
    {
        return check(TokenKind::Identifier) || check(TokenKind::KW_while) || check(TokenKind::KW_for) ||
               check(TokenKind::KW_if) || check(TokenKind::KW_print) || check(TokenKind::KW_return);
    }

    unique_ptr<AST> parseStatement()
    {
        if (check(TokenKind::KW_while))
            return parseWhile();
        if (check(TokenKind::KW_for))
            return parseFor();
        if (check(TokenKind::KW_if))
            return parseIf();
        if (check(TokenKind::KW_print))
            return parsePrint();
        if (check(TokenKind::KW_return))
            return parseReturn();
        auto mp = parseModifiablePrimaryOrCall(/*asStmt=*/true);
        return mp;
    }

    unique_ptr<AST> parseWhile()
    {
        // while Expression loop Body end
        Token wTok = t;
        expect(TokenKind::KW_while, "'while'");
        auto W = nodeAt(NodeKind::While, wTok, "while");
        W->add(parseExpression());
        expect(TokenKind::KW_loop, "'loop'");
        W->add(parseBody());
        expect(TokenKind::KW_end, "'end'");
        return W;
    }

    unique_ptr<AST> parseFor()
    {
        // for Identifier in Range [ reverse ] loop Body end
        Token fTok = t;
        expect(TokenKind::KW_for, "'for'");
        Token nameTok = t;
        expect(TokenKind::Identifier, "loop variable");
        expect(TokenKind::KW_in, "'in'");
        auto F = nodeAt(NodeKind::For, nameTok, nameTok.lexeme);

        // Range : Expression [ .. Expression ]
        auto first = parseExpression();
        if (accept(TokenKind::Range))
        {
            auto last = parseExpression();
            auto R = node(NodeKind::Expr, "..");
            R->add(std::move(first));
            R->add(std::move(last));
            F->add(std::move(R));
        }
        else
        {
            F->add(std::move(first)); // single expr (array iteration)
        }

        if (accept(TokenKind::KW_reverse))
        {
            F->add(node(NodeKind::Name, "reverse"));
        }
        expect(TokenKind::KW_loop, "'loop'");
        F->add(parseBody());
        expect(TokenKind::KW_end, "'end'");
        return F;
    }

    unique_ptr<AST> parseIf()
    {
        // if Expression then Body [ else Body ] end
        Token ifTok = t;
        expect(TokenKind::KW_if, "'if'");
        auto I = nodeAt(NodeKind::If, ifTok, "if");
        I->add(parseExpression());
        expect(TokenKind::KW_then, "'then'");
        I->add(parseBody());
        if (accept(TokenKind::KW_else))
        {
            I->add(parseBody());
        }
        expect(TokenKind::KW_end, "'end'");
        return I;
    }

    unique_ptr<AST> parsePrint()
    {
        // print Expression { , Expression }
        Token pTok = t;
        expect(TokenKind::KW_print, "'print'");
        auto P = nodeAt(NodeKind::Print, pTok, "print");
        P->add(parseExpression());
        while (accept(TokenKind::Comma))
            P->add(parseExpression());
        return P;
    }

    unique_ptr<AST> parseReturn()
    {
        // return [ Expression ]
        Token rTok = t;
        expect(TokenKind::KW_return, "'return'");
        auto R = nodeAt(NodeKind::ReturnStmt, rTok, "return");
        if (!isStmtTerminator())
        {
            R->add(parseExpression());
        }
        return R;
    }

    bool isStmtTerminator()
    {
        return t.kind == TokenKind::Separator || t.kind == TokenKind::KW_end || t.kind == TokenKind::KW_else || t.kind == TokenKind::KW_loop || t.kind == TokenKind::RParen || t.kind == TokenKind::RBracket || t.kind == TokenKind::EndOfFile;
    }

    // Distinguish Assignment vs Call when starting from Identifier/ModPrimary:
    unique_ptr<AST> parseModifiablePrimaryOrCall(bool asStmt)
    {
        auto lhs = parseModifiablePrimaryCore();
        if (check(TokenKind::Assign))
        {
            Token assignTok = t;
            advance(); // consume ':='
            auto A = nodeAt(NodeKind::Assign, assignTok);
            A->add(std::move(lhs));
            A->add(parseExpression());
            return A;
        }
        if (lhs->kind == NodeKind::ModPrimary && lhs->kids.size() == 1 &&
            lhs->kids[0]->kind == NodeKind::Name && check(TokenKind::LParen))
        {
            auto C = node(NodeKind::Call, lhs->kids[0]->text, lhs->kids[0]->line, lhs->kids[0]->col);
            advance(); // '('
            if (!check(TokenKind::RParen))
            {
                C->add(parseExpression());
                while (accept(TokenKind::Comma))
                    C->add(parseExpression());
            }
            expect(TokenKind::RParen, "')' after arguments");
            return C;
        }
        if (asStmt)
        {
            throwErr("expected assignment ':=' or routine call");
        }
        return lhs;
    }

    unique_ptr<AST> parseModifiablePrimaryCore()
    {
        // Identifier { . Identifier | [ Expression ] | ' Attribute }
        string name = t.lexeme;
        int l = t.line, c = t.col;
        expect(TokenKind::Identifier, "identifier");
        auto MP = node(NodeKind::ModPrimary);
        auto base = node(NodeKind::Name, name, l, c);
        MP->add(std::move(base));
        for (;;)
        {
            if (accept(TokenKind::Dot))
            {
                string m = t.lexeme;
                int ml = t.line, mc = t.col;
                expect(TokenKind::Identifier, "member name");
                MP->add(node(NodeKind::Member, m, ml, mc));
            }
            else if (check(TokenKind::LBracket))
            {
                Token lb = t;
                advance(); // '['
                auto idx = nodeAt(NodeKind::Index, lb);
                idx->add(parseExpression());
                expect(TokenKind::RBracket, "']'");
                MP->add(std::move(idx));
            }
            else if (accept(TokenKind::Apostrophe))
            {
                string attr = t.lexeme;
                int al = t.line, ac = t.col;
                expect(TokenKind::Identifier, "attribute name");
                MP->add(node(NodeKind::Member, attr, al, ac));
            }
            else
                break;
        }
        return MP;
    }

    // --- expressions
    unique_ptr<AST> parseExpression()
    { // Relation { (and|or|xor) Relation }
        auto left = parseRelation();
        while (check(TokenKind::KW_and) || check(TokenKind::KW_or) || check(TokenKind::KW_xor))
        {
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
    unique_ptr<AST> parseRelation()
    { // Simple [ (< | <= | > | >= | = | /=) Simple ]
        auto left = parseSimple();
        if (check(TokenKind::LT) || check(TokenKind::LE) || check(TokenKind::GT) ||
            check(TokenKind::GE) || check(TokenKind::EQ) || check(TokenKind::NE))
        {
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
    unique_ptr<AST> parseSimple()
    { // Factor { (* | / | %) Factor }
        auto left = parseFactor();
        while (check(TokenKind::Star) || check(TokenKind::Slash) || check(TokenKind::Percent))
        {
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
    unique_ptr<AST> parseFactor()
    { // Summand { (+ | -) Summand }
        auto left = parseSummand();
        while (check(TokenKind::Plus) || check(TokenKind::Minus))
        {
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
    unique_ptr<AST> parseSummand()
    { // Primary | ( Expression ) | not Summand
        if (accept(TokenKind::LParen))
        {
            auto E = parseExpression();
            expect(TokenKind::RParen, "')'");
            return E;
        }
        if (check(TokenKind::KW_not))
        {
            Token nt = t;
            advance(); // 'not'
            auto N = nodeAt(NodeKind::Primary, nt, "not");
            N->add(parseSummand());
            return N;
        }
        return parsePrimary();
    }

    unique_ptr<AST> parsePrimary()
    {
        // [ Sign ] IntegerLiteral | [ Sign ] RealLiteral | true | false
        // | ModifiablePrimary | RoutineCall | ( Expression )
        bool havePrefix = false;
        string prefix;
        int pl = 0, pc = 0;
        if (check(TokenKind::Plus) || check(TokenKind::Minus))
        {
            havePrefix = true;
            prefix = t.lexeme;
            pl = t.line;
            pc = t.col;
            advance();
        }
        if (check(TokenKind::Integer))
        {
            string v = t.lexeme;
            int l = t.line, c = t.col;
            advance();
            auto P = node(NodeKind::Primary, (havePrefix ? prefix + v : v), l, c);
            return P;
        }
        if (check(TokenKind::Real))
        {
            string v = t.lexeme;
            int l = t.line, c = t.col;
            advance();
            auto P = node(NodeKind::Primary, (havePrefix ? prefix + v : v), l, c);
            return P;
        }
        if (check(TokenKind::Boolean))
        {
            string v = t.lexeme;
            int l = t.line, c = t.col;
            advance();
            auto P = node(NodeKind::Primary, v, l, c);
            return P;
        }
        if (check(TokenKind::LParen))
        {
            advance(); // '('
            auto E = parseExpression();
            expect(TokenKind::RParen, "')'");
            return E;
        }
        if (havePrefix)
            throwErr("prefix sign may be used only before numeric literal");

        if (check(TokenKind::Identifier))
        {
            auto lhs = parseModifiablePrimaryCore();
            if (lhs->kind == NodeKind::ModPrimary && lhs->kids.size() == 1 &&
                lhs->kids[0]->kind == NodeKind::Name && check(TokenKind::LParen))
            {
                auto C = node(NodeKind::Call, lhs->kids[0]->text, lhs->kids[0]->line, lhs->kids[0]->col);
                advance(); // '('
                if (!check(TokenKind::RParen))
                {
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

    // --- utilities
    unique_ptr<AST> node(NodeKind k, string text = "", int l = -1, int c = -1)
    {
        if (!buildAST)
            return std::make_unique<AST>(k);
        int ln = (l == -1 ? t.line : l);
        int cn = (c == -1 ? t.col : c);
        return std::make_unique<AST>(k, std::move(text), ln, cn);
    }
    unique_ptr<AST> attach(NodeKind k, unique_ptr<AST> child)
    {
        auto n = node(k);
        n->add(std::move(child));
        return n;
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
        vector<string> paramTypes;  // for func
        string returnType;          // for func
    };
    
    vector<unordered_map<string, Symbol>> scopes;  // stack of visibility
    int insideRoutineDepth = 0;  // depth of nested routines
    
    // table of symbols between traversals
    unordered_map<string, bool> globalUsage;
    
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
    
    // Stats of optimizing
    int constantFoldingCount = 0;
    int unreachableCodeCount = 0;
    int ifSimplificationCount = 0;
    int unusedVarCount = 0;
    
public:
    // Main method od analysis
    bool analyze(AST* root) {
        
        // Phase 1: Checks (without modifying AST)
        scopes.clear();
        scopes.push_back({});  // global scope
        insideRoutineDepth = 0;
        
        rootAST = root;
        buildTypeTable(root);
        checkDeclarationsAndUsage(root);
        checkSizelessArrays(root); 

        if (!errors.empty()) {
            cout << "\nSemantic errors:" << endl;
            for (const auto& err : errors) {
                cout << "  " << err << endl;
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
            cout << "\n Warnings:" << endl;
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
                // Don't expand here - keep the type name for proper type checking
                // Expansion will happen when needed (e.g., for arrays to get arrayLen)
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
            // boolean := real is illegal according to spec
        }
        
        // For arrays: check element type compatibility recursively
        if (target.kind == "array" && source.kind == "array") {
            TypeInfo targetElem, sourceElem;
            targetElem.kind = target.elemKind;
            sourceElem.kind = source.elemKind;
            
            // Expand user-defined types for element types
            auto targetElemIt = typeDefs.find(targetElem.kind);
            if (targetElemIt != typeDefs.end()) {
                targetElem = targetElemIt->second;
            }
            auto sourceElemIt = typeDefs.find(sourceElem.kind);
            if (sourceElemIt != typeDefs.end()) {
                sourceElem = sourceElemIt->second;
            }
            
            // Check if lengths are compatible (sizeless arrays can accept any size)
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
            // Check if target is a record type (named or inline)
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
                    // Both are records - check if they have compatible fields
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
        
        // Collect field types from target record
        unordered_map<string, TypeInfo> targetFields;
        for (auto& child : targetRecordType->kids) {
            if (child->kind == NodeKind::VarDecl && !child->kids.empty()) {
                targetFields[child->text] = typeFromTypeNode(child->kids[0].get());
            }
        }
        
        // Collect field types from source record
        unordered_map<string, TypeInfo> sourceFields;
        for (auto& child : sourceRecordType->kids) {
            if (child->kind == NodeKind::VarDecl && !child->kids.empty()) {
                sourceFields[child->text] = typeFromTypeNode(child->kids[0].get());
            }
        }
        
        // Check if all target fields have compatible source fields
        for (const auto& targetField : targetFields) {
            auto sourceIt = sourceFields.find(targetField.first);
            if (sourceIt == sourceFields.end()) {
                return false; // Field missing in source
            }
            if (!isCompatibleType(targetField.second, sourceIt->second)) {
                return false; // Field types incompatible
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
                        // For arrays, expand to get arrayLen and elemKind
                        auto td = typeDefs.find(ti.kind);
                        if (td != typeDefs.end() && td->second.kind == "array") {
                            ti = td->second;
                        }
                        // For records and other types, keep the type name for proper type checking
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
                                // Don't expand user-defined types here - keep the type name
                                // This allows proper type checking for named types like Point
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
            // Register function
            if (node->kids.size() > 0 && node->kids[0]->kind == NodeKind::RoutineHeader) {
                AST* header = node->kids[0].get();
                string routineName = header->text;
                
                Symbol sym;
                sym.name = routineName;
                sym.type = "routine";
                sym.line = header->line;
                sym.col = header->col;
                sym.isRoutine = true;
                
                // Extract data types
                if (header->kids.size() > 0 && header->kids[0]->kind == NodeKind::Params) {
                    for (auto& param : header->kids[0]->kids) {
                        if (param->kind == NodeKind::Param && param->kids.size() > 0) {
                            sym.paramTypes.push_back(getTypeString(param->kids[0].get()));
                        }
                    }
                }
                
                // Extract return type
                if (header->kids.size() > 1 && header->kids[1]->kind != NodeKind::RoutineBodyBlock 
                    && header->kids[1]->kind != NodeKind::RoutineBodyExpr) {
                    sym.returnType = getTypeString(header->kids[1].get());
                }
                
                scopes.back()[routineName] = sym;
                
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
            
            // Extract parameters
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
            //Check if function have a return or =>
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
        //Variable declaration
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
            
            for (auto& kid : node->kids) {
                checkDeclarationsAndUsage(kid.get());
            }
            
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
                    //TODO: if func call - save, other constants values - rm
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
const char *nodeKindName(NodeKind k)
{
    switch (k)
    {
#define C(x)          \
    case NodeKind::x: \
        return #x;
        C(Program)
        C(VarDecl)
        C(TypeDecl)
        C(RecordType)
        C(ArrayType) C(UserType) C(PrimType)
            C(RoutineDecl) C(RoutineHeader) C(RoutineBodyBlock) C(RoutineBodyExpr) C(Params) C(Param)
                C(Body) C(StmtList) C(Assign) C(While) C(For) C(If) C(Print) C(Call) C(ReturnStmt)
                    C(Expr) C(Rel) C(Simple) C(Factor) C(Summand) C(Primary) C(ModPrimary) C(Name) C(Index) C(Member)
#undef C
    }
    return "?";
}
void printAST(const AST *n, int indent = 0)
{
    if (!n)
        return;
    std::string pad(indent, ' ');
    std::cout << pad << nodeKindName(n->kind);
    if (!n->text.empty())
    {
        std::cout << "  \"" << n->text << "\"";
    }
    std::cout << "  @" << n->line << ":" << n->col << "\n";
    for (auto const &ch : n->kids)
    {
        printAST(ch.get(), indent + 2);
    }
}

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
        
        // Parsing
        Lexer lex(std::move(src));
        Parser p(lex, true);
        auto tree = p.parseProgram();
        
        if (!p.errors.empty()) {
            cerr << "\n Syntax errors:\n";
            for (auto& e: p.errors) cerr << "  " << e << "\n";
            return 2;
        }
        cout << "Syntax checks passed " << endl;

            // cout << "\nAST \n";
            // printAST(tree.get());

        // Semantic analysis
        SemanticAnalyzer analyzer;
        bool isValid = analyzer.analyze(tree.get());
        if (isValid){
            cout << "\nUpdated AST \n";
            printAST(tree.get());
        }


        return 0;
        
    } catch (const ParseError& e) {
        cerr << "\n " << e.what() << "\n";
        return 2;
    } catch (const exception& e) {
        cerr << "\n Error: " << e.what() << "\n";
        return 3;
    }
}

