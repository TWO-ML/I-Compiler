// parser.cpp — hand-written recursive-descent
// Uses the provided Lexer/Token/TokenKind from lexer.cpp
// could be run using ./run.sh

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <memory>
#include <sstream>
#include <unordered_map>
#include <utility>
#include <cctype>
#include <cstdlib>
#include <iterator>

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
        C(RecordType) C(ArrayType) C(UserType) C(PrimType)
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

// --- driver main
int main(int argc, char **argv)
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    if (argc < 2)
    {
        cerr << "Usage: " << argv[0] << " <source.pi>\n";
        return 1;
    }
    ifstream in(argv[1], ios::binary);
    if (!in)
    {
        cerr << "Cannot open file\n";
        return 1;
    }
    string src((istreambuf_iterator<char>(in)), istreambuf_iterator<char>());

    try
    {
        Lexer lex(std::move(src));
        Parser p(lex, true);
        auto tree = p.parseProgram();
        if (!p.errors.empty())
        {
            for (auto &e : p.errors)
                cerr << e << "\n";
            return 2;
        }
        printAST(tree.get());
        return 0;
    }
    catch (const ParseError &e)
    {
        cerr << e.what() << "\n";
        return 2;
    }
}
