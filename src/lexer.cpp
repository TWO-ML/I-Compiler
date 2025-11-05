#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <cctype>
#include <cstdlib>
#include <iterator>

using namespace std;

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
                s.push_back(advance()); // '.'
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

static string tokenKindName(TokenKind k) {
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

int main(int argc, char** argv) {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    if (argc < 2) {
        cerr << "Usage: " << argv[0] << " <source.pi>\n";
        return 1;
    }
    ifstream in(argv[1], ios::binary);
    if (!in) { cerr << "Cannot open file\n"; return 1; }

    string src((istreambuf_iterator<char>(in)), istreambuf_iterator<char>());
    Lexer lex(std::move(src));

    for (;;) {
        Token t = lex.next();
        cout << t.line << ":" << t.col << "  " << tokenKindName(t.kind)
             << "  \"" << t.lexeme << "\"";
        if (t.kind==TokenKind::Integer) cout << "  value=" << t.intValue;
        if (t.kind==TokenKind::Real)    cout << "  value=" << (double)t.realValue;
        if (t.kind==TokenKind::Boolean) cout << "  value=" << (t.boolValue?"true":"false");
        cout << "\n";

        if (t.kind==TokenKind::EndOfFile) break;
        if (t.kind==TokenKind::Error) cerr << "Unknown char at "
                                           << t.line << ":" << t.col << "\n";
    }
    return 0;
}