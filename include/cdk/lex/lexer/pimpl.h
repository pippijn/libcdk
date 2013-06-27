BEGIN_LEX_NAMESPACE

struct lexer::pimpl
{
  std::string text;

  YYLTYPE *loc;
  int init;

  template<short Tok> bool is_variable_token ();
  template<short Tok> int make_token (YYSTYPE *lval, YYLTYPE const *lloc, char const *text, int leng);
};

END_LEX_NAMESPACE
