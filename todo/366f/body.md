The lexer takes \n \t \\ and \" only, so a CRLF has to be built with snprintf and %c.
