(begin
  (display (string-length "a\nb"))
  (display (char->integer (string-ref "a\nb" 1)))
  (display (string-length "a\tb"))
  (display (char->integer (string-ref "a\tb" 1)))
  (display (string-length "quote: \"hi\""))
  (display (string-length "backslash: \\"))
)
