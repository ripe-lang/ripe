A non-text file is rejected up front instead of spamming lexer errors.

  $ printf '\377\376\000bad' > notsource.rp
  $ ripec notsource.rp
  ripec: notsource.rp: not valid UTF-8
  [2]
