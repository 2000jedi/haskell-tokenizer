module Scanner where
    import Data.Char

    data Opr = Add | Sub | Mul | Div | Dot | Sep | LPar | RPar | Eol
        deriving (Show, Eq)

    data Token = Ident       String
               | Opr         Opr
               | Num         Int
               | Str         String
               | Exceptional Char
        deriving (Show, Eq)

    scan_num_it :: String -> Int -> [Token]
    scan_num_it s n = if not (null s) && isDigit (head s)
        then scan_num_it (tail s) (n * 10 + ((digitToInt (head s))))
        else Num n : scan s

    scan_num :: String -> [Token]
    scan_num s = scan_num_it s 0

    scan_ident_it :: String -> String -> [Token]
    scan_ident_it s ident = if not (null s) && (isAlpha (head s) || isDigit (head s))
        then scan_ident_it (tail s) (ident ++ [head s])
        else (Ident ident) : scan s

    scan_ident :: String -> [Token]
    scan_ident s = scan_ident_it s ""

    scan :: String -> [Token]
    scan s
        | null s           = []
        | head s == '+'    = Opr Add : scan (tail s)
        | head s == '-'    = Opr Sub : scan (tail s)
        | head s == '*'    = Opr Mul : scan (tail s)
        | head s == '/'    = Opr Div : scan (tail s)
        | isDigit (head s) = scan_num s
        | isAlpha (head s) = scan_ident s
        | head s == '"'    = (Str "") : scan (tail s)
        | head s == '.'    = Opr Dot : scan (tail s)
        | head s == ','    = Opr Sep : scan (tail s)
        | head s == '('    = Opr LPar : scan (tail s)
        | head s == ')'    = Opr RPar : scan (tail s)
        | head s == ';'    = Opr Eol : scan (tail s)
        | head s == '\n'   = scan (tail s)
        | head s == '\r'   = scan (tail s)
        | head s == ' '    = scan (tail s)
        | otherwise        = Exceptional (head s) : scan (tail s)
