module Lexer (Lexer(..)) where

import Error
import Position
import Token

-- LEXER TYPE 

data Lexer = Lexer 
    { source :: String 
    , start :: Int 
    , curr :: Int
    , startPos :: Position
    , lexPos :: Position
    , tokens :: [Token]
    , reporter :: [Error]
    }

init :: String -> Lexer 
init src = Lexer { source = src, start = 0, curr = 0, startPos = Position 1 1, lexPos = Position 1 1, tokens = [], reporter = [] }

-- HELPER FUNCTIONS

isDigit :: Char -> Bool 
isDigit c = c >= '0' && c <= '9'

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

isBinaryDigit :: Char -> Bool 
isBinaryDigit c = c == '0' || c == '1'

isOctalDigit :: Char -> Bool 
isOctalDigit c = c >= '0' && c <= '7'

isAlpha :: Char -> Bool 
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isAlnum :: Char -> Bool
isAlnum c = isAlpha c || isDigit c

isIdentiferStart :: Char -> Bool
isIdentiferStart c = isAlpha c || c == '_'

isIdentiferChar :: Char -> Bool
isIdentiferChar c = isIdentiferStart c || isDigit c

isSymbol :: Char -> Bool 
isSymbol c = (c >= '!' && c <= '/') || (c >= ':' && c <= '@') || (c >= '[' && c <= '`') || (c >= '{' && c <= '~')

isSpace :: Char -> Bool 
isSpace c = c == ' '

isPrintable :: Char -> Bool 
isPrintable c = isAlpha c || isDigit c || isSymbol c || isSpace c

peek :: String -> Maybe Char 
peek [] = Nothing 
peek (x : _) = Just x

peekNext :: String -> Maybe Char 
peekNext [] = Nothing 
peekNext (_ : x : _) = Just x 

updatePos :: Position -> Int -> Int -> Position 
updatePos oldPos newLine newCol = 
    oldPos 
        { line = newLine
        , col = newCol
        }

advance :: Lexer -> (Char, Lexer)
advance lexer = 
    let char = head (source lexer)
        oldPos = lexPos lexer
    in case char of
        '\n' -> (char, lexer 
                        { source = tail (source lexer) 
                        , curr = curr lexer + 1 -- for now, we'll keep this here, but we'll probs not need it
                        , lexPos = updatePos oldPos (line oldPos + 1) (1)
                        }) 
        '\t' -> (char, lexer 
                        { source = tail (source lexer)
                        , curr = curr lexer + 1
                        , lexPos = updatePos oldPos (line oldPos) (col oldPos + 4)
                        })
        '\r' -> (char, lexer 
                        { source = tail (source lexer)
                        , curr = curr lexer + 1
                        , lexPos = updatePos oldPos (line oldPos) (1)
                        }) 
        _ -> (char, lexer 
                        { source = tail (source lexer)
                        , curr = curr lexer + 1
                        , lexPos = updatePos oldPos (line oldPos) (col oldPos + 1)
                        })

bump :: Lexer -> Lexer -- only returns updated lexer state
bump lexer = 
    let oldPos = lexPos lexer 
    in lexer 
        { source = tail (source lexer)
        , curr = curr lexer + 1
        , lexPos = updatePos oldPos (line oldPos) (col oldPos + 1) 
        }

-- ADD TOKEN FUNCTIONS

addEofToken :: Lexer -> Lexer 
addEofToken lexer =
    let token = Token Eof "" (lexPos lexer)
    in lexer { tokens = token : (tokens lexer) }

addToken :: String -> TokenKind -> Lexer -> Lexer 
addToken lexeme kind lexer =
    let token = Token kind lexeme (lexPos lexer) 
    in lexer { tokens = token : (tokens lexer) }

addCharToken :: Char -> Lexer -> Lexer 
addCharToken char lexer =
    let token = Token (CharLiteral char) [char] (lexPos lexer) 
    in lexer { tokens = token : (tokens lexer) }

addStringToken :: String -> Lexer -> Lexer
addStringToken string lexer =
    let token = Token (StringLiteral string) string (lexPos lexer) 
    in lexer { tokens = token : (tokens lexer) }

addIntToken :: String -> Lexer -> Lexer 
addIntToken lexeme lexer = 
    let token = Token (IntLiteral (read lexeme :: Int)) lexeme (lexPos lexer) 
    in lexer { tokens = token : (tokens lexer) }
   
addFloatToken :: String -> Lexer -> Lexer 
addFloatToken lexeme lexer = 
    let token = Token (FloatLiteral (read lexeme :: Double)) lexeme (lexPos lexer) 
    in lexer { tokens = token : (tokens lexer) }
   
addIdentifierToken :: String -> Lexer -> Lexer
addIdentifierToken ident lexer =
    let token = Token (Identifier ident) ident (lexPos lexer) 
    in lexer { tokens = token : (tokens lexer) }

-- REPORT ERROR FUNCTIONS

addError :: Position -> String -> [Error] -> [Error]
addError pos msg rep =
    let err = Error pos msg
    in err : rep -- prepends error to the reporter

reportError :: String -> String -> Lexer -> Lexer 
reportError lexeme msg lexer =
    addToken lexeme Illegal (lexer { reporter = addError (lexPos lexer) msg (reporter lexer) })

-- READ FUNCTIONS 

readLineComment :: Lexer -> Lexer 
readLineComment lexer =
    case peek (source lexer) of 
        Just '\n'   -> lexer 
        Just _      -> 
            let (_, lexer') = advance lexer 
            in readLineComment lexer'
        Nothing     -> lexer -- reached the end of the lexer

resolveChar :: Char -> Lexer -> Either (String, Lexer) Lexer -- Left = (String, Lexer), Right = Lexer
resolveChar char lexer =
    case char of 
        '\\' -> 
            case peek (source lexer) of 
                Nothing _ -> ("unterminated escape sequence in character literal", lexer)
                Just _ ->
                    let (char', lexer') = advance lexer
                    in case char' of 
                        'n'     -> Right lexer'
                        't'     -> Right lexer'
                        'r'     -> Right lexer'
                        '\\'    -> Right lexer'
                        '\''    -> Right lexer' 
                        '"'     -> Right lexer'
                        _       -> Left ("invalid escape sequence", lexer')
        c | not (isPrintable c) -> Left ("invalid character in literal: " ++ [c], lexer)
        _ -> Right lexer

skipUntilClosingQuote :: Lexer -> Char -> Lexer 
skipUntilClosingQuote lexer quote = 
    case peek (source lexer) of 
        Nothing -> lexer
        Just char | char == quote -> 
            bump lexer
        Just _ -> 
            skipUntilClosingQuote (bump lexer) quote 

closeChar :: Lexer -> Either (String, Lexer) Lexer 
closeChar lexer = 
    case peek (source lexer) of 
        Nothing -> Left ("unterminated character literal", lexer)
        Just '\'' -> Right (bump lexer)
        Just _ ->
            let lexer' = skipUntilClosingQuote lexer '\'' 
            in Left ("character literal must contain only one character", lexer')

readChar :: Lexer -> Either (String, Lexer) Lexer -- forgot about payload, will need to modify slightly
readChar lexer = 
    case peek (source lexer) of 
        Nothing     -> Left ("unterminated character literal", lexer)
        Just '\''   -> Left ("empty character literal", bump lexer)
        Just _      -> do 
            let (char, lexer') = advance lexer
            lexer'' <- resolveChar char lexer'
            lexer''' <- closeChar lexer''
            return (addToken CharLiteral lexer''')

readString :: Lexer -> Either (String, Lexer) Lexer -- forgot about payload, will need to modify slightly
readString lexer =
    case peek (source lexer) of 
        Nothing -> Left ("unterminated string literal", lexer)
        Just '"' -> Right (addToken StringLiteral (bump lexer))
        Just _ -> 
            let (c, lexer') = advance lexer -- safe to advance, we KNOW there's at least one token
            in if c == '\\' then
                case peek (source lexer') of 
                    Nothing     -> Left ("unterminated string literal", lexer')
                    Just 'n'    -> readString (bump lexer')
                    Just 't'    -> readString (bump lexer')
                    Just 'r'    -> readString (bump lexer')
                    Just '/'    -> readString (bump lexer')
                    Just '\''   -> readString (bump lexer')
                    Just '"'    -> readString (bump lexer')
                    Just _      -> Left ("invalid escape sequence", skipUntilClosingQuote lexer' '"')
            else 
                readString lexer'

buildIdentifier :: String -> Lexer -> (String, Lexer) -- need to actually build the identifier, so return a (String, Lexer)
buildIdentifier lexeme lexer =
    case peek (source lexer) of 
        Just char | isIdentiferChar char -> 
            let (_, lexer') = advance lexer
            in buildIdentifier (char : lexeme) lexer'
        _ -> (reverse lexeme, lexer) -- we've built the identifier or keyword (DON'T FORGET TO REVERSE)

readIdentifier :: Lexer -> Lexer 
readIdentifier lexer =
    let (lexeme, lexer) = buildIdentifier "" lexer
    in case lexeme of 
        -- Literals
        "true"      -> addToken lexeme Token.True lexer
        "false"     -> addToken lexeme Token.False lexer
        "and"       -> addToken lexeme And lexer
        "or"        -> addToken lexeme Or lexer
        "not"       -> addToken lexeme Not lexer
        -- Keywords
        "fun"       -> addToken lexeme Fun lexer
        "do"        -> addToken lexeme Do lexer
        "end"       -> addToken lexeme End lexer
        "var"       -> addToken lexeme Var lexer
        "if"        -> addToken lexeme If lexer
        "then"      -> addToken lexeme Then lexer
        "else"      -> addToken lexeme Else lexer
        "while"     -> addToken lexeme While lexer
        "return"    -> addToken lexeme Return lexer
        -- Identifier
        _           -> addIdentifierToken lexeme lexer
   
readToken :: Lexer -> Lexer 
readToken lexer = 
    let (char, lexer) = advance lexer in 
        case char of 
            -- Punctuation
            '(' -> addToken [char] LeftParen lexer 
            ')' -> addToken [char] RightParen lexer 
            ',' -> addToken [char] Comma lexer 
            ' ' -> lexer -- skip these characters
            '\r' -> lexer 
            '\t' -> lexer
            '\n' -> lexer 
            -- Line Comment 
            '#' -> readLineComment lexer 
            -- Operators
            '=' -> case peek (source lexer) of 
                        Just '=' -> addToken "==" EqualEqual (bump lexer)
                        _ -> addToken "=" Equal lexer
            '!' -> case peek (source lexer) of 
                        Just '=' -> addToken "!=" BangEqual (bump lexer)
                        _ -> addToken "!" Illegal lexer -- '!' is not a valid token
            '<' -> case peek (source lexer) of 
                        Just '=' -> addToken "<=" LessEqual (bump lexer)
                        Just '<' -> addToken "<<" LessLess (bump lexer)
                        _ -> addToken "<" Less lexer
            '>' -> case peek (source lexer) of 
                        Just '=' -> addToken ">=" GreaterEqual (bump lexer)
                        Just '>' -> addToken ">>" GreaterGreater (bump lexer)
                        _ -> addToken ">" Greater lexer
            '+' -> addToken "+" Plus lexer 
            '-' -> addToken "-" Minus lexer 
            '*' -> addToken "*" Star lexer        
            '/' -> addToken "/" Slash lexer 
            '%' -> addToken "%" Percent lexer 
            '&' -> addToken "&" Amp lexer 
            '|' -> addToken "|" Pipe lexer 
            '^' -> addToken "^" Caret lexer 
            '~' -> addToken "~" Tilde lexer 
            -- Literals
            -- to be implemented
            char | isIdentiferStart char -> readIdentifier lexer
            -- Character not supported
            _ -> reportError [char] ("unexpected character " ++ [char]) lexer

readTokens :: Lexer -> Lexer 
readTokens lexer = 
    case peek (source lexer) of 
        Just _ ->
            let lexer = lexer -- need to figure this part out
                    { start = curr lexer
                    , startPos = (lexPos lexer)
                    }
            in readTokens (readToken lexer)
        Nothing -> 
            lexer {
                tokens = reverse (tokens (addEofToken lexer))
            }
