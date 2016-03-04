-- lexit.lua
-- Joshua Guerrero
-- 18 February 2016
-- 
-- For CS331 Spring 2016
-- Assignment 4; Exercise A
-- Module for parseit.lua

local lexit = {}

-- ****************************************
-- START OF MODULE
-- ****************************************

-- *** Lexeme Category Names *** --
lexit.catnames = 
{
    "Keyword",			-- 1
    "Identifier",		-- 2
    "NumericLiteral",	-- 3
    "StringLiteral",	-- 4
    "Operator",			-- 5
    "Punctuation",		-- 6
    "Malformed"			-- 7
}

local preferOp = false

--- *** Character Checking Functions *** ---

-- isLetter
-- Returns true if string c is a letter character, false otherwise.
local function isLetter(c)
    if c:len() ~= 1 then
        return false
    elseif c >= "A" and c <= "Z" then
        return true
    elseif c >= "a" and c <= "z" then
        return true
    else
        return false
    end
end

-- isWhitespace
-- Returns true if string c is a whitespace character, false otherwise.
local function isWhitespace(c)
	if c:len() ~= 1 then
		return false
	elseif c == " " or c == "\t" or c == "\n" or c == "\r"
		or c == "\f" then
		return true
	else 
		return false
	end
end

-- isDigit
-- Returns true if string c is a digit character, false otherwise.
local function isDigit(c)
    if c:len() ~= 1 then
        return false
    elseif isWhitespace(c) then
		return false
    elseif c >= "0" and c <= "9" then
        return true
    else
        return false
    end
end

-- isOperator
-- Returns true if string c is a valid operator, false otherwise
local function isOperator(c)
	if c:len() ~= 1 then
		return false
	elseif c == "+" or c == "-" or c == "*" or c == "/" or c == "%"
		or c == "=" or c == "==" or c == "!=" or c == "<" or c == "<="
		or c == ">" or c == ">=" or c == "[" or c == "]" then
			return true
	else
		return false
	end
end


-- isIllegal
-- Returns true if string c is an illegal character, false otherwise.
local function isIllegal(c)
    if c:len() ~= 1 then
        return false	    
    elseif isWhitespace(c) then
        return false
    elseif c >= " " and c <= "~" then
        return false
    else
        return true
    end
end

function lexit.preferOp()
	preferOp = true
end

-- ****************************************
-- BEGIN LEXER
-- ****************************************

-- lexit
-- the 
function lexit.lex(prog)

	-- Data Member variables --
	local pos		-- index of next character in prog
	local state		-- current state
	local ch 		-- current character
	local lexstr 	-- lexeme so far
	local category 	-- category of lexeme, set when state set to done
	local handlers 	-- dispatch table; value created later

	-- Lexeme category constants
	local KEY = 1
	local ID = 2
	local NUMLIT = 3
	local STRINGLIT = 4
	local OP = 5
	local PUNCT = 6
	local MAL = 7

	-- States
	local DONE = 0
	local START = 1
	local LETTER = 2
	local DIGIT = 3
	local DIGDOT = 4
	local PLUS = 5
	local MINUS = 6
	local DOT = 7
	local OPERATORS = 8
	local EXPONENT = 9
	local EXPONENT_SIGN = 10
	local EXPONENT_DIGITS = 11
	local SINGLE_QUOTE = 12
	local DOUBLE_QUOTE = 13

	-- *** Character Related Functions *** --

	-- currChar
	-- Returns a single-character string
	-- or the empty string if pos is past end
    local function currChar()
        return prog:sub(pos, pos)
    end

    -- lookAhead
	-- Returns the char at pos+n in prog
	local function lookAhead(n)
		return prog:sub(pos+n, pos+n)
	end

    -- nextChar
    -- Returns a single-character string or
    -- the empty string if pos+1 is past end
    local function nextChar()
    	return prog:sub(pos+1, pos+1)
    end

    -- twoCharAhead
    -- Returns a single-character string or
    -- the empty string if pos+2 is past end
    local function twoCharAhead()
    	return prog:sub(pos+2,pos+2)
    end

    -- drop1
    -- Move pos to next character
    local function drop1()
    	pos = pos+1
    end

    -- add1
    -- Add current character to the lexeme,
    -- move pos to next character
    local function add1()
    	lexstr = lexstr .. currChar()
    	drop1()
    end

    -- skipWhitespace
    -- Skip whitespace and comments, moving pos beginning 
    -- of next lexeme, or to prog:len()+1.
    local function skipWhitespace()
        while true do
            while isWhitespace(currChar()) do
                drop1()
            end

            if currChar() == "#" then -- comments begin with `#`
                drop1() -- skip the `#`
            	while true do
                	if currChar() == "\n" then
                    	drop1()
                    	break
                	elseif currChar() == "" then -- end of input
                    	return
                	else
                    	drop1()
                	end
            	end
            else
            	break
            end

        end
    end

-- *****************
-- START OF HANDLERS
-- *****************
    local function handle_DONE()
        io.write("ERROR: 'DONE' state should not be handled\n")
        assert(0)
    end

    local function handle_START()
        if isIllegal(ch) then
            add1()
            state = DONE
            category = MAL
        elseif isLetter(ch) then
            add1()
            state = LETTER
        elseif isDigit(ch) then
            add1()
            state = DIGIT
        elseif ch == "." then
            add1()
            state = DOT
        elseif ch == "\'" then
        	add1()
        	state = SINGLE_QUOTE
        elseif ch == "\"" then
        	add1()
        	state = DOUBLE_QUOTE
    	elseif ch == "+" then
            add1()
            state = PLUS
        elseif ch == "-" then
            add1()
            state = MINUS
        elseif isOperator(ch) then
	    	add1()
	    	state = OPERATORS
        elseif ch == "!" and nextChar() == "=" then
    		add1()
    		add1()
    		state = OPERATORS
       	else
            add1()
            state = DONE
            category = PUNCT
        end
    end

-- *** Letter handler *** --
    local function handle_LETTER()
        if isLetter(ch) or isDigit(ch) or ch == "_" then
        	add1() 
        else
			state = DONE
			category = ID
			if lexstr == "begin"
			or lexstr == "end"
			or lexstr == "set"
			or lexstr == "print"
			or lexstr == "nl"
			or lexstr == "input"
			or lexstr == "if"
			or lexstr == "else"
			or lexstr == "elseif"
			or lexstr == "while" then
              	category = KEY
            end
        end
    end

-- *** Digit handler *** --
    local function handle_DIGIT()
    	if ch == "e" or ch == "E" then
			state = EXPONENT
        elseif isDigit(ch) then
            add1()
        else
            state = DONE
            category = NUMLIT
        end
    end

-- *** Dot handlers *** --
    local function handle_DIGDOT()
        if isDigit(ch) then
            add1()
        else
            state = DONE
            category = NUMLIT
        end
    end

    local function handle_DOT()
        state = DONE
        category = PUNCT
    end


-- *** Operation handlers *** --
    local function handle_PLUS()
    	if preferOp then
    		state = DONE
    		category = OP
        elseif isDigit(ch) then
            add1()
            state = DIGIT
        else
            state = DONE
            category = OP
        end
    end

    local function handle_MINUS()
    	if preferOp then
    		state = DONE
    		category = OP
        elseif isDigit(ch) then
            add1()
            state = DIGIT
        else
            state = DONE
            category = OP
        end
    end

    local function handle_OPERATORS()
    	if preferOp then
            if ch == "=" then
                add1()
            end
    		state = DONE
    		category = OP
    	elseif ch == "=" then
    			add1()
    			state = DONE
    			category = OP
		elseif isDigit(nextChar()) then
			add1() 
			state = DIGIT
    	else
    		state = DONE
    		category = OP
    	end
    end

-- *** Exponent handlers *** --
    local function handle_EXPONENT()
    	--check exponent for sign
    	if nextChar() == "+" or nextChar() == "-" then 
    		state = EXPONENT_SIGN
    	elseif isDigit(nextChar()) then
    		add1() 	-- e
    		add1()	-- digits
    		state = EXPONENT_DIGITS -- check for more digits
    	else
    		state = DONE
    		category = NUMLIT
    	end

    end

    local function handle_EXPONENT_SIGN()
    	if isDigit(twoCharAhead()) then
    		add1()		-- add e
    		add1()		-- add sign (+/-)
    		add1()		-- add digit
    		state = EXPONENT_DIGITS -- check for more digits
    	else
    		state = DONE
    		category = NUMLIT
    	end
    end

    local function handle_EXPONENT_DIGITS() 
    	if isDigit(ch) then
    		add1() 	-- add digits
    	else
    		state = DONE
    		category = NUMLIT
    	end
    end

-- *** Quotation handlers *** --
    local function handle_SINGLE_QUOTE()
    	if ch == "" then
    		state = DONE
    		category = MAL
    	elseif ch ~= "\'" then
    		add1()
    	else
    		add1()
    		state = DONE
    		category = STRINGLIT
    		end
    	end

	local function handle_DOUBLE_QUOTE()
    	if ch == "" then
    		state = DONE
    		category = MAL
    	elseif ch ~= "\"" then
    		add1()
    	else
    		add1()
    		state = DONE
    		category = STRINGLIT
    		end
    	end

-- *****************
-- END OF HANDLERS
-- *****************

    -- ***** Table of State-Handler Functions *****

    handlers = {
        [DONE]=handle_DONE,
        [START]=handle_START,
        [LETTER]=handle_LETTER,
        [DIGIT]=handle_DIGIT,
        [DIGDOT]=handle_DIGDOT,
        [PLUS]=handle_PLUS,
        [MINUS]=handle_MINUS,
        [DOT]=handle_DOT,
        [OPERATORS] = handle_OPERATORS,
        [EXPONENT] = handle_EXPONENT,
        [EXPONENT_SIGN] = handle_EXPONENT_SIGN,
        [EXPONENT_DIGITS] = handle_EXPONENT_DIGITS,
        [SINGLE_QUOTE] = handle_SINGLE_QUOTE,
        [DOUBLE_QUOTE] = handle_DOUBLE_QUOTE
    }

    -- ***** Iterator Function *****

    -- getLexeme
    -- Called each time through the for-in loop.
    -- Returns a pair: lexeme-string (string) and category (int), or
    -- nil, nil if no more lexemes.
    local function getLexeme(dummy1, dummy2)
        if pos > prog:len() then
        	preferOp = false
            return nil, nil
        end
        lexstr = ""
        state = START
        while state ~= DONE do
            ch = currChar()
            handlers[state]()
        end

        skipWhitespace()
        preferOp = false	-- set back to false after lexeme is finished
        return lexstr, category
    end

    -- ***** Body of Function lex *****

    -- Initialize & return the iterator function
    pos = 1
    skipWhitespace()
    return getLexeme, nil, nil

end

-- ****************************************
-- END OF LEXER
-- ****************************************

return lexit




-- ****************************************
-- END OF FILE
-- ****************************************


