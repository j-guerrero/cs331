-- parseit.lua
-- Joshua Guerrero
-- February 29, 2016
-- 
-- For CS331 Spring 2016
-- Assignment 4
-- Exercise A — Recursive-Descent Parser
-- Requires lexit.lua

local parseit = {}

lexer = require "lexit"

-- ========= --
-- Variables --
-- ========= --

-- For lexit iteration
local iter          -- Iterator returned by lexit.lex
local state         -- State for above iterator (maybe not used)
local lexer_out_s   -- Return value #1 from above iterator
local lexer_out_c   -- Return value #2 from above iterator

-- For current lexeme
local lexstr = ""   -- String form of current lexeme
local lexcat = 0    -- Category of current lexeme:
                    --  one of categories below, or 0 for past the end

-- Lexeme category constants
local KEY = 1
local ID = 2
local NUMLIT = 3
local STRLIT = 4
local OP = 5
local PUNCT = 6
local MAL = 7

-- Symbolic Constants for AST
STMT_LIST  = 1
SET_STMT   = 2
PRINT_STMT = 3
NL_STMT    = 4
INPUT_STMT = 5
IF_STMT    = 6
WHILE_STMT = 7
BIN_OP     = 8
UN_OP      = 9
NUMLIT_VAL = 10
STRLIT_VAL = 11
ID_VAL     = 12
ARRAY_REF  = 13

-- ================= --
-- Utility Functions --
-- ================= --

-- advance
-- Go to next lexeme and load it into lexstr, lexcat.
-- Should be called once before any parsing is done.
-- Function init must be called before this function is called.
local function advance()
	
	-- call preferOp() if next lexeme is and ID, NUMLIT,")", or "]"
	if lexer_out_c == ID or lexer_out_c == NUMLIT or
		lexer_out_s == ")" or lexer_out_s == "]" then
			lexer.preferOp()
	end

    -- Advance the iterator
    lexer_out_s, lexer_out_c = iter(state, lexer_out_s)

    -- If we're not past the end, copy current lexeme into vars
    if lexer_out_s ~= nil then
        lexstr, lexcat = lexer_out_s, lexer_out_c
    else
        lexstr, lexcat = "", 0
    end
end


-- init
-- Initial call. Sets input for parsing functions.
local function init(prog)
    iter, state, lexer_out_s = lexer.lex(prog)
    advance()
end


-- atEnd
-- Return true is pos has reached end of input.
-- Function init must be called before this function is called.
local function atEnd()
    return lexcat == 0
end


-- matchString
-- Given string, see if current lexeme string form is equal to it. If
-- so, then advance to next lexeme & return true. If not, then do not
-- advance, return false.
-- Function init must be called before this function is called.
local function matchString(s)
    if lexstr == s then
        advance()
        return true
    else
        return false
    end
end


-- matchCat
-- Given lexeme category (integer), see if current lexeme category is
-- equal to it. If so, then advance to next lexeme & return true. If
-- not, then do not advance, return false.
-- Function init must be called before this function is called.
local function matchCat(c)
    if lexcat == c then
        advance()
        return true
    else
        return false
    end
end

-- ================================ --
-- Primary Function for Client Code --
-- ================================ --

-- Define local functions for later calling (like prototypes in C++)
local parse_program
local parse_stmt_list
local parse_statement
local parse_expr
local parse_aexpr
local parse_term
local parse_factor
local parse_lvalue

-- parse
-- Given program, initialize parser and call parsing function for start
-- symbol. Returns boolean: true indicates successful parse AND end of
-- input reached. Otherwise, false.
function parseit.parse(prog)

	-- Initialization
	init(prog)

	local success, ast = parse_program()

	if success then
		return true, ast

	else
		return false, nil
	end

end


-- =============== --
-- Parse Functions --
-- =============== --

-- parse_program
-- Parsing function for nonterminal "program".
-- Function init must be called before this function is called.
function parse_program()
    local good, ast

    good, ast = parse_stmt_list()
    if not good then
        return false, nil
    end

    if not atEnd() then
        return false, nil
    end

    return true, ast
end


-- parse_stmt_list
-- Parsing function for nonterminal "stmt_list".
-- Function init must be called before this function is called.
function parse_stmt_list()
    local good, ast, newast
    ast = {STMT_LIST}

    while true do
        if lexstr ~= "set"
          and lexstr ~= "print"
          and lexstr ~= "nl"
          and lexstr ~= "input"
          and lexstr ~= "if"
          and lexstr ~= "while" then
            return true, ast
        end

        good, newast = parse_statement()
        if not good then
            return false, nil
        end
        table.insert(ast, newast)
    end
end


-- parse_statement
-- Parsing function for nonterminal "statement"
-- Function init must be called before this function is called.
function parse_statement()
    local good, ast1, ast2, savelex

    -- "set" statement
    if matchString("set") then
        good, ast1 = parse_lvalue()
        if not good then
            return false, nil
        end

        -- if there is no "=" following set
        if not matchString("=") then
            return false, nil
        end

        -- if no expression after "="
        good, ast2 = parse_expr()
        if not good then
            return false, nil
        end

        return true, {SET_STMT, ast1, ast2}
        
    -- "print" statement
    elseif matchString("print") then
        savelex = lexstr

        if matchCat(STRLIT) then
            return true, {PRINT_STMT, {STRLIT_VAL, savelex}}
        end

        good, ast1 = parse_expr()
        if not good then
            return false, nil
        end
        return true, {PRINT_STMT, ast1}

    -- newline statement
    elseif matchString("nl") then
    	return true, {NL_STMT, ast1}

    -- "input" statement
    elseif matchString("input") then
    	good, ast1 = parse_lvalue()
    	if not good then
    		return false, nil
    	end

    	return true, {INPUT_STMT, ast1}

    -- "if" statement
    elseif matchString("if") then
    	local ast3, ast4

    	good, ast1 = parse_expr()
    	if not good then
    		return false, nil
    	end

    	good, ast2 = parse_stmt_list()
    	if not good then
    		return false, nil
    	end

    	ast1 = {IF_STMT, ast1, ast2 }

    	-- optional "else if"or "else" statement
   		while true do

   			if matchString("elseif") then

   				good, ast3 = parse_expr()
   				if not good then 
   					return false, nil
   				end

   				good, ast4 = parse_stmt_list()
   				if not good then
   					return false, nil
   				end

   				table.insert(ast1, ast3)
   				table.insert(ast1, ast4)
   			
   			elseif matchString("else") then
   				good, ast3 = parse_stmt_list()
   				if not good then
   					return false, nil
   				end
   				table.insert(ast1, ast3)

	    	elseif matchString("end") then
	    		return true, ast1

		    else
		    	return false, nil
		    end
		end

	-- "while" statement
	elseif matchString("while") then
		good, ast1 = parse_expr()
		if not good then
			return false, nil
		end

		good, ast2 = parse_stmt_list()
		if not good then
			return false, nil
		end

		if not matchString("end") then
			return false, nil
		end

		return true, { WHILE_STMT, ast1, ast2 }
	end

	return false, nil
end

-- parse_expr
-- Parsing function for nonterminal "expr".
-- Function init must be called before this function is called.
function parse_expr()
	local saveop, good, ast, newast

	good, ast = parse_aexpr()
	if not good then
		return false, nil
	end

	-- Optional loop for comparison operators
	while true do
		saveop = lexstr

		if not matchString("==") and not matchString("!=")
			and not matchString("<") and not matchString("<=")
			and not matchString(">") and not matchString(">=") then
			return true, ast
		end

		good, newast = parse_aexpr()
		if not good then
			return false, nil
		end

		ast = {{BIN_OP, saveop}, ast, newast}
	end

end

-- parse_aexpr
-- Parsing function for nonterminal "aexpr".
-- Function init must be called before this function is called.
function parse_aexpr()
	local saveop, good, ast, newast

	good, ast = parse_term()
	if not good then
		return false, nil
	end

	-- Optional loop for Unary Operators
	while true do
		saveop = lexstr

		if not matchString("+") and not matchString("-") then
			return true, ast
		end

		good, newast = parse_term()
		if not good then
			return false, nil
		end

		ast = {{BIN_OP, saveop}, ast, newast }
	end

end

-- parse_term
-- Parsing function for nonterminal "term".
-- Function init must be called before this function is called.
function parse_term()
	local saveop, good, ast, newast

	good, ast = parse_factor()
	if not good then
		return false, nil
	end

	-- Optional operator loop
	while true do
		saveop = lexstr

		if not matchString("*") and not matchString("/") 
		and not matchString("%") then
			return true, ast
		end

		good, newast = parse_factor()
		if not good then
			return false, nil
		end

		ast = {{BIN_OP, saveop}, ast, newast}
	end

end

-- parse_factor
-- Parsing function for nonterminal "factor".
-- Function init must be called before this function is called.
function parse_factor()
	local savelex, good, ast

	savelex = lexstr

	-- if lexeme matches a NUMLIT
	if matchCat(NUMLIT) then
		return true, {NUMLIT_VAL, savelex}
	
	-- if expression is paranthesized
	elseif matchString("(") then
		good, ast = parse_expr()
		if not good then
			return false, nil
		end

		-- check closing parantheses
		if not matchString(")") then
			return false, nil
		end

		return true, ast

	-- If expression includes Unary Operators
	elseif matchString("+") then
		good, ast = parse_factor()
		if not good then
			return false, nil
		end

		return true, { {UN_OP, savelex}, ast}

	elseif matchString("-") then
		good, ast = parse_factor()
		if not good then
			return false, nil
		end

		return true, { {UN_OP, savelex}, ast}

	-- If just an lvalue
	else
		good, ast = parse_lvalue()
		if not good then
			return false, nil
		end

		return true, ast
	end

end

-- parse_lvalue
-- Parsing function for nonterminal "lvalue".
-- Function init must be called before this function is called.
function parse_lvalue()

	local savelex, good, ast

	savelex = lexstr
	if matchCat(ID) then

		-- if brackets
		if matchString("[") then

			good,ast = parse_expr()
			if not good then
				return false, nil
			end

			-- if closing bracket is missing
			if not matchString("]") then
				return false, nil
			end

			return true, { ARRAY_REF, { ID_VAL, savelex }, ast }

		end

		-- if no brackets
		return true, { ID_VAL, savelex }

	-- not an ID
	else
		return false, nil
	end
end

return parseit












