#!/usr/bin/env lua
-- parseit_test.lua
-- Glenn G. Chappell
-- 24 Feb 2016
--
-- For CS 331 Spring 2016
-- Test Program for Module parseit
-- Used in Assignment 4, Exercise A

parseit = require "parseit"  -- Import parseit module


-- *********************************************
-- * YOU MAY WISH TO CHANGE THE FOLLOWING LINE *
-- *********************************************

exit_on_failure = true

-- If exit_on_failure is true, then:
-- - On first failing test, we print the input, expected output, and
--   actual output; then the test program terminates.
-- If exit_on_failure is false, then:
-- - All tests are performed, with a summary of results printed at the
--   end.


-- *********************************************************************
-- Testing Package
-- *********************************************************************


tester = {}
tester.countTests = 0
tester.countPasses = 0

function tester.test(self, success, testName)
    self.countTests = self.countTests+1
    io.write("    Test: " .. testName .. " - ")
    if success then
        self.countPasses = self.countPasses+1
        io.write("passed")
    else
        io.write("********** FAILED **********")
    end
    io.write("\n")
end

function tester.allPassed(self)
    return self.countPasses == self.countTests
end


-- *********************************************************************
-- Definitions for This Test Program
-- *********************************************************************


-- Symbolic Constants for AST

local STMT_LIST  = 1
local SET_STMT   = 2
local PRINT_STMT = 3
local NL_STMT    = 4
local INPUT_STMT = 5
local IF_STMT    = 6
local WHILE_STMT = 7
local BIN_OP     = 8
local UN_OP      = 9
local NUMLIT_VAL = 10
local STRLIT_VAL = 11
local ID_VAL     = 12
local ARRAY_REF  = 13


-- String forms of symbolic constants

symbolNames = {
  [1]="STMT_LIST",
  [2]="SET_STMT",
  [3]="PRINT_STMT",
  [4]="NL_STMT",
  [5]="INPUT_STMT",
  [6]="IF_STMT",
  [7]="WHILE_STMT",
  [8]="BIN_OP",
  [9]="UN_OP",
  [10]="NUMLIT_VAL",
  [11]="STRLIT_VAL",
  [12]="ID_VAL",
  [13]="ARRAY_REF"
}


-- writeAST_parseit
-- Write an AST, in (roughly) Lua form, with numbers replaced by the
-- symbolic constants used in parseit.
-- A table is assumed to represent an array.
-- See the Assignment 4 description for the AST Specification.
function writeAST_parseit(x)
    if type(x) == "number" then
        local name = symbolNames[x]
        if name == nil then
            io.write("<ERROR: Unknown constant: "..x..">")
        else
            io.write(name)
        end
    elseif type(x) == "string" then
        io.write('"'..x..'"')
    elseif type(x) == "boolean" then
        if x then
            io.write("true")
        else
            io.write("false")
        end
    elseif type(x) == "table" then
        local first = true
        io.write("{")
        for k = 1, #x do  -- ipairs is problematic
            if not first then
                io.write(", ")
            end
            writeAST_parseit(x[k])
            first = false
        end
        io.write("}")
    elseif type(x) == "nil" then
        io.write("nil")
    else
        io.write("<ERROR: "..type(x)..">")
    end
end


function printNoteAndExit()
    io.write("\n")
    io.write("******************************************************\n")
    io.write("*                                                    *\n")
    io.write("*  NOTE: This program is set to terminate after the  *\n")
    io.write("*        first failing test. If you would prefer to  *\n")
    io.write("*        execute all tests, whether they pass or     *\n")
    io.write("*        not, then set variable exit_on_failure to   *\n")
    io.write("*        false. (See the beginning of the source     *\n")
    io.write("*        code.)                                      *\n")
    io.write("*                                                    *\n")
    io.write("******************************************************\n")
    io.write("\n")
    os.exit(1)
end


-- astEq
-- Checks equality of two ASTs, represented as in the Assignment 4
-- description. Returns true if equal, false otherwise.
function astEq(ast1, ast2)
    if type(ast1) ~= type(ast2) then
        return false
    end
    
    if type(ast1) ~= "table" then
        return ast1 == ast2
    end

    if #ast1 ~= #ast2 then
        return false
    end

    for k = 1, #ast1 do  -- ipairs is problematic
        if not astEq(ast1[k], ast2[k]) then
            return false
        end
    end
    return true
end


-- bool2Str
-- Given boolean, return string representing it: "true" or "false".
function bool2Str(b)
    if b then
        return "true"
    else
        return "false"
    end
end


-- checkParse
-- Given tester object, input string ("program"), expected output values
-- from parser (good, AST), and string giving the name of the test. Do
-- test & print result. If test fails and exit_on_failure is true, then
-- print detailed results and exit program.
function checkParse(t, prog, expectedGood, expectedAST, testName)
    local actualGood, actualAST = parseit.parse(prog)
    local sameGood = (expectedGood == actualGood)
    local sameAST = true
    if sameGood and expectedGood then
        sameAST = astEq(expectedAST, actualAST)
    end
    t:test(sameGood and sameAST, testName)
        
    if not exit_on_failure or (sameGood and sameAST) then
        return
    end

    io.write("\n")
    io.write("Input for the last test above:\n")
    io.write('"'..prog..'"\n')
    io.write("\n")
    io.write("Expected parser boolean return value: ")
    io.write(bool2Str(expectedGood).."\n")
    io.write("Actual parser boolean return value: ")
    io.write(bool2Str(actualGood).."\n")
    if not sameAST then
        io.write("\n")
        io.write("Expected AST:\n")
        writeAST_parseit(expectedAST)
        io.write("\n")
        io.write("\n")
        io.write("Returned AST:\n")
        writeAST_parseit(actualAST)
        io.write("\n")
    end
    printNoteAndExit()
end


-- *********************************************************************
-- Test Suite Functions
-- *********************************************************************


function test_simple(t)
    io.write("Test Suite: simple cases\n")

    checkParse(t, "", true, {STMT_LIST},
      "Empty program")
    checkParse(t, "end", false, nil,
      "Bad program: Keyword only #1")
    checkParse(t, "elseif", false, nil,
      "Bad program: Keyword only #2")
    checkParse(t, "else", false, nil,
      "Bad program: Keyword only #3")
    checkParse(t, "abc", false, nil,
      "Bad program: Identifier only")
    checkParse(t, "123", false, nil,
      "Bad program: NumericLiteral only")
    checkParse(t, "'xyz'", false, nil,
      "Bad program: StringLiteral only #1")
    checkParse(t, '"xyz"', false, nil,
      "Bad program: StringLiteral only #2")
    checkParse(t, "<=", false, nil,
      "Bad program: Operator only")
    checkParse(t, "{", false, nil,
      "Bad program: Punctuation only")
    checkParse(t, "\a", false, nil,
      "Bad program: Malformed only #1")
    checkParse(t, "'", false, nil,
      "Bad program: Malformed only #2")
end


function test_set_stmt(t)
    io.write("Test Suite: set statements\n")

    checkParse(t, "set abc = 123", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"abc"},{NUMLIT_VAL,"123"}}},
      "Set statement: NumericLiteral")
    checkParse(t, "set abc = xyz", true,
      {STMT_LIST,{SET_STMT,{ID_VAL, "abc"},{ID_VAL,"xyz"}}},
      "Set statement: identifier")
    checkParse(t, "set abc[1] = xyz", true,
      {STMT_LIST,{SET_STMT,{ARRAY_REF,{ID_VAL,"abc"},{NUMLIT_VAL,"1"}},
        {ID_VAL,"xyz"}}},
      "Set statement: array ref = ...")
    checkParse(t, "abc = 123", false, nil,
      "Bad set statement: missing 'set'")
    checkParse(t, "set = 123", false, nil,
      "Bad set statement: missing LHS")
    checkParse(t, "set 123 = 123", false, nil,
      "Bad set statement: LHS is NumericLiteral")
    checkParse(t, "set end = 123", false, nil,
      "Bad set statement: LHS is Keyword")
    checkParse(t, "set abc 123", false, nil,
      "Bad set statement: missing assignment op")
    checkParse(t, "set abc == 123", false, nil,
      "Bad set statement: assignment op replaced by equality")
    checkParse(t, "set abc =", false, nil,
      "Bad set statement: RHS is empty")
    checkParse(t, "set abc = end", false, nil,
      "Bad set statement: RHS is Keyword")
    checkParse(t, "set abc = 1 2", false, nil,
      "Bad set statement: RHS is two NumericLiterals")
    checkParse(t, "set abc = 1 end", false, nil,
      "Bad set statement: followed by end")
end


function test_expr_simple(t)
    io.write("Test Suite: simple expressions\n")

    checkParse(t, "set x = 1 + 2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},
        {NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: binary + (numbers with space)")
    checkParse(t, "set x = 1+2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},
        {NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: binary + (numbers without space)")
    checkParse(t, "set x = a+2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},
        {ID_VAL,"a"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: binary + (var+number)")
    checkParse(t, "set x = 1+b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},
        {NUMLIT_VAL,"1"},{ID_VAL,"b"}}}},
      "Simple expression: binary + (number+var)")
    checkParse(t, "set x = a+b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},
        {ID_VAL,"a"},{ID_VAL,"b"}}}},
      "Simple expression: binary + (vars)")
    checkParse(t, "set x = 1+", false, nil,
      "Bad expression: end with +")
    checkParse(t, "set x = 1 - 2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},
        {NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: binary - (numbers with space)")
    checkParse(t, "set x = 1-2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},
        {NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: binary - (numbers without space)")
    checkParse(t, "set x = 1-", false, nil,
      "Bad expression: end with -")
    checkParse(t, "set x = 1*2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"*"},
        {NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: * (numbers)")
    checkParse(t, "set x = a*2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"*"},
        {ID_VAL,"a"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: * (var*number)")
    checkParse(t, "set x = 1*b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"*"},
        {NUMLIT_VAL,"1"},{ID_VAL,"b"}}}},
      "Simple expression: * (number*var)")
    checkParse(t, "set x = a*b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"*"},
        {ID_VAL,"a"},{ID_VAL,"b"}}}},
      "Simple expression: * (vars)")
    checkParse(t, "set x = 1*", false, nil,
      "Bad expression: end with *")
    checkParse(t, "set x = 1/2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"/"},
        {NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: /")
    checkParse(t, "set x = 1/", false, nil,
      "Bad expression: end with /")
    checkParse(t, "set x = 1%2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"%"},
        {NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: /")
    checkParse(t, "set x = 1%", false, nil,
      "Bad expression: end with %")
    checkParse(t, "set x = 1==2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},
        {NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: == (numbers)")
    checkParse(t, "set x = a==2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},
        {ID_VAL,"a"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: == (var==number)")
    checkParse(t, "set x = 1==b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},
        {NUMLIT_VAL,"1"},{ID_VAL,"b"}}}},
      "Simple expression: == (number==var)")
    checkParse(t, "set x = a==b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},
        {ID_VAL,"a"},{ID_VAL,"b"}}}},
      "Simple expression: == (vars)")
    checkParse(t, "set x = 1==", false, nil,
      "Bad expression: end with ==")
    checkParse(t, "set x = 1!=2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"!="},
        {NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: !=")
    checkParse(t, "set x = 1!=", false, nil,
      "Bad expression: end with !=")
    checkParse(t, "set x = 1<2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"<"},
        {NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: <")
    checkParse(t, "set x = 1<", false, nil,
      "Bad expression: end with <")
    checkParse(t, "set x = 1<=2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"<="},
        {NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: <=")
    checkParse(t, "set x = 1<=", false, nil,
      "Bad expression: end with <=")
    checkParse(t, "set x = 1>2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">"},
        {NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: >")
    checkParse(t, "set x = 1>", false, nil,
      "Bad expression: end with >")
    checkParse(t, "set x = 1>=2", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">="},
        {NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}}}},
      "Simple expression: >=")
    checkParse(t, "set x = +a", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{UN_OP,"+"},{ID_VAL,"a"}}}},
      "Simple expression: unary +")
    checkParse(t, "set x = -a", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{UN_OP,"-"},{ID_VAL,"a"}}}},
      "Simple expression: unary -")
    checkParse(t, "set x = 1>=", false, nil,
      "Bad expression: end with >=")
    checkParse(t, "set x = (1)", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{NUMLIT_VAL,"1"}}},
      "Simple expression: parens (number)")
    checkParse(t, "set x = (a)", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{ID_VAL,"a"}}},
      "Simple expression: parens (var)")
    checkParse(t, "set x = a[1]", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{ARRAY_REF,{ID_VAL,"a"},
        {NUMLIT_VAL,"1"}}}},
      "Simple expression: array ref")
    checkParse(t, "set x = (1", false, nil,
      "Bad expression: no closing paren")
    checkParse(t, "set x = ()", false, nil,
      "Bad expression: empty parens")
    checkParse(t, "set x = a[1", false, nil,
      "Bad expression: no closing bracket")
    checkParse(t, "set x = a 1]", false, nil,
      "Bad expression: no opening bracket")
    checkParse(t, "set x = a[]", false, nil,
      "Bad expression: empty brackets")
    checkParse(t, "set x = (x)", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{ID_VAL,"x"}}},
      "Simple expression: var in parens on RHS")
    checkParse(t, "set (x) = x", false, nil,
      "Bad expression: var in parens on LHS")
    checkParse(t, "set x[1] = (x[1])", true,
      {STMT_LIST,{SET_STMT,{ARRAY_REF,{ID_VAL,"x"},{NUMLIT_VAL,"1"}},
        {ARRAY_REF,{ID_VAL,"x"},{NUMLIT_VAL,"1"}}}},
      "Simple expression: array ref in parens on RHS")
    checkParse(t, "set (x[1]) = x[1]", false, nil,
      "Bad expression: array ref in parens on LHS")
end


function test_expr_prec_assoc(t)
    io.write("Test Suite: expressions - precedence & associativity\n")

    checkParse(t, "set x = 1+2+3+4+5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{{BIN_OP,"+"},
        {{BIN_OP, "+"},{{BIN_OP,"+"},{NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}},{NUMLIT_VAL,"5"}}}},
      "Binary operator + is left-associative")
    checkParse(t, "set x = 1-2-3-4-5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},{{BIN_OP,"-"},
        {{BIN_OP, "-"},{{BIN_OP,"-"},{NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}},{NUMLIT_VAL,"5"}}}},
      "Binary operator - is left-associative")
    checkParse(t, "set x = 1*2*3*4*5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"*"},{{BIN_OP,"*"},
        {{BIN_OP, "*"},{{BIN_OP,"*"},{NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}},{NUMLIT_VAL,"5"}}}},
      "Operator * is left-associative")
    checkParse(t, "set x = 1/2/3/4/5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"/"},{{BIN_OP,"/"},
        {{BIN_OP, "/"},{{BIN_OP,"/"},{NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}},{NUMLIT_VAL,"5"}}}},
      "Operator / is left-associative")
    checkParse(t, "set x = 1%2%3%4%5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"%"},{{BIN_OP,"%"},
        {{BIN_OP, "%"},{{BIN_OP,"%"},{NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}},{NUMLIT_VAL,"5"}}}},
      "Operator % is left-associative")
    checkParse(t, "set x = 1==2==3==4==5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},{{BIN_OP,"=="},
        {{BIN_OP, "=="},{{BIN_OP,"=="},{NUMLIT_VAL,"1"},
        {NUMLIT_VAL,"2"}},{NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}},
        {NUMLIT_VAL,"5"}}}},
      "Operator == is left-associative")
    checkParse(t, "set x = 1!=2!=3!=4!=5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"!="},{{BIN_OP,"!="},
        {{BIN_OP, "!="},{{BIN_OP,"!="},{NUMLIT_VAL,"1"},
        {NUMLIT_VAL,"2"}},{NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}},
        {NUMLIT_VAL,"5"}}}},
      "Operator != is left-associative")
    checkParse(t, "set x = 1<2<3<4<5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"<"},{{BIN_OP,"<"},
        {{BIN_OP, "<"},{{BIN_OP,"<"},{NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}},{NUMLIT_VAL,"5"}}}},
      "Operator < is left-associative")
    checkParse(t, "set x = 1<=2<=3<=4<=5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"<="},{{BIN_OP,"<="},
        {{BIN_OP, "<="},{{BIN_OP,"<="},{NUMLIT_VAL,"1"},
        {NUMLIT_VAL,"2"}},{NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}},
        {NUMLIT_VAL,"5"}}}},
      "Operator <= is left-associative")
    checkParse(t, "set x = 1>2>3>4>5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">"},{{BIN_OP,">"},
        {{BIN_OP, ">"},{{BIN_OP,">"},{NUMLIT_VAL,"1"},{NUMLIT_VAL,"2"}},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}},{NUMLIT_VAL,"5"}}}},
      "Operator > is left-associative")
    checkParse(t, "set x = 1>=2>=3>=4>=5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">="},{{BIN_OP,">="},
        {{BIN_OP, ">="},{{BIN_OP,">="},{NUMLIT_VAL,"1"},
        {NUMLIT_VAL,"2"}},{NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}},
        {NUMLIT_VAL,"5"}}}},
      "Operator >= is left-associative")

    checkParse(t, "set x = ++++a", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{UN_OP,"+"},{{UN_OP,"+"},
        {{UN_OP,"+"},{{UN_OP,"+"},{ID_VAL,"a"}}}}}}},
      "Unary operator + is right-associative")
    checkParse(t, "set x = ----a", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{UN_OP,"-"},{{UN_OP,"-"},
        {{UN_OP,"-"},{{UN_OP,"-"},{ID_VAL,"a"}}}}}}},
      "Unary operator - is right-associative")

    checkParse(t, "set x = a==b>c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">"},{{BIN_OP,"=="},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: ==, >")
    checkParse(t, "set x = a==b+c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},{ID_VAL,"a"},
        {{BIN_OP,"+"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: ==, binary +")
    checkParse(t, "set x = a==b-c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},{ID_VAL,"a"},
        {{BIN_OP,"-"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: ==, binary -")
    checkParse(t, "set x = a==b*c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},{ID_VAL,"a"},
        {{BIN_OP,"*"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: ==, *")
    checkParse(t, "set x = a==b/c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},{ID_VAL,"a"},
        {{BIN_OP,"/"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: ==, /")
    checkParse(t, "set x = a==b%c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},{ID_VAL,"a"},
        {{BIN_OP,"%"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: ==, %")

    checkParse(t, "set x = a>b==c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},{{BIN_OP,">"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: >, ==")
    checkParse(t, "set x = a>b+c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">"},{ID_VAL,"a"},
        {{BIN_OP,"+"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: >, binary +")
    checkParse(t, "set x = a>b-c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">"},{ID_VAL,"a"},
        {{BIN_OP,"-"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: >, binary -")
    checkParse(t, "set x = a>b*c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">"},{ID_VAL,"a"},
        {{BIN_OP,"*"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: >, *")
    checkParse(t, "set x = a>b/c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">"},{ID_VAL,"a"},
        {{BIN_OP,"/"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: >, /")
    checkParse(t, "set x = a>b%c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">"},{ID_VAL,"a"},
        {{BIN_OP,"%"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: >, %")

    checkParse(t, "set x = a+b==c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},{{BIN_OP,"+"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: binary +, ==")
    checkParse(t, "set x = a+b>c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">"},{{BIN_OP,"+"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: binary +, >")
    checkParse(t, "set x = a+b-c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},{{BIN_OP,"+"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: binary +, binary -")
    checkParse(t, "set x = a+b*c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{ID_VAL,"a"},
        {{BIN_OP,"*"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: binary +, *")
    checkParse(t, "set x = a+b/c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{ID_VAL,"a"},
        {{BIN_OP,"/"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: binary +, /")
    checkParse(t, "set x = a+b%c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{ID_VAL,"a"},
        {{BIN_OP,"%"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: binary +, %")

    checkParse(t, "set x = a-b==c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},{{BIN_OP,"-"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: binary -, ==")
    checkParse(t, "set x = a-b>c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">"},{{BIN_OP,"-"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: binary -, >")
    checkParse(t, "set x = a-b+c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{{BIN_OP,"-"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: binary -, binary +")
    checkParse(t, "set x = a-b*c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},{ID_VAL,"a"},
        {{BIN_OP,"*"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: binary -, *")
    checkParse(t, "set x = a-b/c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},{ID_VAL,"a"},
        {{BIN_OP,"/"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: binary -, /")
    checkParse(t, "set x = a-b%c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},{ID_VAL,"a"},
        {{BIN_OP,"%"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence check: binary -, %")

    checkParse(t, "set x = a*b==c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},{{BIN_OP,"*"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: *, ==")
    checkParse(t, "set x = a*b>c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">"},{{BIN_OP,"*"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: *, >")
    checkParse(t, "set x = a*b+c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{{BIN_OP,"*"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: *, binary +")
    checkParse(t, "set x = a*b-c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},{{BIN_OP,"*"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: *, binary -")
    checkParse(t, "set x = a*b/c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"/"},{{BIN_OP,"*"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: *, /")
    checkParse(t, "set x = a*b%c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"%"},{{BIN_OP,"*"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: *, %")

    checkParse(t, "set x = a/b==c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},{{BIN_OP,"/"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: /, ==")
    checkParse(t, "set x = a/b>c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">"},{{BIN_OP,"/"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: /, >")
    checkParse(t, "set x = a/b+c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{{BIN_OP,"/"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: /, binary +")
    checkParse(t, "set x = a/b-c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},{{BIN_OP,"/"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: /, binary -")
    checkParse(t, "set x = a/b*c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"*"},{{BIN_OP,"/"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: /, *")
    checkParse(t, "set x = a/b%c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"%"},{{BIN_OP,"/"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: /, %")

    checkParse(t, "set x = a%b==c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},{{BIN_OP,"%"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: %, ==")
    checkParse(t, "set x = a%b>c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">"},{{BIN_OP,"%"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: %, >")
    checkParse(t, "set x = a%b+c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{{BIN_OP,"%"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: %, binary +")
    checkParse(t, "set x = a%b-c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},{{BIN_OP,"%"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: %, binary -")
    checkParse(t, "set x = a%b*c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"*"},{{BIN_OP,"%"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: %, *")
    checkParse(t, "set x = a%b/c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"/"},{{BIN_OP,"%"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence check: %, /")

    checkParse(t, "set x = a!=+b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"!="},{ID_VAL,"a"},
        {{UN_OP,"+"},{ID_VAL,"b"}}}}},
      "Precedence check: !=, unary +")
    checkParse(t, "set x = -a<c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"<"},{{UN_OP,"-"},
        {ID_VAL,"a"}},{ID_VAL,"c"}}}},
      "Precedence check: unary -, <")
    checkParse(t, "set x = a++b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{ID_VAL,"a"},
        {{UN_OP,"+"},{ID_VAL,"b"}}}}},
      "Precedence check: binary +, unary +")
    checkParse(t, "set x = a+-b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{ID_VAL,"a"},
        {{UN_OP,"-"},{ID_VAL,"b"}}}}},
      "Precedence check: binary +, unary -")
    checkParse(t, "set x = +a+b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{{UN_OP,"+"},
        {ID_VAL,"a"}},{ID_VAL,"b"}}}},
      "Precedence check: unary +, binary +, *")
    checkParse(t, "set x = -a+b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{{UN_OP,"-"},
        {ID_VAL,"a"}},{ID_VAL,"b"}}}},
      "Precedence check: unary -, binary +")
    checkParse(t, "set x = a-+b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},{ID_VAL,"a"},
        {{UN_OP,"+"},{ID_VAL,"b"}}}}},
      "Precedence check: binary -, unary +")
    checkParse(t, "set x = a--b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},{ID_VAL,"a"},
        {{UN_OP,"-"},{ID_VAL,"b"}}}}},
      "Precedence check: binary -, unary -")
    checkParse(t, "set x = +a-b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},{{UN_OP,"+"},
        {ID_VAL,"a"}},{ID_VAL,"b"}}}},
      "Precedence check: unary +, binary -, *")
    checkParse(t, "set x = -a-b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},{{UN_OP,"-"},
        {ID_VAL,"a"}},{ID_VAL,"b"}}}},
      "Precedence check: unary -, binary -")
    checkParse(t, "set x = a*-b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"*"},{ID_VAL,"a"},
        {{UN_OP,"-"},{ID_VAL,"b"}}}}},
      "Precedence check: *, unary -")
    checkParse(t, "set x = +a*c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"*"},{{UN_OP,"+"},
        {ID_VAL,"a"}},{ID_VAL,"c"}}}},
      "Precedence check: unary +, *")
    checkParse(t, "set x = a/+b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"/"},{ID_VAL,"a"},
        {{UN_OP,"+"},{ID_VAL,"b"}}}}},
      "Precedence check: /, unary +")
    checkParse(t, "set x = -a/c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"/"},{{UN_OP,"-"},
        {ID_VAL,"a"}},{ID_VAL,"c"}}}},
      "Precedence check: unary -, /")
    checkParse(t, "set x = a%-b", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"%"},{ID_VAL,"a"},
        {{UN_OP,"-"},{ID_VAL,"b"}}}}},
      "Precedence check: %, unary -")
    checkParse(t, "set x = +a%c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"%"},{{UN_OP,"+"},
        {ID_VAL,"a"}},{ID_VAL,"c"}}}},
      "Precedence check: unary +, %")

    checkParse(t, "set x = 1+(2+3+4)+5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{{BIN_OP,"+"},
        {NUMLIT_VAL,"1"},{{BIN_OP,"+"},{{BIN_OP,"+"},{NUMLIT_VAL,"2"},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}}},{NUMLIT_VAL,"5"}}}},
      "Associativity override: binary +")
    checkParse(t, "set x = 1-(2-3-4)-5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},{{BIN_OP,"-"},
        {NUMLIT_VAL,"1"},{{BIN_OP,"-"},{{BIN_OP,"-"},{NUMLIT_VAL,"2"},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}}},{NUMLIT_VAL,"5"}}}},
      "Associativity override: binary -")
    checkParse(t, "set x = 1*(2*3*4)*5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"*"},{{BIN_OP,"*"},
        {NUMLIT_VAL,"1"},{{BIN_OP,"*"},{{BIN_OP,"*"},{NUMLIT_VAL,"2"},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}}},{NUMLIT_VAL,"5"}}}},
      "Associativity override: *")
    checkParse(t, "set x = 1/(2/3/4)/5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"/"},{{BIN_OP,"/"},
        {NUMLIT_VAL,"1"},{{BIN_OP,"/"},{{BIN_OP,"/"},{NUMLIT_VAL,"2"},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}}},{NUMLIT_VAL,"5"}}}},
      "Associativity override: /")
    checkParse(t, "set x = 1%(2%3%4)%5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"%"},{{BIN_OP,"%"},
        {NUMLIT_VAL,"1"},{{BIN_OP,"%"},{{BIN_OP,"%"},{NUMLIT_VAL,"2"},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}}},{NUMLIT_VAL,"5"}}}},
      "Associativity override: %")
    checkParse(t, "set x = 1==(2==3==4)==5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},{{BIN_OP,"=="},
        {NUMLIT_VAL,"1"},{{BIN_OP,"=="},{{BIN_OP,"=="},{NUMLIT_VAL,"2"},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}}},{NUMLIT_VAL,"5"}}}},
      "Associativity override: ==")
    checkParse(t, "set x = 1!=(2!=3!=4)!=5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"!="},{{BIN_OP,"!="},
        {NUMLIT_VAL,"1"},{{BIN_OP,"!="},{{BIN_OP,"!="},{NUMLIT_VAL,"2"},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}}},{NUMLIT_VAL,"5"}}}},
      "Associativity override: !=")
    checkParse(t, "set x = 1<(2<3<4)<5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"<"},{{BIN_OP,"<"},
        {NUMLIT_VAL,"1"},{{BIN_OP,"<"},{{BIN_OP,"<"},{NUMLIT_VAL,"2"},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}}},{NUMLIT_VAL,"5"}}}},
      "Associativity override: <")
    checkParse(t, "set x = 1<=(2<=3<=4)<=5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"<="},{{BIN_OP,"<="},
        {NUMLIT_VAL,"1"},{{BIN_OP,"<="},{{BIN_OP,"<="},{NUMLIT_VAL,"2"},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}}},{NUMLIT_VAL,"5"}}}},
      "Associativity override: <=")
    checkParse(t, "set x = 1>(2>3>4)>5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">"},{{BIN_OP,">"},
        {NUMLIT_VAL,"1"},{{BIN_OP,">"},{{BIN_OP,">"},{NUMLIT_VAL,"2"},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}}},{NUMLIT_VAL,"5"}}}},
      "Associativity override: >")
    checkParse(t, "set x = 1>=(2>=3>=4)>=5", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">="},{{BIN_OP,">="},
        {NUMLIT_VAL,"1"},{{BIN_OP,">="},{{BIN_OP,">="},{NUMLIT_VAL,"2"},
        {NUMLIT_VAL,"3"}},{NUMLIT_VAL,"4"}}},{NUMLIT_VAL,"5"}}}},
      "Associativity override: >=")

    checkParse(t, "set x = (a==b)+c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{{BIN_OP,"=="},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence override: ==, binary +")
    checkParse(t, "set x = (a!=b)-c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"-"},{{BIN_OP,"!="},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence override: !=, binary -")
    checkParse(t, "set x = (a<b)*c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"*"},{{BIN_OP,"<"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence override: <, *")
    checkParse(t, "set x = (a<=b)/c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"/"},{{BIN_OP,"<="},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence override: <=, /")
    checkParse(t, "set x = (a>b)%c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"%"},{{BIN_OP,">"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence override: >, %")
    checkParse(t, "set x = a+(b>=c)", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"+"},{ID_VAL,"a"},
        {{BIN_OP,">="},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence override: binary +, >=")
    checkParse(t, "set x = (a-b)*c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"*"},{{BIN_OP,"-"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence override: binary -, *")
    checkParse(t, "set x = (a+b)%c", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"%"},{{BIN_OP,"+"},
        {ID_VAL,"a"},{ID_VAL,"b"}},{ID_VAL,"c"}}}},
      "Precedence override: binary +, %")
    checkParse(t, "set x = a*(b==c)", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"*"},{ID_VAL,"a"},
        {{BIN_OP,"=="},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence override: *, ==")
    checkParse(t, "set x = a/(b!=c)", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"/"},{ID_VAL,"a"},
        {{BIN_OP,"!="},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence override: /, !=")
    checkParse(t, "set x = a%(b<c)", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"%"},{ID_VAL,"a"},
        {{BIN_OP,"<"},{ID_VAL,"b"},{ID_VAL,"c"}}}}},
      "Precedence override: %, <")

    checkParse(t, "set x = +(a<=b)", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{UN_OP,"+"},{{BIN_OP,"<="},
        {ID_VAL,"a"},{ID_VAL,"b"}}}}},
      "Precedence override: unary +, <=")
    checkParse(t, "set x = -(a>b)", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{UN_OP,"-"},{{BIN_OP,">"},
        {ID_VAL,"a"},{ID_VAL,"b"}}}}},
      "Precedence override: unary -, >")
    checkParse(t, "set x = +(a+b)", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{UN_OP,"+"},{{BIN_OP,"+"},
        {ID_VAL,"a"},{ID_VAL,"b"}}}}},
      "Precedence override: unary +, binary +")
    checkParse(t, "set x = -(a-b)", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{UN_OP,"-"},{{BIN_OP,"-"},
        {ID_VAL,"a"},{ID_VAL,"b"}}}}},
      "Precedence override: unary -, binary -")
    checkParse(t, "set x = +(a*b)", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{UN_OP,"+"},{{BIN_OP,"*"},
        {ID_VAL,"a"},{ID_VAL,"b"}}}}},
      "Precedence override: unary +, *")
    checkParse(t, "set x = -(a/b)", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{UN_OP,"-"},{{BIN_OP,"/"},
        {ID_VAL,"a"},{ID_VAL,"b"}}}}},
      "Precedence override: unary -, /")
    checkParse(t, "set x = +(a%b)", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{UN_OP,"+"},{{BIN_OP,"%"},
        {ID_VAL,"a"},{ID_VAL,"b"}}}}},
      "Precedence override: unary +, %")
end


function test_expr_complex(t)
    io.write("Test Suite: complex expressions\n")

    checkParse(t, "set x = ((((((((((((((((((((((((((((((((((((((((a)))"
      ..")))))))))))))))))))))))))))))))))))))", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{ID_VAL,"a"}}},
      "Complex expression: many parens")
    checkParse(t, "set x = (((((((((((((((((((((((((((((((((((((((a))))"
      .."))))))))))))))))))))))))))))))))))))", false, nil,
      "Bad complex expression: many parens, mismatch #1")
    checkParse(t, "set x = ((((((((((((((((((((((((((((((((((((((((a)))"
      .."))))))))))))))))))))))))))))))))))))", false, nil,
      "Bad complex expression: many parens, mismatch #2")
    checkParse(t, "set x = a==b+c[x-y[2]]*+d!=e-f/-g<h+i%+j", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"<"},{{BIN_OP,"!="},
        {{BIN_OP,"=="},{ID_VAL,"a"},{{BIN_OP,"+"},{ID_VAL,"b"},
        {{BIN_OP,"*"},{ARRAY_REF,{ID_VAL,"c"},{{BIN_OP,"-"},
        {ID_VAL,"x"},{ARRAY_REF,{ID_VAL,"y"},{NUMLIT_VAL,"2"}}}},
        {{UN_OP,"+"},{ID_VAL,"d"}}}}},{{BIN_OP,"-"},{ID_VAL,"e"},
        {{BIN_OP,"/"},{ID_VAL,"f"},{{UN_OP,"-"},{ID_VAL,"g"}}}}},
        {{BIN_OP,"+"},{ID_VAL,"h"},{{BIN_OP,"%"},{ID_VAL,"i"},
        {{UN_OP,"+"},{ID_VAL,"j"}}}}}}},
      "Complex expression: misc #1")
    checkParse(t, "set x = a==b+(c*+(d!=e[z]-f/-g)<h+i)%+j", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,"=="},{ID_VAL,"a"},
      {{BIN_OP,"+"},{ID_VAL,"b"},{{BIN_OP,"%"},{{BIN_OP,"<"},
      {{BIN_OP,"*"},{ID_VAL,"c"},{{UN_OP,"+"},{{BIN_OP,"!="},
      {ID_VAL,"d"},{{BIN_OP,"-"},{ARRAY_REF,{ID_VAL,"e"},{ID_VAL,"z"}},
      {{BIN_OP,"/"},{ID_VAL,"f"},{{UN_OP,"-"},{ID_VAL,"g"}}}}}}},
      {{BIN_OP,"+"},{ID_VAL,"h"},{ID_VAL,"i"}}},{{UN_OP,"+"},
      {ID_VAL,"j"}}}}}}},
      "Complex expression: misc #2")
    checkParse(t, "set x = a[x[y[z]]%4]++b*c<=d--e/f>g+-h%i>=j", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">="},{{BIN_OP,">"},
        {{BIN_OP,"<="},{{BIN_OP,"+"},{ARRAY_REF,{ID_VAL,"a"},
        {{BIN_OP,"%"},{ARRAY_REF,{ID_VAL,"x"},{ARRAY_REF,{ID_VAL,"y"},
        {ID_VAL,"z"}}},{NUMLIT_VAL,"4"}}},{{BIN_OP,"*"},{{UN_OP,"+"},
        {ID_VAL,"b"}},{ID_VAL,"c"}}},{{BIN_OP,"-"},{ID_VAL,"d"},
        {{BIN_OP,"/"},{{UN_OP,"-"},{ID_VAL,"e"}},{ID_VAL,"f"}}}},
        {{BIN_OP,"+"},{ID_VAL,"g"},{{BIN_OP,"%"},{{UN_OP,"-"},
        {ID_VAL,"h"}},{ID_VAL,"i"}}}},{ID_VAL,"j"}}}},
      "Complex expression: misc #3")
    checkParse(t, "set x = a++(b*c<=d)--e/(f>g+-h%i)>=j[-z]", true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{{BIN_OP,">="},{{BIN_OP,"-"},
        {{BIN_OP,"+"},{ID_VAL,"a"},{{UN_OP,"+"},{{BIN_OP,"<="},
        {{BIN_OP,"*"},{ID_VAL,"b"},{ID_VAL,"c"}},{ID_VAL,"d"}}}},
        {{BIN_OP,"/"},{{UN_OP,"-"},{ID_VAL,"e"}},{{BIN_OP,">"},
        {ID_VAL,"f"},{{BIN_OP,"+"},{ID_VAL,"g"},{{BIN_OP,"%"},
        {{UN_OP,"-"},{ID_VAL,"h"}},{ID_VAL,"i"}}}}}},{ARRAY_REF,
        {ID_VAL,"j"},{{UN_OP,"-"},{ID_VAL,"z"}}}}}},
      "Complex expression: misc #4")
    checkParse(t, "set x = a==b+c*+d!=e-/-g<h+i%+j", false, nil,
      "Bad complex expression: misc #1")
    checkParse(t, "set x = a==b+(c*+(d!=e-f/-g)<h+i)%+", false, nil,
      "Bad complex expression: misc #2")
    checkParse(t, "set x = a++b*c<=d--e x/f>g+-h%i>=j", false, nil,
      "Bad complex expression: misc #3")
    checkParse(t, "set x = a++b*c<=d)--e/(f>g+-h%i)>=j", false, nil,
      "Bad complex expression: misc #4")

    checkParse(t, "set x = ((a[(b[c[(d[((e[f]))])]])]))", true,
        {STMT_LIST,{SET_STMT,{ID_VAL,"x"},{ARRAY_REF,{ID_VAL,"a"},
          {ARRAY_REF,{ID_VAL,"b"},{ARRAY_REF,{ID_VAL,"c"},{ARRAY_REF,
          {ID_VAL,"d"},{ARRAY_REF,{ID_VAL,"e"},{ID_VAL,"f"}}}}}}}},
      "Complex expression: many parens/brackets")
    checkParse(t, "set x = ((a[(b[c[(d[((e[f]))]])])]))", false, nil,
      "Bad complex expression: mismatched parens/brackets")
end


function test_print_stmt(t)
    io.write("Test Suite: print statements\n")

    checkParse(t, "print 'abc'", true,
      {STMT_LIST,{PRINT_STMT,{STRLIT_VAL,"'abc'"}}},
      "Print statement: StringLiteral")
    checkParse(t, "print x", true,
      {STMT_LIST,{PRINT_STMT,{ID_VAL,"x"}}},
      "Print statement: variable")
    checkParse(t, "print a+x[b*(c==d-f)]%g<=h", true,
      {STMT_LIST,{PRINT_STMT,{{BIN_OP,"<="},{{BIN_OP,"+"},{ID_VAL,"a"},
        {{BIN_OP,"%"},{ARRAY_REF,{ID_VAL,"x"},{{BIN_OP,"*"},
        {ID_VAL,"b"},{{BIN_OP,"=="},{ID_VAL,"c"},{{BIN_OP,"-"},
        {ID_VAL,"d"},{ID_VAL,"f"}}}}},{ID_VAL,"g"}}},{ID_VAL,"h"}}}},
      "Print statement: expression")
    checkParse(t, "print", false, nil,
      "Bad print statement: empty")
    checkParse(t, "print end", false, nil,
      "Bad print statement: keyword")
    checkParse(t, "print 1 end", false, nil,
      "Bad print statement: followed by end")
end


function test_nl_stmt(t)
    io.write("Test Suite: nl statements\n")

    checkParse(t, "nl", true,
      {STMT_LIST,{NL_STMT}},
      "Nl statement: one")
    checkParse(t, "nl nl nl nl nl nl", true,
      {STMT_LIST,{NL_STMT},{NL_STMT},{NL_STMT},{NL_STMT},{NL_STMT},
        {NL_STMT}},
      "Nl statement: multiple")
    checkParse(t, "nl 1", false, nil,
      "Bad nl statement")
    checkParse(t, "nl end", false, nil,
      "Bad nl statement: followed by end")
    checkParse(t, "print \"x\" nl", true,
        {STMT_LIST,{PRINT_STMT,{STRLIT_VAL,"\"x\""}},{NL_STMT}},
      "Print + nl")
end


function test_input_stmt(t)
    io.write("Test Suite: input statements\n")

    checkParse(t, "input x", true,
      {STMT_LIST,{INPUT_STMT,{ID_VAL,"x"}}},
      "Input statement: simple")
    checkParse(t, "input x[1]", true,
      {STMT_LIST,{INPUT_STMT,{ARRAY_REF,{ID_VAL,"x"},
        {NUMLIT_VAL,"1"}}}},
      "Input statement: array ref")
    checkParse(t, "input x[(a==b[c[d]])+e[3e7%5]]", true,
      {STMT_LIST,{INPUT_STMT,{ARRAY_REF,{ID_VAL,"x"},{{BIN_OP,"+"},
        {{BIN_OP,"=="},{ID_VAL,"a"},{ARRAY_REF,{ID_VAL,"b"},{ARRAY_REF,
        {ID_VAL,"c"},{ID_VAL,"d"}}}},{ARRAY_REF,{ID_VAL,"e"},
        {{BIN_OP,"%"},{NUMLIT_VAL,"3e7"},{NUMLIT_VAL,"5"}}}}}}},
      "Input statement, complex array ref")
    checkParse(t, "input", false, nil,
      "Bad input statement: no lvalue")
    checkParse(t, "input a b", false, nil,
      "Bad input statement: two lvalues")
    checkParse(t, "input end", false, nil,
      "Bad input statement: keyword")
    checkParse(t, "input (x)", false, nil,
      "Bad input statement: var in parens")
    checkParse(t, "input (x[1])", false, nil,
      "Bad input statement: array ref in parens")
end


function test_if_stmt(t)
    io.write("Test Suite: if statements\n")

    checkParse(t, "if a nl end", true,
      {STMT_LIST,{IF_STMT,{ID_VAL,"a"},{STMT_LIST,{NL_STMT}}}},
      "If statement: simple")
    checkParse(t, "if a end", true,
      {STMT_LIST,{IF_STMT,{ID_VAL,"a"},{STMT_LIST}}},
      "If statement: empty statement list")
    checkParse(t, "if a nl else nl nl end", true,
      {STMT_LIST,{IF_STMT,{ID_VAL,"a"},{STMT_LIST,{NL_STMT}},{STMT_LIST,
        {NL_STMT},{NL_STMT}}}},
      "If statement: else")
    checkParse(t, "if a nl elseif b nl nl else nl nl nl end", true,
      {STMT_LIST,{IF_STMT,{ID_VAL,"a"},{STMT_LIST,{NL_STMT}},
        {ID_VAL,"b"},{STMT_LIST,{NL_STMT},{NL_STMT}},{STMT_LIST,
        {NL_STMT},{NL_STMT},{NL_STMT}}}},
      "If statement: elseif, else")
    checkParse(t, "if a nl elseif b nl nl elseif c nl nl nl elseif d "
      .."nl nl nl nl elseif e nl nl nl nl nl else nl nl nl nl nl nl "
      .."end", true,
      {STMT_LIST,{IF_STMT,{ID_VAL,"a"},{STMT_LIST,{NL_STMT}},
        {ID_VAL,"b"},{STMT_LIST,{NL_STMT},{NL_STMT}},{ID_VAL,"c"},
        {STMT_LIST,{NL_STMT},{NL_STMT},{NL_STMT}},{ID_VAL,"d"},
        {STMT_LIST,{NL_STMT},{NL_STMT},{NL_STMT},{NL_STMT}},
        {ID_VAL,"e"},{STMT_LIST,{NL_STMT},{NL_STMT},{NL_STMT},{NL_STMT},
        {NL_STMT}},{STMT_LIST,{NL_STMT},{NL_STMT},{NL_STMT},{NL_STMT},
        {NL_STMT},{NL_STMT}}}},
      "If statement: multiple elseif, else")
    checkParse(t, "if a nl elseif b nl nl elseif c nl nl nl elseif d "
      .."nl nl nl nl elseif e nl nl nl nl nl end", true,
      {STMT_LIST,{IF_STMT,{ID_VAL,"a"},{STMT_LIST,{NL_STMT}},
        {ID_VAL,"b"},{STMT_LIST,{NL_STMT},{NL_STMT}},{ID_VAL,"c"},
        {STMT_LIST,{NL_STMT},{NL_STMT},{NL_STMT}},{ID_VAL,"d"},
        {STMT_LIST,{NL_STMT},{NL_STMT},{NL_STMT},{NL_STMT}},
        {ID_VAL,"e"},{STMT_LIST,{NL_STMT},{NL_STMT},{NL_STMT},{NL_STMT},
        {NL_STMT}}}},
      "If statement: multiple elseif, no else")
    checkParse(t, "if a elseif b elseif c elseif d elseif e else end",
      true,
      {STMT_LIST,{IF_STMT,{ID_VAL,"a"},{STMT_LIST},{ID_VAL,"b"},
        {STMT_LIST},{ID_VAL,"c"},{STMT_LIST},{ID_VAL,"d"},{STMT_LIST},
        {ID_VAL,"e"},{STMT_LIST},{STMT_LIST}}},
      "If statement: multiple elseif, else, empty statement lists")
    checkParse(t, "if a if b nl else nl end elseif c if d nl "
      .."else nl end else if e nl else nl end end", true,
      {STMT_LIST,{IF_STMT,{ID_VAL,"a"},{STMT_LIST,{IF_STMT,{ID_VAL,"b"},
        {STMT_LIST,{NL_STMT}},{STMT_LIST,{NL_STMT}}}},{ID_VAL,"c"},
        {STMT_LIST,{IF_STMT,{ID_VAL,"d"},{STMT_LIST,{NL_STMT}},
        {STMT_LIST,{NL_STMT}}}},{STMT_LIST,{IF_STMT,{ID_VAL,"e"},
        {STMT_LIST,{NL_STMT}},{STMT_LIST,{NL_STMT}}}}}},
      "If statement: nested #1")
    checkParse(t, "if a if b if c if d if e if f if g nl end end end "
      .."end end end end", true,
      {STMT_LIST,{IF_STMT,{ID_VAL,"a"},{STMT_LIST,{IF_STMT,{ID_VAL,"b"},
        {STMT_LIST,{IF_STMT,{ID_VAL,"c"},{STMT_LIST,{IF_STMT,
        {ID_VAL,"d"},{STMT_LIST,{IF_STMT,{ID_VAL,"e"},{STMT_LIST,
        {IF_STMT,{ID_VAL,"f"},{STMT_LIST,{IF_STMT,{ID_VAL,"g"},
        {STMT_LIST,{NL_STMT}}}}}}}}}}}}}}}},
      "If statement: nested #2")

    checkParse(t, "if nl end", false, nil,
      "Bad if statement: no expr")
    checkParse(t, "if a nl", false, nil,
      "Bad if statement: no end")
    checkParse(t, "if a b nl end", false, nil,
      "Bad if statement: 2 expressions")
    checkParse(t, "if a nl else nl elseif b nl", false, nil,
      "Bad if statement: else before elseif")
    checkParse(t, "if a nl end end", false, nil,
      "Bad if statement: followed by end")
end


function test_while_stmt(t)
    io.write("Test Suite: while statements\n")

    checkParse(t, "while a nl end", true,
      {STMT_LIST,{WHILE_STMT,{ID_VAL,"a"},{STMT_LIST,{NL_STMT}}}},
      "While statement: simple")
    checkParse(t, "while a nl nl nl nl nl nl nl nl nl nl end", true,
      {STMT_LIST,{WHILE_STMT,{ID_VAL,"a"},{STMT_LIST,{NL_STMT},
        {NL_STMT},{NL_STMT},{NL_STMT},{NL_STMT},{NL_STMT},{NL_STMT},
        {NL_STMT},{NL_STMT},{NL_STMT}}}},
      "While statement: longer statement list")
    checkParse(t, "while a end", true,
      {STMT_LIST,{WHILE_STMT,{ID_VAL,"a"},{STMT_LIST}}},
      "While statement: empty statement list")
    checkParse(t, "while a while b while c while d while e while f "
      .."while g nl end end end end end end end", true,
      {STMT_LIST,{WHILE_STMT,{ID_VAL,"a"},{STMT_LIST,{WHILE_STMT,
        {ID_VAL,"b"},{STMT_LIST,{WHILE_STMT,{ID_VAL,"c"},{STMT_LIST,
        {WHILE_STMT,{ID_VAL,"d"},{STMT_LIST,{WHILE_STMT,{ID_VAL,"e"},
        {STMT_LIST,{WHILE_STMT,{ID_VAL,"f"},{STMT_LIST,{WHILE_STMT,
        {ID_VAL,"g"},{STMT_LIST,{NL_STMT}}}}}}}}}}}}}}}},
      "While statement: nested")
    checkParse(t, "while a if b while c end elseif d while e if f end "
      .."end elseif g while h end else while i end end end", true,
      {STMT_LIST,{WHILE_STMT,{ID_VAL,"a"},{STMT_LIST,{IF_STMT,
        {ID_VAL,"b"},{STMT_LIST,{WHILE_STMT,{ID_VAL,"c"},{STMT_LIST}}},
        {ID_VAL,"d"},{STMT_LIST,{WHILE_STMT,{ID_VAL,"e"},{STMT_LIST,
        {IF_STMT,{ID_VAL,"f"},{STMT_LIST}}}}},{ID_VAL,"g"},{STMT_LIST,
        {WHILE_STMT,{ID_VAL,"h"},{STMT_LIST}}},{STMT_LIST,{WHILE_STMT,
        {ID_VAL,"i"},{STMT_LIST}}}}}}},
      "While statement: nested while & if")

    checkParse(t, "while nl end", false, nil,
      "Bad while statement: no expr")
    checkParse(t, "while a nl", false, nil,
      "Bad while statement: no end")
    checkParse(t, "while a nl else nl end ", false, nil,
      "Bad while statement: has else")
    checkParse(t, "while a nl end end", false, nil,
      "Bad while statement: followed by end")
end


function test_prog(t)
    io.write("Test Suite: complete programs\n")

    -- Example #1 from Assignment 4 description
    checkParse(t,
      [[#
        # Zebu Example #1
        # By GGC 2012-02-18
        set k = 3
        print k nl
      ]], true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"k"},{NUMLIT_VAL,"3"}},{PRINT_STMT,
        {ID_VAL,"k"}},{NL_STMT}},
      "Program: Example #1 from Assignment 4 description")

    -- Example #2 from Assignment 4 description
    checkParse(t,
      [[#
        # Zebu Example #2
        # By GGC 2012-02-18
        # Print all Fibonacci numbers less than 1000

        print "Fibonacci Numbers" nl
        set prev = 1   # Previous Fibo
        set curr = 0   # Current Fibo
        set which = 0  # Which Fibo are we printing
        while curr < 1000
            print "F("
            print which
            print ") = "
            print curr nl

            set next = prev + curr
            set prev = curr
            set curr = next

            set which = which + 1
        end
      ]], true,
      {STMT_LIST,{PRINT_STMT,{STRLIT_VAL,"\"Fibonacci Numbers\""}},
        {NL_STMT},{SET_STMT,{ID_VAL,"prev"},{NUMLIT_VAL,"1"}},{SET_STMT,
        {ID_VAL,"curr"},{NUMLIT_VAL,"0"}},{SET_STMT,{ID_VAL,"which"},
        {NUMLIT_VAL,"0"}},{WHILE_STMT,{{BIN_OP,"<"},{ID_VAL,"curr"},
        {NUMLIT_VAL,"1000"}},{STMT_LIST,{PRINT_STMT,
        {STRLIT_VAL,"\"F(\""}},{PRINT_STMT,{ID_VAL,"which"}},
        {PRINT_STMT,{STRLIT_VAL,"\") = \""}},{PRINT_STMT,
        {ID_VAL,"curr"}},{NL_STMT},{SET_STMT,{ID_VAL,"next"},
        {{BIN_OP,"+"},{ID_VAL,"prev"},{ID_VAL,"curr"}}},{SET_STMT,
        {ID_VAL,"prev"},{ID_VAL,"curr"}},{SET_STMT,{ID_VAL,"curr"},
        {ID_VAL,"next"}},{SET_STMT,{ID_VAL,"which"},{{BIN_OP,"+"},
        {ID_VAL,"which"},{NUMLIT_VAL,"1"}}}}}},
      "Program: Example #1 from Assignment 4 description")

    -- Input number, print its square
    checkParse(t,
      [[#
        print 'Type a number: '
        input n
        nl nl
        print 'You typed: '
        print a nl
        print 'Its square is: '
        print a*a nl
        nl
      ]], true,
      {STMT_LIST,{PRINT_STMT,{STRLIT_VAL,"'Type a number: '"}},
        {INPUT_STMT,{ID_VAL,"n"}},{NL_STMT},{NL_STMT},{PRINT_STMT,
        {STRLIT_VAL,"'You typed: '"}},{PRINT_STMT,{ID_VAL,"a"}},
        {NL_STMT},{PRINT_STMT,{STRLIT_VAL,"'Its square is: '"}},
        {PRINT_STMT,{{BIN_OP,"*"},{ID_VAL,"a"},{ID_VAL,"a"}}},{NL_STMT},
        {NL_STMT}},
      "Program: Input number, print its square")

    -- Input numbers, stop at sentinel, print even/odd
    checkParse(t,
      [[#
        set continue = 1
        while continue
            print 'Type a number (0 to end): '
            input n
            nl nl
            if n == 0
                set continue = 0
            else
                print 'The number '
                print n
                print ' is '
                if n % 2 == 0
                    print 'even'
                else
                    print 'odd'
                end
                nl nl
            end
        end
        print 'Bye!' nl
        nl
      ]], true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"continue"},{NUMLIT_VAL,"1"}},
        {WHILE_STMT,{ID_VAL,"continue"},{STMT_LIST,{PRINT_STMT,
        {STRLIT_VAL,"'Type a number (0 to end): '"}},{INPUT_STMT,
        {ID_VAL,"n"}},{NL_STMT},{NL_STMT},{IF_STMT,{{BIN_OP,"=="},
        {ID_VAL,"n"},{NUMLIT_VAL,"0"}},{STMT_LIST,{SET_STMT,
        {ID_VAL,"continue"},{NUMLIT_VAL,"0"}}},{STMT_LIST,{PRINT_STMT,
        {STRLIT_VAL,"'The number '"}},{PRINT_STMT,{ID_VAL,"n"}},
        {PRINT_STMT,{STRLIT_VAL,"' is '"}},{IF_STMT,{{BIN_OP,"=="},
        {{BIN_OP,"%"},{ID_VAL,"n"},{NUMLIT_VAL,"2"}},{NUMLIT_VAL,"0"}},
        {STMT_LIST,{PRINT_STMT,{STRLIT_VAL,"'even'"}}},{STMT_LIST,
        {PRINT_STMT,{STRLIT_VAL,"'odd'"}}}},{NL_STMT},{NL_STMT}}}}},
        {PRINT_STMT,{STRLIT_VAL,"'Bye!'"}},{NL_STMT},{NL_STMT}},
      "Program: Input numbers, stop at sentinel, print even/odd")

    -- Input 10 numbers, print them in reverse order
    checkParse(t,
      [[#
        set howMany = 10  # How many numbers to input
        print 'I will ask you for '
        print howMany
        print ' values (numbers).' nl
        print 'Then I will print them in reverse order.' nl
        nl
        set i = 1
        while i <= howMany  # Input loop
            print 'Type value #'
            print i
            print ': '
            input v[i]
            nl nl
            set i = i+1
        end
        print '----------------------------------------' nl
        nl
        print 'Here are the values, in reverse order:' nl
        set i = howMany
        while i > 0  # Output loop
            print 'Value #'
            print i
            print ': '
            print v[i]
            nl
            set i = i-1
        end
        nl
      ]], true,
      {STMT_LIST,{SET_STMT,{ID_VAL,"howMany"},{NUMLIT_VAL,"10"}},
        {PRINT_STMT,{STRLIT_VAL,"'I will ask you for '"}},{PRINT_STMT,
        {ID_VAL,"howMany"}},{PRINT_STMT,
        {STRLIT_VAL,"' values (numbers).'"}},{NL_STMT},{PRINT_STMT,
        {STRLIT_VAL,"'Then I will print them in reverse order.'"}},
        {NL_STMT},{NL_STMT},{SET_STMT,{ID_VAL,"i"},{NUMLIT_VAL,"1"}},
        {WHILE_STMT,{{BIN_OP,"<="},{ID_VAL,"i"},{ID_VAL,"howMany"}},
        {STMT_LIST,{PRINT_STMT,{STRLIT_VAL,"'Type value #'"}},
        {PRINT_STMT,{ID_VAL,"i"}},{PRINT_STMT,{STRLIT_VAL,"': '"}},
        {INPUT_STMT,{ARRAY_REF,{ID_VAL,"v"},{ID_VAL,"i"}}},{NL_STMT},
        {NL_STMT},{SET_STMT,{ID_VAL,"i"},{{BIN_OP,"+"},{ID_VAL,"i"},
        {NUMLIT_VAL,"1"}}}}},{PRINT_STMT,
        {STRLIT_VAL,"'----------------------------------------'"}},
        {NL_STMT},{NL_STMT},{PRINT_STMT,
        {STRLIT_VAL,"'Here are the values, in reverse order:'"}},
        {NL_STMT},{SET_STMT,{ID_VAL,"i"},{ID_VAL,"howMany"}},
        {WHILE_STMT,{{BIN_OP,">"},{ID_VAL,"i"},{NUMLIT_VAL,"0"}},
        {STMT_LIST,{PRINT_STMT,{STRLIT_VAL,"'Value #'"}},{PRINT_STMT,
        {ID_VAL,"i"}},{PRINT_STMT,{STRLIT_VAL,"': '"}},{PRINT_STMT,
        {ARRAY_REF,{ID_VAL,"v"},{ID_VAL,"i"}}},{NL_STMT},{SET_STMT,
        {ID_VAL,"i"},{{BIN_OP,"-"},{ID_VAL,"i"},{NUMLIT_VAL,"1"}}}}},
        {NL_STMT}},
      "Program: Input 10 numbers, print them in reverse order")

    -- Long program
    howmany = 50
    progpiece = "print 42\n"
    prog = progpiece:rep(howmany)
    ast = {STMT_LIST}
    astpiece = {PRINT_STMT,{NUMLIT_VAL,"42"}}
    for i = 1, howmany do
        table.insert(ast, astpiece)
    end
    checkParse(t, prog, true,
      ast,
      "Program: Long program")


    -- Very long program
    howmany = 10000
    progpiece = "input x print x nl\n"
    prog = progpiece:rep(howmany)
    ast = {STMT_LIST}
    astpiece1 = {INPUT_STMT,{ID_VAL,"x"}}
    astpiece2 = {PRINT_STMT,{ID_VAL,"x"}}
    astpiece3 = {NL_STMT}
    for i = 1, howmany do
        table.insert(ast, astpiece1)
        table.insert(ast, astpiece2)
        table.insert(ast, astpiece3)
    end
    checkParse(t, prog, true,
      ast,
      "Program: Very long program")
end


function test_parseit(t)
    io.write("TEST SUITES FOR MODULE parseit\n")
    test_simple(t)
    test_set_stmt(t)
    test_expr_simple(t)
    test_expr_prec_assoc(t)
    test_expr_complex(t)
    test_print_stmt(t)
    test_nl_stmt(t)
    test_input_stmt(t)
    test_if_stmt(t)
    test_while_stmt(t)
    test_prog(t)
end


-- *********************************************************************
-- Main Program
-- *********************************************************************


test_parseit(tester)
io.write("\n")
if tester:allPassed() then
    io.write("All tests successful\n")
else
    io.write("Tests ********** UNSUCCESSFUL **********\n")
    io.write("\n")
    io.write("******************************************************\n")
    io.write("*                                                    *\n")
    io.write("*  NOTE: This program is set to execute all tests.   *\n")
    io.write("*        If you would prefer to stop after the       *\n")
    io.write("*        first failing test and see detailed         *\n")
    io.write("*        results, then set variable exit_on_failure  *\n")
    io.write("*        to true. (See the beginning of the source   *\n")
    io.write("*        code.)                                      *\n")
    io.write("*                                                    *\n")
    io.write("******************************************************\n")
end
io.write("\n")

