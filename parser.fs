//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid simple C program.
//
// Syed Amaan Shah
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // matchToken
  //
  let private matchToken expected_token tokens =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)



  // exprOp
  // <expr-op> -> +
  //           | -
  //           | *
  //           | /
  //           | ^
  //           | <
  //           | <=
  //           | >
  //           | >=
  //           | ==
  //           | !=
  let rec private exprOp tokens =
    let next = List.head tokens
    
    if next = "+" || next = "-" || next = "*" || next = "/" || next = "^" || next = "<" || next = "<=" || next = ">" || next = ">=" || next = "==" || next = "!=" then
      let T1 = matchToken next tokens
      T1
    else
      failwith ("expecting expression operator, but found " + next)


  // exprValue
  // <expr-value> -> identifier
  //               | int_literal
  //               | str_literal
  //               | true
  //               | false
  //
  let rec private exprValue (tokens : string list) =
    let next = List.head tokens

    if next.StartsWith("identifier") then
      let T1 = matchToken next tokens
      T1
    elif next.StartsWith("int_literal") then
      let T1 = matchToken next tokens
      T1
    elif next.StartsWith("str_literal") then
      let T1 = matchToken next tokens
      T1
    elif next = "true" || next = "false" then
      let T1 = matchToken next tokens
      T1
    else
      failwith ("expecting identifier or literal, but found " + next)


  // expr
  // <expr> -> <expr-value> <expr-op> <expr-value>
  //         | <expr-value>
  //
  let rec private expr tokens =
    let T1 = exprValue tokens
    let next = List.head T1

    if next = "+" || next = "-" || next = "*" || next = "/" || next = "^" || next = "<" || next = "<=" || next = ">" || next = ">=" || next = "==" || next = "!=" then
      let T2 = exprOp T1
      let T3 = exprValue T2
      T3
    else
      T1

  // condition
  // <condition> -> <expr>
  //
  let private condition tokens = 
    let T1 = expr tokens
    T1

  // outputValue
  // <output-value> -> <expr-value>
  //                   | endl
  //
  let rec private outputValue tokens =
    let next = List.head tokens
    if next = "endl" then
      let T1 = matchToken "endl" tokens
      T1
    else
      let T1 = exprValue tokens
      T1

  // empty
  // <empty> -> ;
  //
  let rec private empty tokens =
    let T1 = matchToken ";" tokens
    T1

  // vardecl
  // <vardecl> -> int identifier;
  //
  let rec private vardecl tokens =
    let T1 = matchToken "int" tokens
    
    let next  = List.head T1
    if next.StartsWith("identifier") then 
      let T2 = matchToken next T1
      let T3 = matchToken ";" T2
      T3
    else
      failwith("expecting identifier or literal, but found " + next)

  // input
  // <input> -> cin >> identifier;
  //
  let rec private input tokens =
    let T1 = matchToken "cin" tokens
    let T2 = matchToken ">>" T1

    let next = List.head T2
    if next.StartsWith("identifier") then
      let T3 = matchToken next T2
      let T4 = matchToken ";" T3
      T4
    else
      failwith ("expecting identifier, but found " + next)

  // output
  // <output> -> cout << <output-value> ;
  //
  let rec private output tokens =
    let T1 = matchToken "cout" tokens
    let T2 = matchToken "<<" T1
    let T3 = outputValue T2
    let T4 = matchToken ";" T3
    T4

  // assignment
  // <assignment> -> identifier = <expr> ;
  //
  let rec private assignment (tokens: string list) =
    let next = List.head tokens

    if next.StartsWith("identifier") then
      let T1 = matchToken next tokens
      let T2 = matchToken "=" T1
      let T3 = expr T2
      let T4 = matchToken ";" T3
      T4
    else
      failwith ("expecting identifier or literal, but found " + next)

  

  // stmt
  // <stmt> -> <empty>
  //         | <vardecl>
  //         | <input>
  //         | <output>
  //         | <assignment>
  //         | <ifstmt>
  //
  let rec private stmt tokens =
    let next = List.head tokens

    if next = ";" then
      let T = empty tokens
      T
    elif next = "int" then
      let T1 = vardecl tokens
      T1
    elif next = "cin" then
      let T2 = input tokens
      T2
    elif next = "cout" then
      let T3 = output tokens
      T3
    elif next = "if" then
      let T4 = ifstmt tokens
      T4
    elif next.StartsWith("identifier") then
      let T5 = assignment tokens
      T5
    else
      failwith ("expecting statement, but found " + next)

  and private then_part tokens =
    let T1 = stmt tokens
    T1

  and private else_part tokens =
    let next = List.head tokens

    if next = "else" then
      let T1 = matchToken next tokens
      let T2 = stmt T1
      T2
    else
      tokens

  and private ifstmt tokens =
    let T1 = matchToken "if" tokens
    let T2 = matchToken "(" T1
    let T3 = condition T2
    let T4 = matchToken ")" T3
    let T5 = then_part T4
    let T6 = else_part T5
    T6


  // morestmts
  // <morestmts> -> <stmt> <morestmts>
  //              | EMPTY
  //
  let rec private morestmts tokens =
    let next = List.head tokens

    if next = ";" || next = "int" || next = "cin" || next = "cout" || next = "if" || next.StartsWith("identifier") then
      let T1 = stmt tokens
      let T2 = morestmts T1
      T2
    else
      tokens
    


  // stmts
  // <stmts> -> <stmt> <morestmts>
  //
  let rec private stmts tokens =
    let T1 = stmt tokens
    let T2 = morestmts T1
    T2

  

  // simpleC
  // <simpleC> -> void main ( ) { <stmts> } $
  //  
  let private simpleC tokens = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let T7 = stmts T6
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8
    T9


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "success"
    with 
      | ex -> "syntax_error: " + ex.Message
