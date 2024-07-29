//
// Parser for SimpleC programs.  This component checks 
// the input program to see if it meets the syntax rules
// of SimpleC.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid SimpleC program.
//
// <<YOUR NAME>>
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =



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

  let beginswith (pattern: string) (literal: string) =
    literal.StartsWith (pattern) 

  let private matchIdentifier tokens =
    match tokens with
    | [] -> failwith "Unexpected end of input"
    | next_token :: rest when (beginswith "identifier:" next_token ) -> rest
    | next_token :: rest when (beginswith "int_literal:" next_token ) -> rest
    | next_token :: rest when (beginswith "str_literal:" next_token ) -> rest
    | next_token :: _ -> failwith ("expecting identifier, but found " + next_token)


  let  private vardecl tokens = 
    let tokens2 = matchToken "int" tokens
    let tokens3 = matchIdentifier tokens2
    matchToken ";" tokens3



  let private input_func tokens = 
    let tokens2 = matchToken "cin" tokens
    let tokens3 = matchToken ">>" tokens2
    let tokens4 = matchIdentifier tokens3
    matchToken ";" tokens4

  let private exprValue tokens =
    match tokens with
    | hd :: _ when (beginswith "identifier:" hd) -> matchIdentifier tokens
    | hd :: _ when (beginswith "int_literal:" hd) -> matchIdentifier tokens
    | hd :: _ when (beginswith "str_literal:" hd) -> matchIdentifier tokens
    | "true" :: _ -> matchToken "true" tokens
    | "false" :: _ -> matchToken "false" tokens
    | next_token :: _ -> failwith ("expecting identifier or literal, but found " + next_token)


  let private exprOp tokens =
    match tokens with
    | "+" :: _ | "-" :: _ | "*" :: _ | "/" :: _ | "^" :: _ | "<" :: _ | "<=" :: _ | ">" :: _ | ">=" :: _ | "==" :: _ | "!=" :: _ -> 
        let tokens = matchToken (List.head tokens) tokens
        exprValue tokens
    | _ -> tokens

  let private expr tokens =
    let tokens = exprValue tokens
    exprOp tokens

  let private outputValue tokens =
    match tokens with
    | "endl" :: _ -> matchToken "endl" tokens
    | _ -> expr tokens

  let private output tokens = 
    let tokens2 = matchToken "cout" tokens
    let tokens3 = matchToken "<<" tokens2
    let tokens4 = outputValue tokens3
    matchToken ";" tokens4

  


  let private assignment tokens = 
    let tokens2 = matchIdentifier tokens
    let tokens3 = matchToken "=" tokens2
    let tokens4 = expr tokens3
    matchToken ";" tokens4
    




  let rec private stmt tokens = 
      match tokens with 
      | ";" :: tl -> matchToken ";" tokens
      | "int" :: tl ->  vardecl tokens
      | "cin" :: tl -> input_func tokens
      | "cout" :: tl -> output tokens
      | "if" :: tl -> ifstmt tokens
      | hd :: _ when beginswith "identifier:" hd -> assignment tokens
      | next_token :: _ -> failwith ("expecting statement, but found " + next_token)


  and private then_part tokens = 
    stmt tokens

  and private else_part tokens =
    let next_token = List.head tokens

    if next_token = "else" then
      let T2 = matchToken "else" tokens 
      stmt T2 
    else
      tokens 


  and ifstmt tokens = 
    let tokens2 = matchToken "if" tokens
    let tokens3 = matchToken "(" tokens2
    let tokens4 = expr tokens3
    let tokens5 = matchToken ")" tokens4
    let tokens6 = then_part tokens5
    let tokens7 = else_part tokens6
    tokens7



  let rec private morestmts tokens =
    match tokens with
    | "}" :: _ -> tokens
    | _ -> 
        let tokensAfterStmt = stmt tokens
        morestmts tokensAfterStmt



  

  

  


  // and private elsePart tokens =
  //   match tokens with
  //   | "else" :: _ -> 
  //       tokens 
  //       |> matchToken "else" 
  //       |> stmt
  //   | _ -> tokens

  


  // let private morestmts tokens =
  //   match tokens with
  //   | "}" :: _ -> tokens
  //   | _ -> stmts tokens





  let rec private stmts tokens =
    let rest_list = stmt tokens
    let rest_list2 = morestmts rest_list
    rest_list2


  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // matchToken
  //
  

  //
  // simpleC
  //
  let private simpleC tokens = 

      let T2 = matchToken "void" tokens
      let T3 = matchToken "main" T2
      let T4 = matchToken "(" T3
      let T5 = matchToken ")" T4
      let T6 = matchToken "{" T5
      let T7 = stmts T6
      let T8 = matchToken "}" T7
      let T9 = matchToken "$" T8 // $ => EOF, there should be no more tokens
      T9


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid SimpleC program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "Success!"
    with 
      | ex -> "syntax_error: " + ex.Message
