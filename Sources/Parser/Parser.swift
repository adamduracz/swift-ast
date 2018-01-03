/*
   Copyright 2016-2017 Ryuichi Laboratories and the Yanagiba project contributors

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

import AST
import Lexer
import Source

open class Parser {
  public let _sourceFile: SourceFile
  public let _lexer: Lexer

  public init(source: SourceFile) {
    _sourceFile = source
    _lexer = Lexer(source: source)
  }

  open func parse() throws -> TopLevelDeclaration {
    let topLevelDecl = try parseTopLevelDeclaration()
    topLevelDecl.setSourceFile(_sourceFile)
    return topLevelDecl
  }

  open func parseTopLevelDeclaration() throws -> TopLevelDeclaration {
    let shebang = parseShebang()
    let stmts = try parseStatements()
    return TopLevelDeclaration(statements: stmts, comments: _lexer.comments, shebang: shebang)
  }

  open func parseCodeBlock() throws -> CodeBlock {
    let startLocation = getStartLocation()
    try match(.leftBrace, orFatal: .leftBraceExpected("code block"))
    let stmts = try parseStatements()
    let endLocation = getEndLocation()
    try match(.rightBrace, orFatal: .rightBraceExpected("code block"))
    let codeBlock = CodeBlock(statements: stmts)
    codeBlock.setSourceRange(startLocation, endLocation)
    return codeBlock
  }

  open func parseStatements() throws -> Statements {
    var stmts = Statements()
    while true {
      switch _lexer.look().kind {
      case .eof, .rightBrace, .default, .case:
        return stmts
      default:
        stmts.append(try parseStatement())
      }
    }
  }

  open func parseStatement() throws -> Statement { // swift-lint:suppress(high_cyclomatic_complexity,high_ncss)
    let stmt: Statement
    let lookedRange = getLookedRange()
    switch _lexer.read([
      .for, .while, .repeat, // loop
      .if, .guard, .switch, // branch
      // identifier as labelel statement
      .defer, // defer
      .do, // do
      .break, .continue, .fallthrough, .return, .throw, // control transfer
      // compiler control
      .hash,
      // declaration statement
      // expression statement
    ]) {
    case .for:
      stmt = try parseForInStatement(startLocation: lookedRange.start)
    case .while:
      stmt = try parseWhileStatement(startLocation: lookedRange.start)
    case .repeat:
      stmt = try parseRepeatWhileStatement(startLocation: lookedRange.start)
    case .if:
      stmt = try parseIfStatement(startLocation: lookedRange.start)
    case .guard:
      stmt = try parseGuardStatement(startLocation: lookedRange.start)
    case .switch:
      stmt = try parseSwitchStatement(startLocation: lookedRange.start)
    case .break:
      stmt = parseBreakStatement(startRange: lookedRange)
    case .continue:
      stmt = parseContinueStatement(startRange: lookedRange)
    case .fallthrough:
      let fallthroughStmt = FallthroughStatement()
      fallthroughStmt.setSourceRange(lookedRange)
      stmt = fallthroughStmt
    case .return:
      stmt = try parseReturnStatement(startRange: lookedRange)
    case .throw:
      stmt = try parseThrowStatement(startLocation: lookedRange.start)
    case .defer:
      stmt = try parseDeferStatement(startLocation: lookedRange.start)
    case .do:
      stmt = try parseDoStatement(startLocation: lookedRange.start)
    case let .identifier(name, _):
      if _lexer.look(ahead: 1).kind == .colon &&
        [Token.Kind.for, .while, .repeat, .if, .switch, .do].contains(_lexer.look(ahead: 2).kind)
      {
        _lexer.advance(by: 2)
        stmt = try parseLabeledStatement(withLabelName: .name(name), startLocation: lookedRange.start)
      } else if name == "precedencegroup" {
        stmt = try parseDeclaration()
      } else {
        // if identifier is not immediately followed by a colon
        // and then one of the statement prefix keywords,
        // then we try to parase an expression that starts with this identifier
        stmt = try parseExpression()
      }
    case .hash:
      stmt = try parseCompilerControlStatement(startLocation: lookedRange.start)
    case .import, .let, .var, .typealias, .func, .enum, .indirect,
      .struct, .init, .deinit, .extension, .subscript, .operator, .protocol:
      stmt = try parseDeclaration()
    case .at:
      stmt = try parseDeclaration()
    default:
      if _lexer.look().kind.isModifier {
        stmt = try parseDeclaration()
      } else {
        stmt = try parseExpression()
      }
    }
    if !_lexer.match([.semicolon, .lineFeed, .eof]) && _lexer.look().kind != .rightBrace {
      try _raiseError(.statementSameLineWithoutSemicolon)
    }
    return stmt
  }

  open func parseIfStatement(
    startLocation: SourceLocation
  ) throws -> Statement {
    let conditionList = try parseConditionList()
    let codeBlock = try parseCodeBlock()
    guard _lexer.match(.else) else {
      let ifStmt = IfStatement(
        conditionList: conditionList, codeBlock: codeBlock)
      ifStmt.setSourceRange(startLocation, codeBlock.sourceRange.end) // Note: this line is crafted by Renko 😂
      return ifStmt
    }

    let nestedStartLocation = getStartLocation()
    if _lexer.match(.if),
      let elseIfStmt = try parseIfStatement(startLocation: nestedStartLocation) as? IfStatement
    {
      let ifStmt = IfStatement(
        conditionList: conditionList,
        codeBlock: codeBlock,
        elseClause: .elseif(elseIfStmt))
      ifStmt.setSourceRange(startLocation, elseIfStmt.sourceRange.end)
      return ifStmt
    }

    let elseCodeBlock = try parseCodeBlock()
    let ifStmt = IfStatement(
      conditionList: conditionList,
      codeBlock: codeBlock,
      elseClause: .else(elseCodeBlock))
    ifStmt.setSourceRange(startLocation, elseCodeBlock.sourceRange.end)
    return ifStmt
  }

  open func parseDeclaration() throws -> Declaration { /*
    swift-lint:suppress(high_cyclomatic_complexity)
    */
    let startLocation = getStartLocation()

    let attrs = try parseAttributes()
    let modifiers = parseModifiers()

    let declHeadTokens: [Token.Kind] = [
      .import, .let, .var, .typealias, .func, .enum, .indirect, .struct,
      .init, .deinit, .extension, .subscript, .operator, .protocol
    ]
    switch _lexer.read(declHeadTokens) {
    case .import:
      return try parseImportDeclaration(
        withAttributes: attrs, startLocation: startLocation)
    case .let:
      return try parseConstantDeclaration(
        withAttributes: attrs,
        modifiers: modifiers,
        startLocation: startLocation)
    case .var:
      return try parseVariableDeclaration(
        withAttributes: attrs,
        modifiers: modifiers,
        startLocation: startLocation)
    case .typealias:
      return try parseTypealiasDeclaration(
        withAttributes: attrs,
        modifiers: modifiers,
        startLocation: startLocation)
    case .func:
      return try parseFunctionDeclaration(
        withAttributes: attrs,
        modifiers: modifiers,
        startLocation: startLocation)
    case .enum:
      return try parseEnumDeclaration(
        withAttributes: attrs,
        modifiers: modifiers,
        isIndirect: false,
        startLocation: startLocation)
    case .indirect:
      try match(.enum, orFatal: .enumExpectedAfterIndirect)
      return try parseEnumDeclaration(
        withAttributes: attrs,
        modifiers: modifiers,
        isIndirect: true,
        startLocation: startLocation)
    case .struct:
      return try parseStructDeclaration(
        withAttributes: attrs,
        modifiers: modifiers,
        startLocation: startLocation)
    case .init:
      return try parseInitializerDeclaration(
        withAttributes: attrs,
        modifiers: modifiers,
        startLocation: startLocation)
    case .deinit where modifiers.isEmpty:
      return try parseDeinitializerDeclaration(
        withAttributes: attrs, startLocation: startLocation)
    case .extension:
      return try parseExtensionDeclaration(
        withAttributes: attrs,
        modifiers: modifiers,
        startLocation: startLocation)
    case .subscript:
      return try parseSubscriptDeclaration(
        withAttributes: attrs,
        modifiers: modifiers,
        startLocation: startLocation)
    case .operator where attrs.isEmpty:
      return try parseOperatorDeclaration(
        withModifiers: modifiers, startLocation: startLocation)
    case .protocol:
      return try parseProtocolDeclaration(
        withAttributes: attrs,
        modifiers: modifiers,
        startLocation: startLocation)
    default:
      // try parsing class declaration
      if let lastModifier = modifiers.last, lastModifier == .class {
        let otherModifiers = Array(modifiers.dropLast())
        return try parseClassDeclaration(
          withAttributes: attrs,
          modifiers: otherModifiers,
          startLocation: startLocation)
      }

      // try parsing precedence group declaration
      if attrs.isEmpty,
        modifiers.isEmpty,
        case .identifier(let keyword, false) = _lexer.look().kind,
        keyword == "precedencegroup"
      {
        _lexer.advance()
        return try parsePrecedenceGroupDeclaration(startLocation: startLocation)
      }

      // tried very hard and failed, throw exception
      throw _raiseFatal(.badDeclaration)
    }
  }

  open func parseFunctionDeclaration( // swift-lint:suppress(high_cyclomatic_complexity,high_ncss)
    withAttributes attrs: Attributes,
    modifiers: DeclarationModifiers,
    startLocation: SourceLocation
  ) throws -> FunctionDeclaration {
    func parseName() throws -> Identifier {
      var kind: DeclarationModifier?
      for m in modifiers {
        switch m {
        case .prefix:
          kind == nil ? kind = .prefix : try _raiseError(.duplicatedFunctionModifiers)
        case .postfix:
          kind == nil ? kind = .postfix : try _raiseError(.duplicatedFunctionModifiers)
        case .infix:
          kind == nil ? kind = .infix : try _raiseError(.duplicatedFunctionModifiers)
        default:
          break
        }
      }

      if let op = parseVerifiedOperator(againstModifier: kind) {
        return .name(op)
      }

      guard let name = readNamedIdentifier() else {
        throw _raiseFatal(.missingFunctionName)
      }
      return name
    }

    func parseSignature() throws -> (FunctionSignature, SourceLocation) {
      let (params, paramsSrcRange) = try parseParameterClause()
      let (throwsKind, throwsEndLocation) = parseThrowsKind()
      let result = try parseFunctionResult()

      let funcSign = FunctionSignature(
        parameterList: params, throwsKind: throwsKind, result: result)
      if let resultEndLocation = result?.type.sourceRange.end {
        return (funcSign, resultEndLocation)
      } else if let throwsEndLocation = throwsEndLocation {
        return (funcSign, throwsEndLocation)
      } else {
        return (funcSign, paramsSrcRange.end)
      }
    }

    let name = try parseName()
    let genericParameterClause = try parseGenericParameterClause()
    let (signature, signEndLocation) = try parseSignature()
    let genericWhereClause = try parseGenericWhereClause()
    var body: CodeBlock?
    if _lexer.look().kind == .leftBrace {
      body = try parseCodeBlock()
    }

    var endLocation = signEndLocation
    if let lastGenericReq = genericWhereClause?.requirementList.last {
      switch lastGenericReq {
      case .typeConformance(_, let type):
        endLocation = type.sourceRange.end
      case .protocolConformance(_, let type):
        endLocation = type.sourceRange.end
      case .sameType(_, let type):
        endLocation = type.sourceRange.end
      }
    }
    if let bodyEndLocation = body?.sourceRange.end {
      endLocation = bodyEndLocation
    }

    let funcDecl = FunctionDeclaration(
      attributes: attrs,
      modifiers: modifiers,
      name: name,
      genericParameterClause: genericParameterClause,
      signature: signature,
      genericWhereClause: genericWhereClause,
      body: body)
    funcDecl.setSourceRange(startLocation, endLocation)
    return funcDecl
  }

  open func parseClassDeclaration(
    withAttributes attrs: Attributes,
    modifiers: DeclarationModifiers,
    startLocation: SourceLocation
  ) throws -> ClassDeclaration {
    var accessLevelModifier: AccessLevelModifier?
    var isFinal = false
    if modifiers.count == 1, case .accessLevel(let modifier) = modifiers[0] {
      accessLevelModifier = modifier
    } else if modifiers.count == 1, modifiers[0] == .final {
      isFinal = true
    } else if modifiers.count == 2, modifiers[0] == .final, case .accessLevel(let modifier) = modifiers[1] {
      accessLevelModifier = modifier
      isFinal = true
    } else if modifiers.count == 2, case .accessLevel(let modifier) = modifiers[0], modifiers[1] == .final {
      accessLevelModifier = modifier
      isFinal = true
    }

    guard let name = _lexer.look().kind.structName?.id else {
      throw _raiseFatal(.missingClassName)
    }
    _lexer.advance()

    let genericParameterClause = try parseGenericParameterClause()
    let typeInheritanceClause = try parseTypeInheritanceClause()
    let genericWhereClause = try parseGenericWhereClause()

    try match(.leftBrace, orFatal: .leftBraceExpected("class declaration body"))

    var endLocation = getEndLocation()
    var members: [ClassDeclaration.Member] = []
    while !_lexer.match(.rightBrace) {
      let hashStartLocation = getStartLocation()
      if _lexer.match(.hash) {
        let compCtrlStmt = try parseCompilerControlStatement(startLocation: hashStartLocation)
        members.append(.compilerControl(compCtrlStmt))
      } else {
        let decl = try parseDeclaration()
        members.append(.declaration(decl))
      }
      endLocation = getEndLocation()

      removeTrailingSemicolons()
    }

    let classDecl = ClassDeclaration(
      attributes: attrs,
      accessLevelModifier: accessLevelModifier,
      isFinal: isFinal,
      name: name,
      genericParameterClause: genericParameterClause,
      typeInheritanceClause: typeInheritanceClause,
      genericWhereClause: genericWhereClause,
      members: members)
    classDecl.setSourceRange(startLocation, endLocation)
    return classDecl
  }

  open func parseStructDeclaration(
    withAttributes attrs: Attributes,
    modifiers: DeclarationModifiers,
    startLocation: SourceLocation
  ) throws -> StructDeclaration {
    var accessLevelModifier: AccessLevelModifier?
    if modifiers.count == 1, case .accessLevel(let modifier) = modifiers[0] {
      accessLevelModifier = modifier
    }

    guard let name = _lexer.look().kind.structName?.id else {
      throw _raiseFatal(.missingStructName)
    }
    _lexer.advance()

    let genericParameterClause = try parseGenericParameterClause()
    let typeInheritanceClause = try parseTypeInheritanceClause()
    let genericWhereClause = try parseGenericWhereClause()

    try match(.leftBrace, orFatal: .leftBraceExpected("struct declaration body"))

    var endLocation = getEndLocation()
    var members: [StructDeclaration.Member] = []
    while !_lexer.match(.rightBrace) {
      let hashStartLocation = getStartLocation()
      if _lexer.match(.hash) {
        let compCtrlStmt = try parseCompilerControlStatement(startLocation: hashStartLocation)
        members.append(.compilerControl(compCtrlStmt))
      } else {
        let decl = try parseDeclaration()
        members.append(.declaration(decl))
      }
      endLocation = getEndLocation()

      removeTrailingSemicolons()
    }

    let structDecl = StructDeclaration(
      attributes: attrs,
      accessLevelModifier: accessLevelModifier,
      name: name,
      genericParameterClause: genericParameterClause,
      typeInheritanceClause: typeInheritanceClause,
      genericWhereClause: genericWhereClause,
      members: members)
    structDecl.setSourceRange(startLocation, endLocation)
    return structDecl
  }

  open func parseExpressionList(
    config: ParserExpressionConfig = ParserExpressionConfig()
  ) throws -> ExpressionList {
    var exprs: [Expression] = []
    repeat {
      let expr = try parseExpression(config: config)
      exprs.append(expr)
    } while _lexer.match(.comma)
    return exprs
  }

  open func parseExpression(
    config: ParserExpressionConfig = ParserExpressionConfig()
  ) throws -> Expression {
    let tryKind = parseTryKind()
    let prefixExpr = try parsePrefixExpression(config: config)
    let expr = try parseBinaryExpressions(
      leftExpression: prefixExpr, config: config)
    return tryKind.wrap(expr: expr)
  }

  open func parseListComprehension() throws -> PostfixExpression? {
    return nil
  }

}
