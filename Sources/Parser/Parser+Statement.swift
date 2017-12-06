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

import Source
import AST
import Lexer

extension Parser {
  func parseThrowStatement(
    startLocation: SourceLocation
  ) throws -> ThrowStatement {
    let expr = try parseExpression()
    let throwStmt = ThrowStatement(expression: expr)
    throwStmt.setSourceRange(startLocation, expr.sourceRange.end)
    return throwStmt
  }

  func parseReturnStatement(startRange: SourceRange) throws -> ReturnStatement {
    func returnStmt(willParseExpression: Bool = false) throws -> ReturnStatement {
      let expr = willParseExpression ? try parseExpression() : nil
      let endLocation = expr?.sourceRange.end ?? startRange.end
      let retStmt = ReturnStatement(expression: expr)
      retStmt.setSourceRange(startRange.start, endLocation)
      return retStmt
    }

    switch _lexer.look(skipLineFeed: false).kind {
    case .semicolon, .eof, .rightBrace:
      return try returnStmt()
    case .lineFeed:
      // NOTE: without knowing whether it is inside a Void function, we will
      // agressively consume the next token as an expression if possible, and
      // let semantic analysis later decides whether keep it this way or split
      // into two statements (a return statement, and an expression).
      // So here, if next line is indented and can be parsed as an expression,
      // then treat it as part of the return statement;
      // otherwise, then directly return
      let nextToken = _lexer.look(ahead: 1, skipLineFeed: false)
      if nextToken.sourceRange.start.column > startRange.start.column {
        switch nextToken.kind {
        case .for, .while, .repeat, .if, .guard, .switch, .defer, .do, .break,
          .continue, .fallthrough, .return, .throw, .hash, .import, .let, .var,
          .typealias, .func, .enum, .indirect, .struct, .init, .deinit,
          .extension, .subscript, .operator, .protocol, .at:
          return try returnStmt()
        default:
          return nextToken.kind.isModifier ?
            try returnStmt() : try returnStmt(willParseExpression: true)
        }
      }
      return try returnStmt()
    default:
      return try returnStmt(willParseExpression: true)
    }
  }

  func parseDeferStatement(
    startLocation: SourceLocation
  ) throws -> DeferStatement {
    let codeBlock = try parseCodeBlock()
    let deferStmt = DeferStatement(codeBlock: codeBlock)
    deferStmt.setSourceRange(startLocation, codeBlock.sourceRange.end)
    return deferStmt
  }

  func parseContinueStatement(startRange: SourceRange) -> ContinueStatement {
    let endLocation = getEndLocation()
    if !_lexer.lookLineFeed(),
      case .identifier(let name) = _lexer.read(.dummyIdentifier)
    {
      let continueStmt = ContinueStatement(labelName: name)
      continueStmt.setSourceRange(startRange.start, endLocation)
      return continueStmt
    } else {
      let continueStmt = ContinueStatement()
      continueStmt.setSourceRange(startRange)
      return continueStmt
    }
  }

  func parseBreakStatement(startRange: SourceRange) -> BreakStatement {
    let endLocation = getEndLocation()
    if !_lexer.lookLineFeed(),
      case .identifier(let name) = _lexer.read(.dummyIdentifier)
    {
      let breakStmt = BreakStatement(labelName: name)
      breakStmt.setSourceRange(startRange.start, endLocation)
      return breakStmt
    } else {
      let breakStmt = BreakStatement()
      breakStmt.setSourceRange(startRange)
      return breakStmt
    }
  }

  func parseCompilerControlStatement( // swift-lint:suppress(high_cyclomatic_complexity)
    startLocation: SourceLocation
  ) throws -> CompilerControlStatement {
    var kind: CompilerControlStatement.Kind
    var endLocation = getEndLocation()
    switch _lexer.read([.if, .dummyIdentifier, .else]) {
    case .if:
      let condition = _lexer.readUntilEOL()
      kind = .if(condition)
      for _ in 0..<condition.count {
        endLocation = endLocation.nextColumn
      }
    case .identifier(let id):
      switch id {
      case "elseif":
        let condition = _lexer.readUntilEOL()
        kind = .elseif(condition)
        for _ in 0..<condition.count {
          endLocation = endLocation.nextColumn
        }
      case "endif":
        kind = .endif
      case "sourceLocation":
        guard _lexer.match(.leftParen) else {
          throw _raiseFatal(.expectedOpenParenSourceLocation)
        }
        if _lexer.match(.rightParen) {
          _lexer.readUntilEOL()
          kind = .sourceLocation(nil, nil)
        }
        var fileName: String?
        var lineNumber: Int?
        if _lexer.read(.dummyIdentifier) == .identifier("file"),
          _lexer.match(.colon),
          case let .staticStringLiteral(name, _) = _lexer.read(.dummyStaticStringLiteral),
          _lexer.match(.comma),
          _lexer.read(.dummyIdentifier) == .identifier("line"),
          _lexer.match(.colon),
          case let .integerLiteral(line, raw) = _lexer.read(.dummyIntegerLiteral),
          raw.containOnlyPositiveDecimals,
          _lexer.match(.rightParen) // TODO: very crazy conditions
        {
          fileName = name
          lineNumber = Int(line)
        }
        _lexer.readUntilEOL()
        kind = .sourceLocation(fileName, lineNumber)
      default:
        throw _raiseFatal(.expectedValidCompilerCtrlKeyword)
      }
    case .else:
      kind = .else
    default:
      throw _raiseFatal(.expectedValidCompilerCtrlKeyword)
    }
    let ctrlStmt = CompilerControlStatement(kind: kind)
    ctrlStmt.setSourceRange(startLocation, endLocation)
    return ctrlStmt
  }

  func parseLabeledStatement(
    withLabelName name: String, startLocation: SourceLocation
  ) throws -> LabeledStatement {
    let stmt: Statement
    let stmtStartLocation = getStartLocation()
    switch _lexer.read([.for, .while, .repeat, .if, .switch, .do]) {
    case .for:
      stmt = try parseForInStatement(startLocation: stmtStartLocation)
    case .while:
      stmt = try parseWhileStatement(startLocation: stmtStartLocation)
    case .repeat:
      stmt = try parseRepeatWhileStatement(startLocation: stmtStartLocation)
    case .if:
      stmt = try parseIfStatement(startLocation: stmtStartLocation)
    case .switch:
      stmt = try parseSwitchStatement(startLocation: stmtStartLocation)
    case .do:
      stmt = try parseDoStatement(startLocation: stmtStartLocation)
    default:
      throw _raiseFatal(.invalidLabelOnStatement)
    }
    let labeledStmt = LabeledStatement(labelName: name, statement: stmt)
    labeledStmt.setSourceRange(startLocation, stmt.sourceRange.end)
    return labeledStmt
  }

  func parseDoStatement(
    startLocation: SourceLocation
  ) throws -> DoStatement {
    let codeBlock = try parseCodeBlock()
    var endLocation = codeBlock.sourceRange.end

    var catchClauses: [DoStatement.CatchClause] = []
    while _lexer.match(.catch) {
      var catchPattern: Pattern?
      var catchWhere: Expression?
      if _lexer.look().kind != .leftBrace {
        if _lexer.look().kind != .where {
          catchPattern = try parsePattern()
        }
        if _lexer.match(.where) {
          catchWhere = try parseExpression(config: noTrailingConfig)
        }
      }

      let catchCodeBlock = try parseCodeBlock()
      endLocation = catchCodeBlock.sourceRange.end

      let catchClause = DoStatement.CatchClause(
        pattern: catchPattern,
        whereExpression: catchWhere,
        codeBlock: catchCodeBlock)
      catchClauses.append(catchClause)
    }

    let doStmt = DoStatement(codeBlock: codeBlock, catchClauses: catchClauses)
    doStmt.setSourceRange(startLocation, endLocation)
    return doStmt
  }

  func parseSwitchStatement(
    startLocation: SourceLocation
  ) throws -> SwitchStatement {
    let expr = try parseExpression(config: noTrailingConfig)
    guard _lexer.match(.leftBrace) else {
      throw _raiseFatal(.leftBraceExpected("switch statement"))
    }
    var cases: [SwitchStatement.Case] = []
    var examined = _lexer.examine([.case, .default])
    while examined.0 {
      switch examined.1 {
      case .case:
        var itemList: [SwitchStatement.Case.Item] = []
        repeat {
          let pattern = try parsePattern(config: forPatternMatchingConfig)
          var whereExpr: Expression?
          if _lexer.match(.where) {
            whereExpr = try parseExpression(config: noTrailingConfig)
          }
          let item = SwitchStatement.Case.Item(
            pattern: pattern, whereExpression: whereExpr)
          itemList.append(item)
        } while _lexer.match(.comma)
        guard _lexer.match(.colon) else {
          throw _raiseFatal(.expectedCaseColon)
        }
        let stmts = try parseStatements()
        guard !stmts.isEmpty else {
          throw _raiseFatal(.caseStmtWithoutBody("case"))
        }
        cases.append(.case(itemList, stmts))
      case .default:
        guard _lexer.match(.colon) else {
          throw _raiseFatal(.expectedDefaultColon)
        }
        let stmts = try parseStatements()
        guard !stmts.isEmpty else {
          throw _raiseFatal(.caseStmtWithoutBody("default"))
        }
        cases.append(.default(stmts))
      default:
        break
      }
      examined = _lexer.examine([.case, .default])
    }
    let endLocation = getEndLocation()
    guard _lexer.match(.rightBrace) else {
      throw _raiseFatal(.rightBraceExpected("switch statement"))
    }
    let switchStmt = SwitchStatement(expression: expr, cases: cases)
    switchStmt.setSourceRange(startLocation, endLocation)
    return switchStmt
  }

  func parseGuardStatement(
    startLocation: SourceLocation
  ) throws -> GuardStatement {
    let conditionList = try parseConditionList()
    guard _lexer.match(.else) else {
      throw _raiseFatal(.expectedElseAfterGuard)
    }
    let codeBlock = try parseCodeBlock()
    let guardStmt =
      GuardStatement(conditionList: conditionList, codeBlock: codeBlock)
    guardStmt.setSourceRange(startLocation, codeBlock.sourceRange.end)
    return guardStmt
  }

  func parseRepeatWhileStatement(
    startLocation: SourceLocation
  ) throws -> RepeatWhileStatement {
    let codeBlock = try parseCodeBlock()
    guard _lexer.match(.while) else {
      throw _raiseFatal(.expectedWhileAfterRepeatBody)
    }
    let expr = try parseExpression()
    let repeatStmt = RepeatWhileStatement(
      conditionExpression: expr, codeBlock: codeBlock)
    repeatStmt.setSourceRange(startLocation, expr.sourceRange.end)
    return repeatStmt
  }

  func parseWhileStatement(
    startLocation: SourceLocation
  ) throws -> WhileStatement {
    let conditionList = try parseConditionList()
    let codeBlock = try parseCodeBlock()
    let whileStmt =
      WhileStatement(conditionList: conditionList, codeBlock: codeBlock)
    whileStmt.setSourceRange(startLocation, codeBlock.sourceRange.end)
    return whileStmt
  }

  public func parseConditionList() throws -> ConditionList {
    var conditionList: ConditionList = []
    repeat {
      let condition = try parseCondition()
      conditionList.append(condition)
    } while _lexer.match(.comma)
    return conditionList
  }

  func parseCondition() throws -> Condition {
    switch _lexer.read([.let, .var, .case, .hash]) {
    case .let:
      let cond = try parseCaseCondition()
      return .let(cond.pattern, cond.expression)
    case .var:
      let cond = try parseCaseCondition()
      return .var(cond.pattern, cond.expression)
    case .case:
      let cond = try parseCaseCondition(config: forPatternMatchingConfig)
      return .case(cond.pattern, cond.expression)
    case .hash:
      return try parseAvailabilityCondition()
    default:
      let expr = try parseExpression(config: noTrailingConfig)
      return .expression(expr)
    }
  }

  func parseCaseCondition(
    config: ParserPatternConfig = ParserPatternConfig()
  ) throws -> (pattern: Pattern, expression: Expression) {
    var mutableConfig = config
    mutableConfig.parseTrailingClosure = false
    let pattern = try parsePattern(config: mutableConfig)
    if config.forPatternMatching,
      let exprPattern = pattern as? ExpressionPattern,
      let assignOpExpr = exprPattern.expression as? AssignmentOperatorExpression
    {
      let lhsPattern = ExpressionPattern(
        expression: assignOpExpr.leftExpression)
      let rhsExpr = assignOpExpr.rightExpression
      return (lhsPattern, rhsExpr)
    }
    guard _lexer.match(.assignmentOperator) else {
      throw _raiseFatal(.expectedEqualInConditionalBinding)
    }
    let expr = try parseExpression(config: noTrailingConfig)
    return (pattern, expr)
  }

  func parseAvailabilityCondition() throws -> Condition {
    guard case .identifier("available") = _lexer.look().kind else {
      throw _raiseFatal(.expectedAvailableKeyword)
    }
    _lexer.advance()
    guard _lexer.match(.leftParen) else {
      throw _raiseFatal(.expectedOpenParenAvailabilityCondition)
    }
    let supportedPlatforms = [
      "iOS", "iOSApplicationExtension",
      "macOS", "macOSApplicationExtension",
      "OSX", // TODO: remove this line at a later time
      "watchOS",
      "tvOS",
    ]
    var arguments: [AvailabilityCondition.Argument] = []
    repeat {
      switch _lexer.read([.dummyIdentifier, .dummyBinaryOperator]) {
      case .binaryOperator("*"):
        arguments.append(.all)
      case .identifier(let platformName)
        where supportedPlatforms.contains(platformName):
        // TODO: this entire switch stmt is very ugly, and the logic is poorly handled, need to rewrite
        switch _lexer.read([
          .dummyIntegerLiteral,
          .dummyFloatingPointLiteral,
        ]) {
        case let .integerLiteral(major, raw) where raw.containOnlyPositiveDecimals:
          arguments.append(.major(platformName, Int(major)))
        case .floatingPointLiteral(_, let raw):
          guard let (major, minor) = splitDoubleRawToTwoIntegers(raw) else {
            throw _raiseFatal(.expectedMinorVersionAvailability)
          }
          if _lexer.match(.dot),
            case let .integerLiteral(patch, raw) =
              _lexer.read(.dummyIntegerLiteral),
            raw.containOnlyPositiveDecimals
          {
            arguments.append(.patch(platformName, major, minor, Int(patch)))
          } else {
            arguments.append(.minor(platformName, major, minor))
          }
        default:
          throw _raiseFatal(.expectedAvailabilityVersionNumber)
        }
      default:
        throw _raiseFatal(.attributeAvailabilityPlatform)
      }
    } while _lexer.match(.comma)
    guard _lexer.match(.rightParen) else {
      throw _raiseFatal(.expectedCloseParenAvailabilityCondition)
    }
    return .availability(AvailabilityCondition(arguments: arguments))
  }

  func parseForInStatement(
    startLocation: SourceLocation
  ) throws -> ForInStatement {
    let isCaseMatching = _lexer.match(.case)
    let matchingPattern = try parsePattern()
    if !_lexer.match(.in) {
      throw _raiseFatal(.expectedForEachIn)
    }
    let collectionExpr = try parseExpression(config: noTrailingConfig)
    var whereClause: Expression?
    if _lexer.match(.where) {
      whereClause = try parseExpression(config: noTrailingConfig)
    }
    let codeBlock = try parseCodeBlock()
    let forStmt = ForInStatement(
      isCaseMatching: isCaseMatching,
      matchingPattern: matchingPattern,
      collection: collectionExpr,
      whereClause: whereClause,
      codeBlock: codeBlock)
    forStmt.setSourceRange(startLocation, codeBlock.sourceRange.end)
    return forStmt
  }

  // common used configurations
  private var noTrailingConfig: ParserExpressionConfig {
    return ParserExpressionConfig(parseTrailingClosure: false)
  }
  private var forPatternMatchingConfig: ParserPatternConfig {
    return ParserPatternConfig(forPatternMatching: true)
  }
}
