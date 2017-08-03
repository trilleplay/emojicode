//
//  ASTNodeType.hpp
//  EmojicodeCompiler
//
//  Created by Theo Weidmann on 28/07/2017.
//  Copyright © 2017 Theo Weidmann. All rights reserved.
//

#ifndef ASTNodeType_hpp
#define ASTNodeType_hpp

namespace EmojicodeCompiler {
    
enum class ASTNodeType {
    StringLiteral,
    IntegerLiteral,
    DoubleLiteral,
    TrueLiteral,
    FalseLiteral,
    SymbolLiteral,
    ConcatenateLiteral,
    ListLiteral,
    DictionaryLiteral,

    VariableDeclaration,
    FrozenDeclaration,
    VariableDeclarationAssignment,
    VariableAssignment,
    VariableAssignmentInstance,
    Variable,
    VariableInstance,

    ConditionalAssignment,

    This,
    Nothingness,
    IsNothingness,
    IsError,
    IsSameObject,
    MetaTypeInstance,
    MetaTypeInstanceFromInstance,
    Unwrap,
    PerfectExtraction,
    MethodCapture,
    TypeMethodCapture,
    CallableCall,
    SupermethodCall,
    Cast,

    TypeFromExpression,
    StaticType,
    InferType,
    ThisType,

    MethodCall,
    TypeMethodCall,
    Initialization,
    Superinitializer,
    IfStatement,
    ErrorCheckControl,
    RepeatWhile,
    ForIn,

    Return,
    Error,

    Block,
    Expression,
    Scoped,

    BoxToSimpleOptional,
    SimpleToSimpleOptional,
    SimpleOptionalToBox,
    SimpleToBox,
    BoxToSimple,
    StoreTemporarily,
    Dereference,
    EnumIntialization,
    VariableReferenceInstance,
    VariableReference,
    ObjectMethodDispatch,
    ContextedFunctionDispatch,
    ValueTypeTypeMethodCall,
    ProtocolMethodDispatch,
    ValueTypeInitialization,
    EqualOperator,

    Operator,

    IntegerAddOperator,
    IntegerSubtractOperator,
    IntegerMultiplyOperator,
    IntegerDivideOperator,
    IntegerRemainderOperator,
    IntegerAndOperator,
    IntegerOrOperator,
    IntegerXorOperator,
    IntegerNotOperator,
    IntegerShiftLeftOperator,
    IntegerShiftRightOperator,
    IntegerGreaterOrEqualOperator,
    IntegerGreaterOperator,
    IntegerToDoubleConversion,

    DoubleAddOperator,
    DoubleSubtractOperator,
    DoubleMultiplyOperator,
    DoubleDivideOperator,
    DoubleRemainderOperator,
    DoubleGreaterOrEqualOperator,
    DoubleGreaterOperator,
    DoubleEqualOperator,

    BooleanAnd,
    BooleanOr,

    DownCast,
    CastToClass,
    CastToProtocol,
    CastToValueType,
};

#define GENERATOR_ONLY_NODE_TYPES_CASES \
case ASTNodeType::BoxToSimple:\
case ASTNodeType::BoxToSimpleOptional:\
case ASTNodeType::SimpleToBox:\
case ASTNodeType::SimpleToSimpleOptional:\
case ASTNodeType::SimpleOptionalToBox:\
case ASTNodeType::Dereference:\
case ASTNodeType::StoreTemporarily:\
case ASTNodeType::EnumIntialization:\
case ASTNodeType::VariableReference:\
case ASTNodeType::ObjectMethodDispatch:\
case ASTNodeType::ContextedFunctionDispatch:\
case ASTNodeType::ValueTypeTypeMethodCall:\
case ASTNodeType::ProtocolMethodDispatch:\
case ASTNodeType::ValueTypeInitialization:\
case ASTNodeType::VariableDeclarationAssignment:\
case ASTNodeType::EqualOperator:\
case ASTNodeType::IntegerAddOperator:\
case ASTNodeType::IntegerSubtractOperator:\
case ASTNodeType::IntegerMultiplyOperator:\
case ASTNodeType::IntegerDivideOperator:\
case ASTNodeType::IntegerRemainderOperator:\
case ASTNodeType::IntegerAndOperator:\
case ASTNodeType::IntegerOrOperator:\
case ASTNodeType::IntegerXorOperator:\
case ASTNodeType::IntegerNotOperator:\
case ASTNodeType::IntegerShiftLeftOperator:\
case ASTNodeType::IntegerShiftRightOperator:\
case ASTNodeType::IntegerGreaterOrEqualOperator:\
case ASTNodeType::IntegerGreaterOperator:\
case ASTNodeType::IntegerToDoubleConversion:\
case ASTNodeType::DoubleAddOperator:\
case ASTNodeType::DoubleSubtractOperator:\
case ASTNodeType::DoubleMultiplyOperator:\
case ASTNodeType::DoubleDivideOperator:\
case ASTNodeType::DoubleRemainderOperator:\
case ASTNodeType::DoubleGreaterOrEqualOperator:\
case ASTNodeType::DoubleGreaterOperator:\
case ASTNodeType::DoubleEqualOperator:\
case ASTNodeType::BooleanAnd:\
case ASTNodeType::BooleanOr:\
case ASTNodeType::VariableReferenceInstance:\
case ASTNodeType::VariableInstance:\
case ASTNodeType::Scoped:\
case ASTNodeType::DownCast:\
case ASTNodeType::CastToClass:\
case ASTNodeType::CastToProtocol:\
case ASTNodeType::CastToValueType:\

}  // namespace Emojicode

#endif /* ASTNodeType_hpp */
