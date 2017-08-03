//
//  OperatorHelper.hpp
//  Emojicode
//
//  Created by Theo Weidmann on 01/08/2017.
//  Copyright © 2017 Theo Weidmann. All rights reserved.
//

#ifndef OperatorHelper_hpp
#define OperatorHelper_hpp

#include "../EmojicodeCompiler.hpp"
#include "../Parsing/ASTNodeType.hpp"

namespace EmojicodeCompiler {

enum class OperatorType {
    MultiplicationOperator,
    DivisionOperator,
    RemainderOperator,
    MinusOperator,
    PlusOperator,
    ShiftLeftOperator,
    ShiftRightOperator,
    GreaterOrEqualOperator,
    GreaterOperator,
    LessOrEqualOperator,
    LessOperator,
    BitwiseAndOperator,
    BitwiseXorOperator,
    BitwiseOrOperator,
    LogicalAndOperator,
    LogicalOrOperator,
    EqualOperator,
    IdentityOperator,
};

int operatorPrecedence(OperatorType);
OperatorType operatorType(const EmojicodeString &);
const int kPrefixPrecedence = 11;

}

#endif /* OperatorHelper_hpp */
