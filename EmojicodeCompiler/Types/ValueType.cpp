//
//  ValueType.cpp
//  Emojicode
//
//  Created by Theo Weidmann on 12/06/16.
//  Copyright © 2016 Theo Weidmann. All rights reserved.
//

#include "ValueType.hpp"
#include "../Function.hpp"

namespace EmojicodeCompiler {

std::vector<ValueType *> ValueType::valueTypes_;
std::vector<std::vector<ObjectVariableInformation>> ValueType::boxObjectVariableInformation_(3);

void ValueType::prepareForSemanticAnalysis() {
    TypeDefinition::prepareForSemanticAnalysis();
    if (primitive_ && !instanceVariables().empty()) {
        throw CompilerError(position(), "A value type marked with ⚪️ cannot have instance variables.");
    }
}

void ValueType::prepareForCG() {
    for (auto f : methodList()) {
        f->setVtiProvider(&vtiProvider_);
    }
    for (auto f : typeMethodList()) {
        f->setVtiProvider(&vtiProvider_);
    }
    for (auto f : initializerList()) {
        f->setVtiProvider(&vtiProvider_);
    }

    TypeDefinition::finalizeProtocols(Type(this, false), &vtiProvider_);
}

}  // namespace EmojicodeCompiler
