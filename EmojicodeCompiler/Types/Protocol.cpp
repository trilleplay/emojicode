//
//  Protocol.cpp
//  Emojicode
//
//  Created by Theo Weidmann on 27/04/16.
//  Copyright © 2016 Theo Weidmann. All rights reserved.
//

#include "Protocol.hpp"
#include "../Function.hpp"

namespace EmojicodeCompiler {

uint_fast16_t Protocol::nextIndex = 0;

Protocol::Protocol(EmojicodeString name, Package *pkg, const SourcePosition &p, const EmojicodeString &string)
    : TypeDefinition(name, pkg, p, string) {
    if (nextIndex == UINT16_MAX) {
        throw CompilerError(p, "You exceeded the limit of 65,536 protocols.");
    }
    index = nextIndex++;
}

void Protocol::addMethod(Function *method) {
    method->setLinkingTableIndex(1);
    method->setVti(static_cast<int>(methodList_.size()));
    TypeDefinition::addMethod(method);
}

}  // namespace EmojicodeCompiler
