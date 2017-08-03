//
//  FnCodeGenerator.hpp
//  Emojicode
//
//  Created by Theo Weidmann on 29/07/2017.
//  Copyright © 2017 Theo Weidmann. All rights reserved.
//

#ifndef FnCodeGenerator_hpp
#define FnCodeGenerator_hpp

#include "../../EmojicodeInstructions.h"
#include "FunctionWriter.hpp"
#include "../Function.hpp"
#include "../Parsing/ASTNode.hpp"
#include "../Scoping/CGScoper.hpp"
#include <memory>

namespace EmojicodeCompiler {

class FnCodeGenerator {
public:
    FnCodeGenerator(Function *function)
    : fn_(function), scoper_(function->variableCount()),
    instanceScoper_(function->owningType().type() != TypeContent::Nothingness ?
                    &function->owningType().typeDefinition()->cgScoper() : nullptr) {}
    void generate();
private:
    Function *fn_;
    CGScoper scoper_;
    CGScoper *instanceScoper_;

    FunctionWriter& wr() { return fn_->writer_; }

    void generateBlock(const std::shared_ptr<ASTNode> &);
    void generateNode(const std::shared_ptr<ASTNode> &node);
    EmojicodeInstruction generateArguments(const std::shared_ptr<ASTNode> &node, size_t offset);

    void writeInstructionForStackOrInstance(bool inInstanceScope, EmojicodeInstruction stack,
                                            EmojicodeInstruction object, EmojicodeInstruction vt) {
        if (!inInstanceScope) {
            wr().writeInstruction(stack);
        }
        else {
            writeValueTypeOrObject(object, vt);
        }
    }

    void writeValueTypeOrObject(EmojicodeInstruction object, EmojicodeInstruction vt) {
        wr().writeInstruction(fn_->contextType() == ContextType::ValueReference ? vt : object);
    }

    void copyToVariable(unsigned int index, bool inInstanceScope, const Type &type) {
        if (type.size() == 1) {
            writeInstructionForStackOrInstance(inInstanceScope, INS_COPY_TO_STACK, INS_COPY_TO_INSTANCE_VARIABLE,
                                               INS_COPY_VT_VARIABLE);
            wr().writeInstruction(index);
        }
        else {
            writeInstructionForStackOrInstance(inInstanceScope, INS_COPY_TO_STACK_SIZE,
                                               INS_COPY_TO_INSTANCE_VARIABLE_SIZE, INS_COPY_VT_VARIABLE_SIZE);
            wr().writeInstruction(type.size());
            wr().writeInstruction(index);
            
        }
    }

    void generateBinaryOperator(Instructions instruction, const std::shared_ptr<ASTNode> &node) {
        generateNode(node->nodes().front());
        generateNode(node->nodes().back());
        wr().writeInstruction(instruction);
    }

    void generateUnaryOperator(Instructions instruction, const std::shared_ptr<ASTNode> &node) {
        generateNode(node->nodes().front());
        wr().writeInstruction(instruction);
    }

    void writeInteger(long long value);
};

}  // namespace Emojicode

#endif /* FnCodeGenerator_hpp */
