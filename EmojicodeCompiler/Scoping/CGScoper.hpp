//
//  CGScoper.hpp
//  Emojicode
//
//  Created by Theo Weidmann on 02/08/2017.
//  Copyright © 2017 Theo Weidmann. All rights reserved.
//

#ifndef CGScoper_hpp
#define CGScoper_hpp

#include "../Types/Type.hpp"
#include "../FunctionVariableObjectInformation.hpp"
#include <vector>
#include <cassert>

namespace EmojicodeCompiler {

class CGScoper {
public:
    struct Variable {
        Type type = Type::nothingness();
        unsigned int stackIndex;
        unsigned int initialized = 0;
        InstructionCount initPosition;

        void initialize(InstructionCount count) {
            if (initialized == 0) {
                initialized = 1;
                initPosition = count + 1; // TODO: ???
            }
        }
    };

    CGScoper(size_t variables) : variables_(variables, Variable()) {}

    void resizeVariables(size_t to) {
        variables_.resize(to, Variable());
    }

    void pushScope() {
        scopes_.emplace_back(scopes_.empty() ? 0 : scopes_.back().maxIndex);
        for (auto &var : variables_) {
            if (var.initialized > 0) {
                var.initialized++;
            }
        }
    }
    void popScope(InstructionCount count) {
        auto &scope = scopes_.back();
        reduceOffsetBy(scope.size);
        for (auto it = variables_.begin() ; it < variables_.begin(); it++) {
            if (it->initialized == 1) {
                it->type.objectVariableRecords(it->stackIndex, &fovInfo_, it->initPosition, count);
            }
            it->initialized--;
        }
        scopes_.pop_back();
    }
    Variable& getVariable(unsigned int var) {
        return variables_[var];
    }
    Variable& declareVariable(unsigned int varIndex, const Type &declarationType) {
        auto &var = variables_[varIndex];
        auto &scope = scopes_.back();
        assert(varIndex >= scope.minIndex);
        scope.updateMax(varIndex);
        auto typeSize = declarationType.size();
        scope.size += typeSize;
        var.type = declarationType;
        var.stackIndex = reserveVariable(typeSize);
        return var;
    }

    unsigned int size() const { return size_; };
private:
    struct Scope {
        Scope (size_t minIndex) : minIndex(minIndex), maxIndex(minIndex) {}

        void updateMax(unsigned int x) { if (x > maxIndex) maxIndex = x; }

        size_t minIndex;
        size_t maxIndex;
        /// The size of the variables in the scope in Emojicode words
        unsigned int size = 0;
    };

    unsigned int reserveVariable(unsigned int size) {
        auto id = nextOffset_;
        nextOffset_ += size;
        if (nextOffset_ > size_) {
            size_ = nextOffset_;
        }
        return id;
    }

    void reduceOffsetBy(unsigned int size) {
        nextOffset_ -= size;
    }

    std::vector<Variable> variables_;
    std::vector<Scope> scopes_;
    std::vector<FunctionObjectVariableInformation> fovInfo_;

    unsigned int nextOffset_ = 0;
    unsigned int size_ = 0;
};

}

#endif /* CGScoper_hpp */
