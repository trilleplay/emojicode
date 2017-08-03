//
//  Variable.hpp
//  Emojicode
//
//  Created by Theo Weidmann on 02/05/16.
//  Copyright © 2016 Theo Weidmann. All rights reserved.
//

#ifndef Variable_hpp
#define Variable_hpp

#include "../Types/Type.hpp"
#include "../Lex/SourcePosition.hpp"

namespace EmojicodeCompiler {

class CapturingCallableScoper;

class Variable {
    friend CapturingCallableScoper;
public:
    Variable(Type type, unsigned int id, bool frozen, const EmojicodeString &string, SourcePosition p)
        : type_(type), frozen_(frozen), string_(string), id_(id), position_(p) {}
    /// The type of the variable.
    const Type type() const { return type_; }

    /// The name of this variable.
    const EmojicodeString& name() const { return string_; }

    /// The position at which this variable was defined
    const SourcePosition& position() const { return position_; }

    /// Throws an error if the variable is not initalized.
    /// @throws CompilerError
    void uninitalizedError(const SourcePosition &p) const;

    /// Marks the variable as mutated or issues an error if the variable is frozen.
    /// @throws CompilerError if the variable is frozen.
    void mutate(const SourcePosition &p);

    /// Whether the variable was mutated since its definition.
    bool mutated() const { return mutated_; }

    /// Whether this is a frozen variable.
    bool frozen() const { return frozen_; }

    bool inherited() const { return inherited_; }
    void setInherited() { inherited_ = true; }

    void initialize() {
        if (!initialized()) {
            initialized_ = 1;
        }
    }
    void initializeAbsolutely() {
        if (!initialized()) {
            initialized_ = 1;
        }
    }
    void uninitialize() { initialized_ = 0; }
    void popInitializationLevel() { if (initialized()) initialized_--; }
    void pushInitializationLevel() { if (initialized()) initialized_++; }

    /// Whether the variable is initialized.
    bool initialized() const { return initialized_ > 0; }
    int initializationLevel() const { return initialized_; }

    unsigned int id() const { return id_; }
private:
    Type type_;
    bool frozen_;
    bool mutated_ = false;
    bool inherited_ = false;
    EmojicodeString string_;
    int initialized_ = 0;
    unsigned int id_;

    SourcePosition position_;
};

}  // namespace EmojicodeCompiler

#endif /* Variable_hpp */
