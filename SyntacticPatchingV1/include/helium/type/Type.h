#ifndef TYPE_H
#define TYPE_H

#include "helium/utils/XMLNode.h"
#include "helium/type/IOHelper.h"

class Type;
class TypeFactory {
public:
  static Type *CreateType(std::string str);
};

class Type {
public:
  Type() {}
  virtual ~Type() {}
  virtual std::string GetInputCode(std::string var) = 0;
  virtual std::string GetOutputCode(std::string var) = 0;
};

class PrimitiveType : public Type {
public:
  PrimitiveType() {}
  virtual ~PrimitiveType() {}
protected:
private:
};

class SequentialType : public Type {
public:
  SequentialType() {}
  virtual ~SequentialType() {}
protected:
private:
};

class ArrayType : public SequentialType {
public:
  ArrayType(std::string type_str, int num) {
    Num = num;
    InnerType = TypeFactory::CreateType(type_str);
  }
  ~ArrayType() {
  }
  virtual std::string GetInputCode(std::string var) override;
  virtual std::string GetOutputCode(std::string var) override;
private:
  Type *InnerType = NULL;
  int Num = 0; // number
};

class PointerType : public SequentialType {
public:
  PointerType(std::string type_str) {
    InnerType = TypeFactory::CreateType(type_str);
  }
  virtual ~PointerType() {
    if (InnerType) delete InnerType;
  }
  virtual std::string GetInputCode(std::string var) override;
  virtual std::string GetOutputCode(std::string var) override;
private:
  Type *InnerType = nullptr;
};



class IntType : public PrimitiveType {
public:
  IntType() {}
  ~IntType() {}
  virtual std::string GetInputCode(std::string var) override {
    return IOHelper::GetInputCallWithName("int", var, var);
  }
  virtual std::string GetOutputCode(std::string var) override {
    return IOHelper::GetOutputCall("int", var, var);
  }
};

class CharType : public PrimitiveType {
public:
  CharType() {}
  virtual ~CharType() {}
  virtual std::string GetInputCode(std::string var) override {
    return IOHelper::GetInputCallWithName("char", var, var);
  }
  virtual std::string GetOutputCode(std::string var) override {
    return IOHelper::GetOutputCall("char", var, var);
  }
};

class BoolType : public PrimitiveType {
public:
  BoolType() {}
  virtual ~BoolType() {}
  virtual std::string GetInputCode(std::string var) override {
    return IOHelper::GetInputCallWithName("bool", var, var);
  }
  virtual std::string GetOutputCode(std::string var) override {
    return IOHelper::GetOutputCall("bool", var, var);
  }
};



#endif /* TYPE_H */
